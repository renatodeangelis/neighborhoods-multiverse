# R/01_load_psid_raw.R
# Read PSID cross-year individual extract (Job ID 362100) from fixed-width ASCII.
#
# Extract metadata (from J362100.do header):
#   Domain:            IND (cross-year individual file — all persons, 1968–2023)
#   N observations:    85,536 (every person ever in PSID)
#   N variables:       1,441
#   Max record length: 2,978 characters
#   Generated:         2026-05-23
#
# Output: data/psid_wide.rds  (85,536 × 1,442: one row per person, wide format)
#
# Run time: roughly 30–90 seconds depending on disk speed. Save the .rds so
# downstream scripts skip this step.

library(tidyverse)
library(here)

# ── Paths ────────────────────────────────────────────────────────────────────
# Data lives in Dropbox, not in this repo. Adjust PSID_DIR as needed.
PSID_DIR  <- "~/Downloads/J362100"
DATA_FILE <- file.path(PSID_DIR, "J362100.txt")
DO_FILE   <- file.path(PSID_DIR, "J362100.do")
OUT_FILE  <- here("data", "psid_wide.rds")

# ── Step 1: Parse column positions from J362100.do ───────────────────────────
#
# The .do file uses Stata's `infix` command. The block between "infix" and
# "using" maps variable names to fixed-width column ranges:
#
#   [long] VARNAME   start - end   [/* comment */]
#
# Multiple definitions appear per line, separated by whitespace.
# The "long" prefix signals a wider Stata storage type; the column positions
# are the same, so we strip it and parse normally.

raw_do  <- read_lines(DO_FILE)

# Locate the infix block (line indices are 1-based in R)
infix_s <- which(str_detect(raw_do, "^infix\\s*$"))[1] + 1
infix_e <- which(str_detect(raw_do, "^\\s*using\\s"))[1] - 1
stopifnot(
  "Could not locate 'infix' in .do file" = !is.na(infix_s),
  "Could not locate 'using' line in .do file" = !is.na(infix_e)
)

# Collapse all infix lines into one string, strip "long" prefixes, then use
# str_match_all to capture every VARNAME start-end triplet. The regex anchors
# on a letter-start to avoid matching the column numbers themselves.
infix_text <- raw_do[infix_s:infix_e] |>
  str_remove_all("\\blong\\s+") |>
  paste(collapse = " ")

col_map <- str_match_all(
  infix_text,
  "([A-Za-z][A-Za-z0-9]+)\\s+(\\d+)\\s*-\\s*(\\d+)"
)[[1]] |>
  as_tibble(.name_repair = "minimal") |>
  set_names(c("full_match", "var", "start", "end")) |>
  select(var, start, end) |>
  mutate(across(c(start, end), as.integer))

message("Column map: ", nrow(col_map), " variables parsed")
# Expected: 1,441. If fewer, check that the regex found the infix block.

# ── Step 2: Build variable labels from the .do file ─────────────────────────
#
# The second half of the .do file has `label variable VARNAME "Label"` for
# every variable. We extract these to keep as a lookup table — they serve as
# the codebook since J362100_codebook.pdf requires poppler to read.

label_map <- raw_do |>
  str_match('label variable\\s+(\\S+)\\s+"([^"]+)"') |>
  as_tibble(.name_repair = "minimal") |>
  set_names(c("full_match", "var", "label")) |>
  filter(!is.na(var)) |>
  select(var, label)

# Merge labels onto position map for easy reference
col_map <- col_map |>
  left_join(label_map, by = "var")

message("Labels matched: ", sum(!is.na(col_map$label)), " / ", nrow(col_map))

# Save the column map separately — useful for variable lookups in later scripts
dir.create(here("data"), showWarnings = FALSE, recursive = TRUE)
saveRDS(col_map, here("data", "psid_col_map.rds"))

# ── Step 3: Read the fixed-width ASCII file ──────────────────────────────────
#
# read_fwf() is the tidyverse reader for fixed-width files.
# We read all columns as integer because:
#   (a) PSID encodes missing as large integers (9, 99, 9999, etc.) — reading
#       as double would add unnecessary float imprecision.
#   (b) All income/wage variables use round dollar values in the raw file.
# Convert to double downstream if needed for arithmetic.

message("Reading ", DATA_FILE, " ...")
psid_wide <- read_fwf(
  file          = DATA_FILE,
  col_positions = fwf_positions(
    start     = col_map$start,
    end       = col_map$end,
    col_names = col_map$var
  ),
  col_types = cols(.default = col_integer()),
  progress  = TRUE
)
message("Loaded: ", nrow(psid_wide), " rows × ", ncol(psid_wide), " columns")

# ── Step 4: Construct the unique person identifier ───────────────────────────
#
# The PSID person ID is ER30001 (1968 Interview Number) × 1000 + ER30002
# (1968 Person Number). This combination is permanent — it never changes across
# waves, even if the person moves households, marries, or is recontacted after
# dropping out. ER30001 ranges 1–9308; ER30002 ranges 1–20, so multiplying by
# 1000 gives a unique 7-digit integer for each person.

psid_wide = psid_wide |>
  mutate(pid = ER30001 * 1000L + ER30002, .before = 1)

# ── Step 5: Save ─────────────────────────────────────────────────────────────
saveRDS(psid_wide, OUT_FILE)
message("Saved wide file to: ", OUT_FILE)
message("Dimensions: ", nrow(psid_wide), " × ", ncol(psid_wide))
