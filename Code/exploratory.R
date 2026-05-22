# Exploratory Analysis — PSID
# Project: Neighborhood Effects Multiverse
# ---------------------------------------------------------------------------

library(tidyverse)
library(here)

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

dofile_path = "/Users/renatodeangelis/Downloads/J362045/J362045.do"
data_path   = "/Users/renatodeangelis/Downloads/J362045/J362045.txt"

# ---------------------------------------------------------------------------
# Parse the do-file
# Extracts variable names, column start/end positions, and labels
# ---------------------------------------------------------------------------

parse_dofile = function(dofile_path) {

  txt  = readLines(dofile_path)
  full = paste(txt, collapse = "\n")

  # Extract the infix block (between 'infix' and 'using')
  infix_block = regmatches(full, regexpr("(?s)infix\\s+(.+?)using\\s+", full, perl = TRUE))
  infix_block = sub("^infix\\s+", "", infix_block)
  infix_block = sub("using\\s+$", "", infix_block)

  # Parse variable name and column positions (ignore 'long' prefix)
  matches = gregexpr(
    "(?:long\\s+)?(\\w+)\\s+(\\d+)\\s*-\\s*(\\d+)",
    infix_block, perl = TRUE
  )
  raw = regmatches(infix_block, matches)[[1]]

  parsed = lapply(raw, function(x) {
    parts = regmatches(x, regexec(
      "(?:long\\s+)?(\\w+)\\s+(\\d+)\\s*-\\s*(\\d+)", x, perl = TRUE
    ))[[1]]
    list(name = parts[2], start = as.integer(parts[3]), end = as.integer(parts[4]))
  })

  names_vec = sapply(parsed, `[[`, "name")
  start_vec = sapply(parsed, `[[`, "start")
  end_vec   = sapply(parsed, `[[`, "end")

  # Extract variable labels
  label_matches = gregexpr(
    'label variable\\s+(\\w+)\\s+"([^"]+)"',
    full, perl = TRUE
  )
  label_raw = regmatches(full, label_matches)[[1]]

  label_df = do.call(rbind, lapply(label_raw, function(x) {
    parts = regmatches(x, regexec(
      'label variable\\s+(\\w+)\\s+"([^"]+)"', x, perl = TRUE
    ))[[1]]
    data.frame(name = parts[2], label = trimws(parts[3]), stringsAsFactors = FALSE)
  }))

  # Combine into a data frame
  col_spec = data.frame(
    name  = names_vec,
    start = start_vec,
    end   = end_vec,
    width = end_vec - start_vec + 1,
    stringsAsFactors = FALSE
  )

  col_spec = merge(col_spec, label_df, by = "name", all.x = TRUE)
  col_spec = col_spec[order(col_spec$start), ]
  rownames(col_spec) = NULL

  col_spec
}

message("Parsing do-file...")
col_spec = parse_dofile(dofile_path)
message(sprintf("  Found %d variables", nrow(col_spec)))

# ---------------------------------------------------------------------------
# Read the ASCII data file
# Uses readr::read_fwf() with fixed-width column positions
# ---------------------------------------------------------------------------

message("Reading ASCII data file (this may take a moment)...")

fwf_cols = fwf_positions(
  start     = col_spec$start,
  end       = col_spec$end,
  col_names = col_spec$name
)

psid = read_fwf(
  file          = data_path,
  col_positions = fwf_cols,
  col_types     = cols(.default = col_integer()),  # all PSID vars are numeric
  progress      = TRUE
)

message(sprintf("  Loaded %d observations, %d variables", nrow(psid), ncol(psid)))

# ---------------------------------------------------------------------------
# Attach variable labels
# Stored as column attributes — visible in RStudio viewer,
# compatible with haven/labelled conventions
# ---------------------------------------------------------------------------

message("Attaching variable labels...")

label_lookup = setNames(col_spec$label, col_spec$name)

for (v in names(psid)) {
  if (!is.na(label_lookup[v])) {
    attr(psid[[v]], "label") = label_lookup[v]
  }
}

# ---------------------------------------------------------------------------
# Key identifier variables
# ER30001 = 1968 interview number  )  together these form the unique
# ER30002 = person number          )  person identifier across all waves
# ---------------------------------------------------------------------------

n_unique = nrow(distinct(psid, ER30001, ER30002))
message(sprintf("  Unique individuals (ER30001 + ER30002): %d", n_unique))
if (n_unique != nrow(psid)) {
  warning("ER30001 + ER30002 do not uniquely identify rows — check the data.")
}

# Single person ID for convenience
psid = psid |>
  mutate(pid = ER30001 * 1000 + ER30002, .before = everything())

# ---------------------------------------------------------------------------
# Save as .rds for fast reloading
# ---------------------------------------------------------------------------

out_path = here::here("data", "psid_wide.rds")
message(sprintf("Saving to %s...", out_path))
saveRDS(psid, out_path)
message("Done. Reload quickly with: psid <- readRDS(here::here('data', 'psid_wide.rds'))")

# ---------------------------------------------------------------------------
# Quick sanity checks
# ---------------------------------------------------------------------------

message("\n--- Quick checks ---")
message(sprintf("Dimensions: %d rows x %d cols", nrow(psid), ncol(psid)))

# Sex distribution (ER32000: 1 = Male, 2 = Female)
message("\nSex of individual (ER32000):")
print(table(psid$ER32000, useNA = "ifany"))

# Sequence number in 2023 wave (01 = head, 02 = spouse/partner, etc.)
if ("ER35102" %in% names(psid)) {
  message("\nSequence number in 2023 (ER35102) — 0 = not in sample that year:")
  print(table(psid$ER35102, useNA = "ifany"))
}

# Always-present flag (ER32001: 1 = always present, 0 = not)
if ("ER32001" %in% names(psid)) {
  message("\nAlways in responding FU (ER32001):")
  print(table(psid$ER32001, useNA = "ifany"))
}

psid = readRDS("data/psid_wide.rds")
