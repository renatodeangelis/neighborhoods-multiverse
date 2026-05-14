# =============================================================================
# 01_convert_raw.R
# Convert raw PSID fixed-width .txt files to .RData for use with psidR
# =============================================================================
# Reads each famYEAR/.txt using column positions parsed from the .do file,
# selects only the variables in crosswalk.csv, and saves FAMyyyy.RData.
# Also converts the individual file (IND2023ER.txt) to IND2023ER.RData.
# Run after 00_crosswalk.R has produced crosswalk.csv.

library(readr)
library(dplyr)
library(purrr)
library(here)

# -----------------------------------------------------------------------------
# Paths — update when data moves to Dropbox
# -----------------------------------------------------------------------------

raw_dir  <- "~/Downloads"
out_dir  <- "~/Downloads/psid_rdata"   # staging area; move to Dropbox when ready
dir.create(out_dir, showWarnings = FALSE)

years <- c(1968:1996, 1997, 1999, 2001, 2003)

crosswalk <- read.csv(here("Code", "crosswalk.csv"))

# -----------------------------------------------------------------------------
# Helper: parse a PSID .do file → data frame of (varname, start, end)
# -----------------------------------------------------------------------------

parse_do_positions <- function(do_path) {
  lines <- readLines(do_path)

  # infix block: lines like "  VARNAME  start - end"
  infix_start <- grep("^infix", lines)
  infix_end   <- grep("^\\s*using", lines)
  infix_lines <- lines[(infix_start + 1):(infix_end - 1)]

  # pull out variable name and column range
  matches <- regmatches(
    infix_lines,
    regexpr("(long\\s+)?([A-Z0-9_]+)\\s+(\\d+)\\s*-\\s*(\\d+)", infix_lines, perl = TRUE)
  )
  matches <- matches[nchar(matches) > 0]

  parsed <- regmatches(
    matches,
    regexec("(long\\s+)?([A-Z0-9_]+)\\s+(\\d+)\\s*-\\s*(\\d+)", matches, perl = TRUE)
  )

  data.frame(
    varname = sapply(parsed, `[`, 3),
    start   = as.integer(sapply(parsed, `[`, 4)),
    end     = as.integer(sapply(parsed, `[`, 5)),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Convert family files
# -----------------------------------------------------------------------------

convert_family_year <- function(yr) {
  # folder and file naming differ between V-era and ER-era
  suffix   <- if (yr >= 1994) "er" else ""
  folder   <- file.path(raw_dir, paste0("fam", yr, suffix))
  do_file  <- list.files(folder, pattern = "\\.do$", full.names = TRUE, ignore.case = TRUE)[1]
  txt_file <- list.files(folder, pattern = "\\.txt$", full.names = TRUE, ignore.case = TRUE)[1]

  positions <- parse_do_positions(do_file)

  # keep only variables in the crosswalk for this year
  keep_vars <- crosswalk |>
    filter(year == yr) |>
    pull(varname)

  pos_sub <- positions |> filter(varname %in% keep_vars)

  if (nrow(pos_sub) == 0) {
    warning("No matching variables for year ", yr, " — skipping")
    return(invisible(NULL))
  }

  fwf_spec <- fwf_positions(
    start = pos_sub$start,
    end   = pos_sub$end,
    col_names = pos_sub$varname
  )

  dat <- read_fwf(txt_file, col_positions = fwf_spec, show_col_types = FALSE)

  # psidR expects object named fam<YYYY> or FAM<YYYY>
  obj_name <- paste0("fam", yr)
  assign(obj_name, dat)
  out_file <- file.path(out_dir, paste0("FAM", yr, "ER.RData"))
  save(list = obj_name, file = out_file)
  message("Saved ", out_file, " (", nrow(dat), " rows, ", ncol(dat), " cols)")
}

# walk(years, convert_family_year)   # uncomment to run

# -----------------------------------------------------------------------------
# Convert individual file
# -----------------------------------------------------------------------------

convert_individual <- function() {
  folder   <- file.path(raw_dir, "ind2023er")
  do_file  <- list.files(folder, pattern = "^IND.*\\.do$", full.names = TRUE, ignore.case = TRUE)[1]
  txt_file <- list.files(folder, pattern = "^IND.*\\.txt$", full.names = TRUE, ignore.case = TRUE)[1]

  positions <- parse_do_positions(do_file)

  # Keep the core ID variables + any individual-level vars (e.g. survey weights)
  # ER30001 = 1968 person ID, ER30002 = person number within family
  # Add survey weight variables here as needed (check ind.vars in psidR docs)
  keep_vars <- c("ER30001", "ER30002")  # TODO: add weight variable names

  pos_sub <- positions |> filter(varname %in% keep_vars)

  fwf_spec <- fwf_positions(
    start = pos_sub$start,
    end   = pos_sub$end,
    col_names = pos_sub$varname
  )

  IND2023ER <- read_fwf(txt_file, col_positions = fwf_spec, show_col_types = FALSE)

  out_file <- file.path(out_dir, "IND2023ER.RData")
  save(IND2023ER, file = out_file)
  message("Saved ", out_file, " (", nrow(IND2023ER), " rows)")
}

# convert_individual()   # uncomment to run
