# =============================================================================
# 00_crosswalk.R
# Build a concept x year x raw_varname crosswalk using psidR + UMich psid.xlsx
# =============================================================================
# Run this once to generate crosswalk.csv, which drives all downstream scripts.
# Before running: install.packages("psidR") if not already installed.

library(psidR)
library(openxlsx)
library(dplyr)

# -----------------------------------------------------------------------------
# Paths — update when data moves to Dropbox
# -----------------------------------------------------------------------------

raw_dir  = "~/Downloads"          # root of fam* and ind* folders
code_dir = here::here("Code")     # this script lives here
out_dir  = here::here("Code")     # save crosswalk.csv alongside scripts

# -----------------------------------------------------------------------------
# Load UMich cross-year variable index
# The psid.xlsx file ships with psidR; alternatively download from UMich.
# -----------------------------------------------------------------------------

psid_xlsx = file.path(system.file(package = "psidR"), "psid-lists", "psid.xlsx")
cwf = read.xlsx(psid_xlsx)

# -----------------------------------------------------------------------------
# Years of interest
# Annual 1968-1996, biennial 1997-2003
# -----------------------------------------------------------------------------

years = c(1968:1996, 1997, 1999, 2001, 2003)

# -----------------------------------------------------------------------------
# Define concepts to track
# For each concept: find one known variable name in any year, then let
# getNamesPSID() return the correct name for every year.
#
# Steps:
#   1. Look up each variable in the PSID cross-year index (simba.isr.umich.edu)
#   2. Add a row below with the known variable name and the concept label
#   3. Run getNamesPSID() to fill in names across all years
# -----------------------------------------------------------------------------

# TODO: populate with actual variable names after consulting cross-year index
# Format: getNamesPSID("<any-year varname>", cwf, years = years)

concepts = list(
  # interview_id  = getNamesPSID("V3",       cwf, years = years),  # 1968 interview #
  # fam_income    = getNamesPSID("V74",      cwf, years = years),  # total family income
  # fam_size      = getNamesPSID("V30",      cwf, years = years),  # family size
  # state         = getNamesPSID("V93",      cwf, years = years),  # state of residence
  # county        = getNamesPSID("...",      cwf, years = years),  # county (availability varies)
  # head_race     = getNamesPSID("...",      cwf, years = years),
  # head_educ     = getNamesPSID("...",      cwf, years = years),
  # head_age      = getNamesPSID("...",      cwf, years = years)
)

# -----------------------------------------------------------------------------
# Assemble into a flat data frame and save
# -----------------------------------------------------------------------------

# crosswalk = bind_rows(
#   lapply(names(concepts), function(concept) {
#     data.frame(concept = concept, year = years, varname = concepts[[concept]])
#   })
# )
#
# write.csv(crosswalk, file.path(out_dir, "crosswalk.csv"), row.names = FALSE)
# message("Saved crosswalk.csv with ", nrow(crosswalk), " rows")
