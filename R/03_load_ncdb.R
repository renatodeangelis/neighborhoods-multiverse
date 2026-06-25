# Load NCDB (Neighborhood Change Database) — census tract panel, 1970–2020

library(tidyverse)

ncdb_path = '/Users/renatodeangelis/Downloads/ncdb.csv'

ncdb_raw = read_csv(
  ncdb_path,
  col_types = cols(GEO2020 = col_character(), .default = col_double()),
  na = c("", "NA", "-999", "-999.0")
)

# Quick inspection
glimpse(ncdb_raw)
cat("\nRows:", nrow(ncdb_raw), "| Columns:", ncol(ncdb_raw), "\n")
cat("Geographic coverage — unique states:", n_distinct(ncdb_raw$STATE), "\n")
cat("Sample GEO2020 IDs:\n")
print(head(ncdb_raw$GEO2020, 5))
