library(tidyverse)
library(zoo)
library(data.table)

# --- Data sources
ncdb_url   = "https://www.dropbox.com/scl/fi/87zmcughgnq82z4ryl9o0/census_tracts.csv?rlkey=q5y8a0m5niaecqngvl5k0j5ca&st=chyki4ef&dl=1"
ffh_url    = "https://www.dropbox.com/scl/fi/3wnon4x1eeofmjtxmuc13/ffh_2020.csv?rlkey=exryyge1ivnifv6z0ginhqcj9&st=r8nl3w4n&dl=1"
faminc_url = "https://www.dropbox.com/scl/fi/ng5n7e9u2jdbwd9prk4cn/family_incomes.csv?rlkey=gfioqwxz1ycc4r3uq3i5e9urr&st=wdm2h7vk&dl=1"

# --- Helpers

# Return columns whose name-minus-suffix matches another column in nms
drop_suffix = function(nms, suffix) {
  candidates = nms[str_ends(nms, suffix)]
  candidates[str_remove(candidates, paste0(suffix, "$")) %in% nms]
}

# Divide matched columns by their denominators; pipeable
divide_by_denom = function(df, col_regex, denom_fn) {
  cols = names(df)[str_detect(names(df), col_regex)]
  for (col in cols) {
    denom = denom_fn(col)
    if (!denom %in% names(df))
      stop(sprintf("divide_by_denom: denominator '%s' not found for '%s'", denom, col))
    df[[col]] = df[[col]] / df[[denom]]
  }
  df
}

# famsub_1970 is absent from the supplemental extract (FAMSUB7 was not included);
# compute it from the exhaustive 1970 income bins if not already present.
add_famsub_1970 = function(df) {
  if ("famsub_1970" %in% names(df)) return(df)
  mutate(df, famsub_1970 = rowSums(select(df, matches("^falt.*_1970$")), na.rm = TRUE))
}

# =============================================================================
# Stage 1: Load and join
# =============================================================================

ncdb_raw = read_csv(ncdb_url) |>
  left_join(read_csv(ffh_url),    by = "GEO2020") |>
  left_join(read_csv(faminc_url), by = "GEO2020")

# =============================================================================
# Stage 2: Select columns
# =============================================================================
# Kept: Hispanic shares (SHRHSP, SHRHW, SHRHB, NONHISP, WBIAHSP, OTHHISX)
#       INDEMP (civilian employed); IND0* drops sector-level industry codes only
#       Summary income: AVHHIN, FAVINC, MDFAMY
#       Family structure proportions: FFH, FMC
#       Commute time bins: COMMUT2/4/X

nms = names(ncdb_raw)

drop_n          = drop_suffix(nms, "N")
drop_d          = drop_suffix(nms, "D")
drop_mismatched = c(
  nms[str_detect(nms, "^WELFAR") & str_detect(nms, "[ND]$")],      # WELFAR_  vs WELFARE
  nms[str_detect(nms, "^AVWEL")  & !str_detect(nms, "^AVWELIN")],  # AVWEL_   vs AVWELIN
  nms[str_detect(nms, "^AVSEME") & !str_detect(nms, "^AVSEMER")],  # AVSEME_  vs AVSEMER
  nms[str_detect(nms, "^UNEMPT")],                                   # UNEMPT_  vs UNEMPRT
  nms[str_detect(nms, "^CMEPR|^CFEPR")]                             # CMEPR/CFEPR vs MEPR/FEPR
)
drop_orphan_d   = nms[str_detect(nms, "^SHR\\dA?D$")]

ncdb_selected = ncdb_raw |>
  select(
    # Non-B/W race shares: AIAN, Asian, NHPI, other race — alone, min, max variants
    -starts_with(c(
      "SHRAMI", "SHRASN", "SHRHIP", "SHRAPI", "SHROTH",
      "SHRNHI", "SHRNHA", "SHRNHH", "SHRNHO", "SHRNHR",
      "MINAMI", "MAXAMI", "MINASN", "MAXASN", "MINHIP", "MAXHIP",
      "MINAPI", "MAXAPI", "MINOTH", "MAXOTH",
      "MINNHI", "MAXNHI", "MINNHR", "MAXNHR", "MINNHH", "MAXNHH",
      "MINNHA", "MAXNHA", "MINNHO", "MAXNHO"
    )),
    # White/Black alone-or-combination multiracial counting variants
    -starts_with(c(
      "MINWHT", "MAXWHT", "MINBLK", "MAXBLK",
      "MINNHW", "MAXNHW", "MINNHB", "MAXNHB"
    )),
    # Multiracial population counts
    -starts_with(c("MR1POP", "MR2POP", "MR3POP", "MRAPOP", "MRANHS", "MRAHSP")),
    # Race × sex (16+) and race × age (16–19)
    -starts_with(c(
      "WM16P", "WF16P", "BM16P", "BF16P", "IM16P", "IF16P",
      "AM16P", "AF16P", "RM16P", "RF16P", "PM16P", "PF16P",
      "OM16P", "OF16P", "MM16P", "MF16P", "HM16P", "HF16P", "XM16P", "XF16P",
      "WP1619", "BP1619", "IP1619", "AP1619", "RP1619", "PP1619",
      "OP1619", "MP1619", "HP1619", "XP1619"
    )),
    # Military, industry sector codes, children under 5, misc.
    -starts_with(c(
      "ARMFRM", "ARMFRF", "IND0", "KIDS",
      "ALTLAB", "SPANAM", "SPLANG", "YTHPOP", "OCC0"
    )),
    # Commuting destination and mode (keep time bins: COMMUT2/4/X)
    -starts_with(c(
      "WRCNTY", "WRKSM", "WRSTAT", "WRKSC", "WRKDM", "WRKDC", "WRKNM",
      "WORKSUB", "WORKNR", "TRVLPB", "WKHOME", "AUTO", "TRVLOT"
    )),
    # Income subtype means and avg public assistance $ (keep: AVHHIN, FAVINC, MDFAMY)
    -starts_with(c(
      "AVEMER", "AVSEMER", "AVGERN", "AVFINY", "AVSOCS", "AVRETR",
      "AVOTHY", "AVOTY", "AVPBLA", "AVWELIN", "AVSSRR"
    )),
    # Sample counts (long-form unweighted; not substantive)
    -starts_with(c("SMPPRS", "SMPHSU")),
    # Non-working week counts and orphaned sex/age denominators (survive suffix rules above)
    -starts_with(c("MNOPRT", "FNOPRT", "MNPRTB")),
    # Detailed family structure counts (keep FFH and FMC proportions only)
    -starts_with(c(
      "MCWKID", "MCNKID", "MHWKID", "MHNKID", "FHWKID", "FHNKID",
      "MOTHERS", "FATHERS", "NKID", "MCF", "MHH", "FHH"
    )),
    # Additional drops: redundant counts, housing subcomponents, out-of-scope
    -starts_with(c(
      "NONHISP",                       # Count; proportion implicit in SHRHSP
      "MDFAMY",                        # Median family income
      "OCCHU", "VACHU",               # Housing subcounts (keeping TOTHSUN, OWNOCC, RNTOCC)
      "LFRAT",                         # Labor force participation rate
      "FEMLAB", "FNOLF",              # Raw labor counts (keeping employment rates)
      # Note: INDEMP retained here; used as occupation denominator below, then dropped
      "OLD",                           # Prop. 65+ (age structure covered by ADULT, CHILD)
      # FFH7D appears in both census_tracts.csv and ffh_2020.csv; the join produces
      # FFH7D.x / FFH7D.y which end in x/y not D, bypassing drop_d — drop explicitly
      "FFH7D"
    )),
    # Partial-wave and single-wave variables dropped for insufficient coverage
    -starts_with(c(
      "EDUCA",                              # Associate degree: 1990+ only
      "SHRHB", "SHRHW",                    # Hispanic-Black/White subcats: 1980+ only
      "SHRNHB", "SHRNHW", "SHRNHJ",       # Non-Hisp. race subcats: 1980+ only
      "OTHHISX", "WBIAHSP",               # 1980-specific Hispanic composites
      "EMPMT",                             # 1970-only employed count (superseded by INDEMP)
      "SPNAME", "WRKOM",                   # 1970/1980 single-wave quirks
      "MNOLF",                             # Prime-age male non-LF: ACS only, 2 waves
      "PERS517", "PERS64", "PERS65P",     # ACS-only age group counts: 2010–2020
      "FEM15P", "MEN15P", "PERS15P"      # 2020-only ACS denominators
    )),
    # N/D suffix counts, mismatched stems, and orphaned SHR denominators (computed above)
    -any_of(c(drop_n, drop_d, drop_mismatched, drop_orphan_d))
  )

# =============================================================================
# Stage 3: Rename and normalize
# =============================================================================
# Normalize ACS wave suffixes: strip trailing A (1A → 1, 2A → 2) and AR (1AR → 1)
# Where a decennial column has the same stem (e.g. ADULT1A vs ADULT1), prefer ACS
# for consistency — all substantive vars are ACS-sourced for the 2010/2020 waves.

a_vars  = names(ncdb_selected)[str_ends(names(ncdb_selected), "A") & !str_ends(names(ncdb_selected), "AR")]
ar_vars = names(ncdb_selected)[str_ends(names(ncdb_selected), "AR")]

decennial_dups = c(
  str_remove(a_vars,  "A$") [str_remove(a_vars,  "A$")  %in% names(ncdb_selected)],
  str_remove(ar_vars, "AR$")[str_remove(ar_vars, "AR$") %in% names(ncdb_selected)]
)

# Strip A suffix, then recheck AR vars — some conflict with names just created by A-strip
# (e.g. FNOLF1AR conflicts with FNOLF1 renamed from FNOLF1A)
ncdb_a_stripped = ncdb_selected |>
  select(-any_of(decennial_dups)) |>
  rename_with(\(x) str_remove(x, "A$"), ends_with("A") & !ends_with("AR"))

ar_safe = names(ncdb_a_stripped)[str_ends(names(ncdb_a_stripped), "AR")]
ar_safe = ar_safe[!str_remove(ar_safe, "AR$") %in% names(ncdb_a_stripped)]

# Convert wave digit suffixes to full years, clean names, fix welfare stem
# Mapping: 7 → 1970, 8 → 1980, 9 → 1990, 0 → 2000, 1 → 2010, 2 → 2020
year_map = c("7" = "_1970", "8" = "_1980", "9" = "_1990",
             "0" = "_2000", "1" = "_2010", "2" = "_2020")

ncdb_renamed = ncdb_a_stripped |>
  rename_with(\(x) str_remove(x, "AR$"), all_of(ar_safe)) |>
  rename_with(
    \(x) x |>
      str_replace("(\\d)AR$", \(m) paste0(year_map[str_sub(m, 1, 1)], "AR")) |>
      str_replace("\\d$",     \(m) year_map[m]),
    .cols = !any_of(c("GEO2020", "STATE", "COUNTY"))
  ) |>
  janitor::clean_names() |>
  rename(welfare_2010 = welfar_2010, welfare_2020 = welfar_2020)

# =============================================================================
# Stage 4: Proportions and income harmonization
# =============================================================================

ncdb_props = ncdb_renamed |>
  # Education: each attainment category as prop. of persons 25+ (educpp)
  divide_by_denom(
    "^educ(8|11|12|15|16)_\\d{4}$",
    \(col) str_replace(col, "^educ(8|11|12|15|16)_", "educpp_")
  ) |>
  # Occupations: each category as prop. of civilian employed 16+ (indemp)
  divide_by_denom(
    "^(occ[1-9]|prfemp|uskocc)_\\d{4}$",
    \(col) str_replace(col, "^(occ[1-9]|prfemp|uskocc)_", "indemp_")
  ) |>
  # Housing: rntocc and ownocc as prop. of total housing units (tothsun)
  divide_by_denom(
    "^(rntocc|ownocc)_\\d{4}$",
    \(col) str_replace(col, "^(rntocc|ownocc)_", "tothsun_")
  ) |>
  # Ensure famsub_1970 exists before dividing (not in supplemental extract)
  add_famsub_1970() |>
  # Family income bins: each bracket as prop. of total families and subfamilies (famsub)
  divide_by_denom(
    "^(falt|fay0).*_\\d{4}$",
    \(col) paste0("famsub_", str_extract(col, "\\d{4}$"))
  ) |>
  # Drop denominators; numhhs is a raw count with no proportion counterpart
  select(-matches("^(educpp|indemp|tothsun|numhhs|famsub)_")) |>
  # Harmonize income bins into 8 cross-wave bands using thresholds ($10k, $15k, $25k, $50k)
  # that fall on clean bracket edges in all waves. Above $50k, early waves lack sub-brackets:
  #   1970: $50k+ undivided — full share assigned to inc_50_75; higher bands = NA
  #   1980: $75k+ undivided — full share assigned to inc_75_100; higher bands = NA
  mutate(
    # < $10,000
    inc_lt10_1970 = falt1_1970 + falt2_1970 + falt3_1970 + falt4_1970 + falt5_1970 +
                    falt6_1970 + falt7_1970 + falt8_1970 + falt9_1970 + falt10_1970,
    inc_lt10_1980 = falt3_1980 + falt5_1980 + falt8_1980 + falt10_1980,
    inc_lt10_1990 = falty5_1990 + falty10_1990,
    inc_lt10_2000 = fay010_2000,
    inc_lt10_2010 = fay010_2010,
    inc_lt10_2020 = fay010_2020,

    # $10,000–14,999
    inc_10_15_1970 = falt12_1970 + falt15_1970,
    inc_10_15_1980 = falt13_1980 + falt15_1980,
    inc_10_15_1990 = falt13_1990 + falt15_1990,
    inc_10_15_2000 = fay015_2000,
    inc_10_15_2010 = fay015_2010,
    inc_10_15_2020 = fay015_2020,

    # $15,000–24,999
    inc_15_25_1970 = falt25_1970,
    inc_15_25_1980 = falt18_1980 + falt20_1980 + falt23_1980 + falt25_1980,
    inc_15_25_1990 = falt18_1990 + falt20_1990 + falt23_1990 + falt25_1990,
    inc_15_25_2000 = fay020_2000 + fay025_2000,
    inc_15_25_2010 = fay020_2010 + fay025_2010,
    inc_15_25_2020 = fay020_2020 + fay025_2020,

    # $25,000–49,999
    inc_25_50_1970 = falt50_1970,
    inc_25_50_1980 = falt28_1980 + falt30_1980 + falt35_1980 + falt40_1980 + falt49_1980,
    inc_25_50_1990 = falt28_1990 + falt30_1990 + falt35_1990 + falt40_1990 + falt49_1990,
    inc_25_50_2000 = fay030_2000 + fay035_2000 + fay040_2000 + fay045_2000 + fay050_2000,
    inc_25_50_2010 = fay030_2010 + fay035_2010 + fay040_2010 + fay045_2010 + fay050_2010,
    inc_25_50_2020 = fay030_2020 + fay035_2020 + fay040_2020 + fay045_2020 + fay050_2020,

    # $50,000–74,999 (1970: entire $50k+ assigned here — no finer breakdown available)
    inc_50_75_1970 = faltmx_1970,
    inc_50_75_1980 = falt75_1980,
    inc_50_75_1990 = falt60_1990 + falt75_1990,
    inc_50_75_2000 = fay060_2000 + fay075_2000,
    inc_50_75_2010 = fay060_2010 + fay075_2010,
    inc_50_75_2020 = fay060_2020 + fay075_2020,

    # $75,000–99,999 (1970: NA; 1980: entire $75k+ assigned here — no finer breakdown available)
    inc_75_100_1970 = NA_real_,
    inc_75_100_1980 = faltmx_1980,
    inc_75_100_1990 = falt100_1990,
    inc_75_100_2000 = fay0100_2000,
    inc_75_100_2010 = fay0100_2010,
    inc_75_100_2020 = fay0100_2020,

    # $100,000–149,999 (1970 and 1980: NA)
    inc_100_150_1970 = NA_real_,
    inc_100_150_1980 = NA_real_,
    inc_100_150_1990 = falt125_1990 + falt150_1990,
    inc_100_150_2000 = fay0125_2000 + fay0150_2000,
    inc_100_150_2010 = fay0125_2010 + fay0150_2010,
    inc_100_150_2020 = fay0125_2020 + fay0150_2020,

    # $150,000+ (1970 and 1980: NA)
    inc_150p_1970 = NA_real_,
    inc_150p_1980 = NA_real_,
    inc_150p_1990 = faltmxb_1990,
    inc_150p_2000 = fay0200_2000 + fay0m20_2000,
    inc_150p_2010 = fay0200_2010 + fay0m20_2010,
    inc_150p_2020 = fay0200_2020 + fay0m20_2020
  ) |>
  select(-matches("^(falt|fay0).*_\\d{4}$"))

# =============================================================================
# Stage 5: Filter empty tracts
# =============================================================================

ncdb = ncdb_props |>
  filter(!if_all(matches("^trctpop_\\d{4}$"), ~ .x == 0)) |>
  mutate(across(where(is.numeric), ~ if_else(is.finite(.x), .x, NA_real_)))

# =============================================================================
# Stage 6: Reshape to long (one row per tract × wave)
# =============================================================================

ncdb_long = ncdb |>
  pivot_longer(
    cols          = matches("_\\d{4}$"),
    names_to      = c(".value", "year"),
    names_pattern = "^(.+)_(\\d{4})$"
  ) |>
  mutate(year = as.integer(year)) |>
  # Drop 1970 and 1980 tract-years with zero population: coverage artifact
  # (counties not yet tracted in those waves), confirmed by 98% having real
  # 1990 populations. Genuine uninhabited tracts in later waves are retained.
  filter(!(trctpop == 0 & year %in% c(1970L, 1980L)))

# =============================================================================
# Stage 7: Annual linear interpolation (1970–2020)
# =============================================================================
# Expands each tract from 6 wave observations to 51 annual observations.
# na.approx interpolates between adjacent non-NA endpoints only — NAs with
# no non-NA boundary on one side (structural or coverage) remain NA.

# Expand each tract only within its observed year range, so coverage-dropped
# early waves are not reintroduced as all-NA rows
full_grid     = ncdb_long |>
  group_by(geo2020) |>
  reframe(year = seq(min(year), max(year), by = 1L))
tract_meta    = distinct(ncdb_long, geo2020, state, county)
ncdb_expanded = full_grid |>
  left_join(select(ncdb_long, -state, -county), by = c("geo2020", "year")) |>
  left_join(tract_meta, by = "geo2020")

# Interpolate with data.table (faster than grouped dplyr mutate across 84k groups)
dt       = as.data.table(ncdb_expanded)[order(geo2020, year)]
num_cols = setdiff(names(dt)[sapply(dt, is.numeric)], "year")
dt[, (num_cols) := lapply(.SD, zoo::na.approx, x = year, na.rm = FALSE),
   by = geo2020, .SDcols = num_cols]
ncdb_annual = as_tibble(dt) |>
  mutate(trctpop = round(trctpop))

arrow::write_parquet(ncdb_annual, here::here("data/ncdb_annual.parquet"))
