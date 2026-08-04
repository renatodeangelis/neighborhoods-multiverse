library(tidyverse)

ncdb_link = "https://www.dropbox.com/scl/fi/87zmcughgnq82z4ryl9o0/census_tracts.csv?rlkey=q5y8a0m5niaecqngvl5k0j5ca&st=chyki4ef&dl=1"
ffh = "https://www.dropbox.com/scl/fi/3wnon4x1eeofmjtxmuc13/ffh_2020.csv?rlkey=exryyge1ivnifv6z0ginhqcj9&st=r8nl3w4n&dl=1"
mcr = "https://www.dropbox.com/scl/fi/ni1cixeu7c6ad7da33aaw/mcr_1970.csv?rlkey=oy0yh3wh7zuo9sv9mgkyev9sl&st=ecyc3b97&dl=1"
faminc = "https://www.dropbox.com/scl/fi/8d6wzyeph75k5e8e3be9a/family_incomes.csv?rlkey=q2hwnlfvji7h7d8uqtxvsg26t&st=yz99ry86&dl=1"

ncdb = read_csv(ncdb_link)
ffh = read_csv(ffh)
mcr = read_csv(mcr)
faminc = read_csv(faminc)

# --- Append 2020 ACS family structure variables (FFH2A, FMC2A) from separate extract
ncdb = ncdb |>
  left_join(ffh, by = "GEO2020") |>
  left_join(mcr, by = "GEO2020") |>
  left_join(faminc, by = "GEO2020")

# --- Pre-compute dynamic drop lists on full column set before any removal
# Kept: Hispanic shares (SHRHSP, SHRHW, SHRHB, NONHISP, WBIAHSP, OTHHISX)
#       INDEMP (civilian employed); IND0* drops sector-level industry codes only
#       Summary income: AVHHIN, FAVINC, MDFAMY
#       Family structure proportions: FFH, FMC
#       Commute time bins: COMMUT2/4/X
nms = names(ncdb)

# N-suffix counts where a proportion exists under the same stem
drop_n = Filter(\(x) str_remove(x, "N$") %in% nms, nms[str_detect(nms, "N$")])

# D-suffix denominators where a proportion exists under the same stem
drop_d = Filter(\(x) str_remove(x, "D$") %in% nms, nms[str_detect(nms, "D$")])

# Mismatched-stem counts/denominators not caught by the suffix rules above
drop_mismatched = c(
  nms[str_detect(nms, "^WELFAR") & str_detect(nms, "[ND]$")],      # WELFAR_  vs WELFARE
  nms[str_detect(nms, "^AVWEL")  & !str_detect(nms, "^AVWELIN")],  # AVWEL_   vs AVWELIN
  nms[str_detect(nms, "^AVSEME") & !str_detect(nms, "^AVSEMER")],  # AVSEME_  vs AVSEMER
  nms[str_detect(nms, "^UNEMPT")],                                   # UNEMPT_  vs UNEMPRT
  nms[str_detect(nms, "^CMEPR|^CFEPR")]                             # CMEPR/CFEPR vs MEPR/FEPR
)

# Orphaned race/ethnicity population denominators (SHRxD, SHRxAD — no proportion counterpart)
drop_orphan_d = nms[str_detect(nms, "^SHR\\dA?D$")]

# --- Drop all irrelevant columns in one pass
ncdb = ncdb |>
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
      "OLD"                            # Prop. 65+ (age structure covered by ADULT, CHILD)
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

# --- Normalize ACS wave suffixes: strip trailing A (1A → 1, 2A → 2) and AR (1AR → 1)
# Where a decennial column has the same stem (e.g. ADULT1A vs ADULT1), prefer ACS
# for consistency — all substantive vars are ACS-sourced for the 2010/2019 waves.

# Identify decennial columns that duplicate ACS-measured variables
a_vars  = names(ncdb)[str_ends(names(ncdb), "A") & !str_ends(names(ncdb), "AR")]
ar_vars = names(ncdb)[str_ends(names(ncdb), "AR")]

decennial_dups = c(
  str_remove(a_vars,  "A$") [str_remove(a_vars,  "A$")  %in% names(ncdb)],
  str_remove(ar_vars, "AR$")[str_remove(ar_vars, "AR$") %in% names(ncdb)]
)

ncdb = ncdb |>
  select(-any_of(decennial_dups)) |>
  rename_with(\(x) str_remove(x, "A$"), ends_with("A") & !ends_with("AR"))

# Recheck after the A-rename: skip AR vars whose stripped name now exists
# (e.g. FNOLF1AR conflicts with FNOLF1 just renamed from FNOLF1A)
nms     = names(ncdb)
ar_vars = nms[str_ends(nms, "AR")]
ar_safe = ar_vars[!str_remove(ar_vars, "AR$") %in% nms]

ncdb = ncdb |>
  rename_with(\(x) str_remove(x, "AR$"), all_of(ar_safe))

# --- Convert wave digit suffixes to full years
# Mapping: 7 → 1970, 8 → 1980, 9 → 1990, 0 → 2000, 1 → 2010, 2 → 2020
# GEO2020/STATE/COUNTY excluded — their trailing digits are not wave suffixes
year_map = c("7" = "_1970", "8" = "_1980", "9" = "_1990",
             "0" = "_2000", "1" = "_2010", "2" = "_2020")

ncdb = ncdb |>
  rename_with(
    \(x) x |>
      str_replace("(\\d)AR$", \(m) paste0(year_map[str_sub(m, 1, 1)], "AR")) |>
      str_replace("\\d$",     \(m) year_map[m]),
    .cols = !any_of(c("GEO2020", "STATE", "COUNTY"))
  ) |>
  janitor::clean_names()

# --- Fix welfare/welfar name-shift: decennial WELFARE* and ACS WELFAR* are the same variable
ncdb = ncdb |> rename(welfare_2010 = welfar_2010, welfare_2020 = welfar_2020)

# --- Compute proportions for education, occupation, and housing count variables

# Education: each attainment category as prop. of persons 25+ (educpp)
ncdb = ncdb |>
  mutate(across(
    matches("^educ(8|11|12|15|16)_\\d{4}$"),
    \(x) x / .data[[str_replace(cur_column(), "^educ(8|11|12|15|16)_", "educpp_")]]
  ))

# Occupations: each category as prop. of civilian employed 16+ (indemp)
ncdb = ncdb |>
  mutate(across(
    matches("^(occ[1-9]|prfemp|uskocc)_\\d{4}$"),
    \(x) x / .data[[str_replace(cur_column(), "^(occ[1-9]|prfemp|uskocc)_", "indemp_")]]
  ))

# Housing: rntocc and ownocc as prop. of total housing units (tothsun)
ncdb = ncdb |>
  mutate(across(
    matches("^(rntocc|ownocc)_\\d{4}$"),
    \(x) x / .data[[str_replace(cur_column(), "^(rntocc|ownocc)_", "tothsun_")]]
  ))

# Drop denominators; numhhs is a raw count with no proportion counterpart
ncdb = ncdb |>
  select(-matches("^(educpp|indemp|tothsun|numhhs)_"))

empty_tracts = ncdb |>
  filter(trctpop_1970 == 0, trctpop_1980 == 0, trctpop_1990 == 0, trctpop_2000 == 0, trctpop_2010 == 0, trctpop_2020 == 0) |>
  select(geo2020)

ncdb = ncdb |>
  anti_join(empty_tracts, by = "geo2020")