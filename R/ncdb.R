library(tidyverse)

ncdb_link = "https://www.dropbox.com/scl/fi/87zmcughgnq82z4ryl9o0/census_tracts.csv?rlkey=q5y8a0m5niaecqngvl5k0j5ca&st=chyki4ef&dl=1"
ncdb = read_csv(ncdb_link)

# --- Drop columns not relevant to analysis
# Removed: non-B/W race shares, female labor force, multiracial, race-by-sex, military, industry
# Kept: Hispanic shares (SHRHSP, SHRHW, SHRHB, NONHISP, WBIAHSP, OTHHISX)
# Note: IND0* drops industry codes only; INDEMP (civilian employed persons) is kept
drop_prefixes = c(
  # Non-B/W race shares (AIAN, Asian, NHPI, other race — alone, min, max variants)
  "SHRAMI", "SHRASN", "SHRHIP", "SHRAPI", "SHROTH",
  "SHRNHI", "SHRNHA", "SHRNHH", "SHRNHO", "SHRNHR",
  "MINAMI", "MAXAMI", "MINASN", "MAXASN", "MINHIP", "MAXHIP",
  "MINAPI", "MAXAPI", "MINOTH", "MAXOTH",
  "MINNHI", "MAXNHI", "MINNHR", "MAXNHR", "MINNHH", "MAXNHH",
  "MINNHA", "MAXNHA", "MINNHO", "MAXNHO",
  # Multiracial
  "MR1POP", "MR2POP", "MR3POP", "MRAPOP", "MRANHS", "MRAHSP",
  # Race × sex (16+) and race × age (16–19)
  "WM16P", "WF16P", "BM16P", "BF16P", "IM16P", "IF16P",
  "AM16P", "AF16P", "RM16P", "RF16P", "PM16P", "PF16P",
  "OM16P", "OF16P", "MM16P", "MF16P", "HM16P", "HF16P", "XM16P", "XF16P",
  "WP1619", "BP1619", "IP1619", "AP1619", "RP1619", "PP1619",
  "OP1619", "MP1619", "HP1619", "XP1619",
  # Military
  "ARMFRM", "ARMFRF",
  # Industry codes (IND0* prefix targets only the sector-level counts)
  "IND0"
)

ncdb = ncdb |> select(-starts_with(drop_prefixes))

# --- Drop counts (N-suffix) and denominators (D-suffix) where a proportion also exists
# Keeps counts with no share counterpart (e.g. TRCTPOP, UNEMPT)
# Note: a few count/share pairs have mismatched stems (e.g. WELFAR7N / WELFARE7,
# UNEMPT7N / UNEMPRT7) and are not caught here — review manually if needed.
nms    = names(ncdb)
drop_n = Filter(\(x) str_remove(x, "N$") %in% nms, nms[str_detect(nms, "N$")])
drop_d = Filter(\(x) str_remove(x, "D$") %in% nms, nms[str_detect(nms, "D$")])
ncdb   = ncdb |> select(-all_of(c(drop_n, drop_d)))
