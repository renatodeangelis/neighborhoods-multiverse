# Build PSID Panel — Manual Merge
# Parses raw ASCII + do-files, selects needed columns per wave, stacks
# 43 family files, joins to individual crosswalk. No psidR dependency.
# ---------------------------------------------------------------------------
# Before running: ~/Downloads must contain the unzipped PSID folders:
#   fam1968/ ... fam1993/       (V-series, annual 1968-1993)
#   fam1994er/ ... fam2023er/   (ER-series, biennial after 1997)
#   ind2023er/                  (cross-year individual file)
# ---------------------------------------------------------------------------

library(tidyverse)
library(here)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

RAW_DIR = "~/Downloads"
dir.create(here::here("data"), showWarnings = FALSE, recursive = TRUE)

YEARS = c(1968:1997, seq(1999, 2023, 2))   # 43 waves

# ---------------------------------------------------------------------------
# Helper: parse PSID fixed-width do-file → column spec (name, start, end)
# ---------------------------------------------------------------------------

parse_dofile = function(path) {
  txt  = readLines(path)
  full = paste(txt, collapse = "\n")

  infix = regmatches(full, regexpr("(?s)infix\\s+(.+?)using\\s+", full, perl = TRUE))
  infix = sub("^infix\\s+", "", infix)
  infix = sub("using\\s+$", "", infix)

  m   = gregexpr("(?:long\\s+)?(\\w+)\\s+(\\d+)\\s*-\\s*(\\d+)", infix, perl = TRUE)
  raw = regmatches(infix, m)[[1]]

  parsed = lapply(raw, function(x) {
    p = regmatches(x, regexec("(?:long\\s+)?(\\w+)\\s+(\\d+)\\s*-\\s*(\\d+)", x, perl = TRUE))[[1]]
    data.frame(name = p[2], start = as.integer(p[3]), end = as.integer(p[4]),
               stringsAsFactors = FALSE)
  })

  do.call(rbind, parsed) |> mutate(width = end - start + 1)
}

# ---------------------------------------------------------------------------
# File-path helpers (family files differ in naming before/after 1994)
# ---------------------------------------------------------------------------

fam_paths = function(yr) {
  if (yr <= 1993) {
    list(
      do  = file.path(RAW_DIR, sprintf("fam%d",   yr), sprintf("FAM%d.do",   yr)),
      txt = file.path(RAW_DIR, sprintf("fam%d",   yr), sprintf("FAM%d.txt",  yr))
    )
  } else {
    list(
      do  = file.path(RAW_DIR, sprintf("fam%der", yr), sprintf("FAM%dER.do",  yr)),
      txt = file.path(RAW_DIR, sprintf("fam%der", yr), sprintf("FAM%dER.txt", yr))
    )
  }
}

# ---------------------------------------------------------------------------
# Family-level variable crosswalk
#
# One row per wave. Values are PSID codes; NA = not collected that year.
# Column names become variable names in the output panel.
#
# CORRECTIONS vs. earlier version:
#   housing_tenure 2015: ER60030 (codebook; ER60029 was wrong)
#   house_value    2015: ER60031 (codebook; ER60030 was wrong)
#   educ_head: 1975-1984 has 10 values; 1985-1990 are 6 NAs;
#              V20198/V21504/V23333 = 1991/1992/1993 (not 1988-1990)
# ---------------------------------------------------------------------------

fam_vars = tibble(
  year = YEARS,

  # --- Geographic ---
  region = c(
    "V361","V876","V1572","V2284","V2911","V3279","V3699","V4178","V5054",
    "V5633","V6180","V6773","V7419","V8071","V8695","V9381","V11028","V12379",
    "V13631","V14678","V16152","V17538","V18889","V20189","V21495","V23327",
    "ER4157E","ER6997E","ER9248E","ER12221E","ER16430","ER20376","ER24143",
    "ER28042","ER41032","ER46974","ER52398","ER58215","ER65451","ER71530",
    "ER77591","ER81918","ER85772"),

  state_gsa = c(
    "V93","V537","V1103","V1803","V2403","V3003","V3403","V3803","V4303",
    "V5203","V5703","V6303","V6903","V7503","V8203","V8803","V10003","V11103",
    "V12503","V13703","V14803","V16303","V17703","V19003","V20303","V21603",
    "ER4156","ER6996","ER9247","ER12221","ER13004","ER17004","ER21003",
    "ER25003","ER36003","ER42003","ER47303","ER53003","ER60003","ER66003",
    "ER72003","ER78003","ER82003"),

  state_fips = c(
    rep(NA_character_, 17),
    "V12380","V13632","V14679","V16153","V17539","V18890","V20190","V21496",
    "V23328","ER4157","ER6997","ER9248","ER10004","ER13005","ER17005",
    "ER21004","ER25004","ER36004","ER42004","ER47304","ER53004","ER60004",
    "ER66004","ER72004","ER78004","ER82004"),

  # Beale's urbanicity (1994-2013 only)
  urbanicity = c(
    rep(NA_character_, 26),
    "ER4157F","ER6997F","ER9248F","ER12221F","ER16431C","ER20377C","ER24144A",
    "ER28043A","ER41033A","ER46975A","ER52399A","ER58216",
    rep(NA_character_, 5)),

  # MSA/metro status (2015-2023 only)
  msa_status = c(
    rep(NA_character_, 38),
    "ER65452","ER71531","ER77592","ER81919","ER85773"),

  region_grew_up = c(
    "V362","V877","V1573","V2285","V2912","V3280","V3700","V4179","V5055",
    "V5634","V6181","V6774","V7420","V8072","V8696","V9382","V11029","V12383",
    "V13635","V14682","V16156","V17542","V18893","V20193","V21499","V23331",
    "ER4157C","ER6997C","ER9248C","ER12221C","ER16431A","ER20377A","ER24146",
    "ER28045","ER41035","ER46977","ER52401","ER58219","ER65455","ER71534",
    "ER77595","ER81922","ER85776"),

  # State head grew up (1968-1996 only)
  grew_up_state_gsa = c(
    "V311","V787","V1477","V2189","V2815","V3233","V3655","V4131","V4674",
    "V5594","V6143","V6740","V7373","V8025","V8649","V9335","V10982","V11915",
    "V13542","V14589","V16063","V17460","V18791","V20091","V21397","V23254",
    "ER3919A","ER6789A","ER9035A",
    rep(NA_character_, 14)),

  # --- Demographic ---
  age_head = c(
    "V117","V1008","V1239","V1942","V2542","V3095","V3508","V3921","V4436",
    "V5350","V5850","V6462","V7067","V7658","V8352","V8961","V10419","V11606",
    "V13011","V14114","V15130","V16631","V18049","V19349","V20651","V22406",
    "ER2007","ER5006","ER7006","ER10009","ER13010","ER17013","ER21017",
    "ER25017","ER36017","ER42017","ER47317","ER53017","ER60017","ER66017",
    "ER72017","ER78017","ER82018"),

  sex_head = c(
    "V119","V1010","V1240","V1943","V2543","V3096","V3509","V3922","V4437",
    "V5351","V5851","V6463","V7068","V7659","V8353","V8962","V10420","V11607",
    "V13012","V14115","V15131","V16632","V18050","V19350","V20652","V22407",
    "ER2008","ER5007","ER7007","ER10010","ER13011","ER17014","ER21018",
    "ER25018","ER36018","ER42018","ER47318","ER53018","ER60018","ER66018",
    "ER72018","ER78018","ER82019"),

  race_head = c(
    "V181","V801","V1490","V2202","V2828","V3300","V3720","V4204","V5096",
    "V5662","V6209","V6802","V7447","V8099","V8723","V9408","V11055","V11938",
    "V13565","V14612","V16086","V17483","V18814","V20114","V21420","V23276",
    "ER3944","ER6814","ER9060","ER11848","ER15928","ER19989","ER23426",
    "ER27393","ER40565","ER46543","ER51904","ER57659","ER64810","ER70882",
    "ER76897","ER81144","ER85121"),

  # Hispanicity (1985-1996, gap 1997-2003, 2005-2023)
  hispanic_head = c(
    rep(NA_character_, 17),
    "V11937","V13564","V14611","V16085","V17482","V18813","V20113","V21419",
    "V23275","ER3941","ER6811","ER9057",
    rep(NA_character_, 4),
    "ER27392","ER40564","ER46542","ER51903","ER57658","ER64809","ER70881",
    "ER76896","ER81143","ER85120"),

  # Marital status — cohabitors coded as married (full coverage)
  marital_status = c(
    "V239","V607","V1365","V2072","V2670","V3181","V3598","V4053","V4603",
    "V5650","V6197","V6790","V7435","V8087","V8711","V9419","V11065","V12426",
    "V13665","V14712","V16187","V17565","V18916","V20216","V21522","V23336",
    "ER4159A","ER6999A","ER9250A","ER12223A","ER16423","ER20369","ER24150",
    "ER28049","ER41039","ER46983","ER52407","ER58225","ER65461","ER71540",
    "ER77601","ER81928","ER85782"),

  # Marital status — reported (1977-2023)
  marital_reported = c(
    rep(NA_character_, 9),
    "V5502","V6034","V6659","V7261","V7952","V8603","V9276","V10426","V11612",
    "V13017","V14120","V15136","V16637","V18055","V19355","V20657","V22412",
    "ER2014","ER5013","ER7013","ER10016","ER13021","ER17024","ER21023",
    "ER25023","ER36023","ER42023","ER47323","ER53023","ER60024","ER66024",
    "ER72024","ER78025","ER82026"),

  # --- Education ---
  # 1968-1974: not collected (7 NAs)
  # 1975-1984: collected (10 values)
  # 1985-1990: not collected (6 NAs)
  # 1991-1993: collected (V20198, V21504, V23333)
  # 1994-2023: ER series (17 values)
  educ_head = c(
    rep(NA_character_, 7),
    "V4093","V4684","V5608","V6157","V6754","V7387","V8039","V8663","V9349","V10996",
    rep(NA_character_, 6),
    "V20198","V21504","V23333",
    "ER4158","ER6998","ER9249","ER12222","ER16516","ER20457","ER24148",
    "ER28047","ER41037","ER46981","ER52405","ER58223","ER65459","ER71538",
    "ER77599","ER81926","ER85780"),

  # College attended — head (1985-2023)
  college_head = c(
    rep(NA_character_, 17),
    "V11956","V13579","V14626","V16100","V17497","V18828","V20128","V21434",
    "V23290","ER3959","ER6829","ER9075","ER11865","ER15948","ER20009",
    "ER23446","ER27413","ER40585","ER46563","ER51924","ER57680","ER64832",
    "ER70904","ER76919","ER81166","ER85143"),

  # --- Family structure ---
  family_size = c(
    "V115","V549","V1238","V1941","V2541","V3094","V3507","V3920","V4435",
    "V5349","V5849","V6461","V7066","V7657","V8351","V8960","V10418","V11605",
    "V13010","V14113","V15129","V16630","V18048","V19348","V20650","V22405",
    "ER2006","ER5005","ER7005","ER10008","ER13009","ER17012","ER21016",
    "ER25016","ER36016","ER42016","ER47316","ER53016","ER60016","ER66016",
    "ER72016","ER78016","ER82017"),

  # --- Income / poverty ---
  total_family_income = c(
    "V81","V529","V1514","V2226","V2852","V3256","V3676","V4154","V5029",
    "V5626","V6173","V6766","V7412","V8065","V8689","V9375","V11022","V12371",
    "V13623","V14670","V16144","V17533","V18875","V20175","V21481","V23322",
    "ER4153","ER6993","ER9244","ER12079","ER16462","ER20456","ER24099",
    "ER28037","ER41027","ER46935","ER52343","ER58152","ER65349","ER71426",
    "ER77448","ER81775","ER85629"),

  needs_census = c(
    "V440","V1017","V1768","V2347","V2981","V3312","V3732","V4233","V5115",
    "V5683","V6222","V6816","V7458","V8112","V8740","V9434","V11080","V12447",
    "V13688","V14738","V16209","V17613","V18884","V20184","V21490","V23326",
    "ER4155","ER6995","ER9246","ER12220","ER16427","ER20373","ER24140",
    "ER28039","ER41029","ER46972","ER52396","ER58213","ER65449","ER71528",
    "ER77589","ER81916","ER85770"),

  # USDA low-cost needs (ratio form 1968-1992; dollar standard 1994-2007)
  needs_usda = c(
    "V325","V835","V1530","V2242","V2868","V3272","V3692","V4171","V5047",
    "V5629","V6176","V6769","V7415","V8067","V8691","V9377","V11024","V12375",
    "V13627","V14674","V16148",NA_character_,"V18883","V20183","V21489",NA_character_,
    "ER4154","ER6994","ER9245","ER12219","ER16426","ER20372","ER24139",
    "ER28038","ER41028",rep(NA_character_, 8)),

  tanf_receipt = c(
    rep(NA_character_, 26),
    "ER3262","ER6262","ER8379","ER11272","ER14538","ER18697","ER22069",
    "ER26050","ER37068","ER43059","ER48381","ER54059","ER61101","ER67153",
    "ER73176","ER79272","ER83247"),

  welfare_payments = c(
    rep(NA_character_, 18),
    "V12830","V13932","V14947","V16447","V17863","V19163","V20463","V22010",
    rep(NA_character_, 7),
    "ER27958","ER40948","ER46856","ER52264","ER58065","ER65258","ER71335",
    "ER77357","ER81684","ER85538"),

  # --- Housing ---
  housing_tenure = c(
    "V103","V593","V1264","V1967","V2566","V3108","V3522","V3939","V4450",
    "V5364","V5864","V6479","V7084","V7675","V8364","V8974","V10437","V11618",
    "V13023","V14126","V15140","V16641","V18072","V19372","V20672","V22427",
    "ER2032","ER5031","ER7031","ER10035","ER13040","ER17043","ER21042",
    "ER25028","ER36028","ER42029","ER47329","ER53029","ER60030","ER66030",
    "ER72030","ER78031","ER82032"),

  house_value = c(
    "V5","V449","V1122","V1823","V2423","V3021","V3417","V3817","V4318",
    "V5217","V5717","V6319","V6917","V7517","V8217","V8817","V10018","V11125",
    "V12524","V13724","V14824","V16324","V17724","V19024","V20324","V21610",
    "ER2033","ER5032","ER7032","ER10036","ER13041","ER17044","ER21043",
    "ER25029","ER36029","ER42030","ER47330","ER53030","ER60031","ER66031",
    "ER72031","ER78032","ER82033"),

  moved = c(
    NA_character_,"V603","V1274","V1979","V2577","V3110","V3524","V3941","V4452",
    "V5366","V5866","V6484","V7089","V7700","V8369","V8999","V10447","V11628",
    "V13037","V14140","V15148","V16649","V18087","V19387","V20687","V22441",
    "ER2062","ER5061","ER7155","ER10072","ER13077","ER17088","ER21117",
    "ER25098","ER36103","ER42132","ER47440","ER53140","ER60155","ER66156",
    "ER72156","ER78158","ER82141")
)

stopifnot(all(map_int(fam_vars, length) == length(YEARS)))

# ---------------------------------------------------------------------------
# Individual-level variable crosswalk
#
# interview_num: links each person to their family file each wave.
# For 1968, this equals ER30001 (the permanent ID).
# Value = 0 in later waves means not in sample that year.
# ---------------------------------------------------------------------------

ind_vars = tibble(
  year = YEARS,

  interview_num = c(
    "ER30001","ER30020","ER30043","ER30067","ER30091","ER30117","ER30138",
    "ER30160","ER30188","ER30217","ER30246","ER30283","ER30313","ER30343",
    "ER30373","ER30399","ER30429","ER30463","ER30498","ER30535","ER30570",
    "ER30606","ER30642","ER30689","ER30733","ER30806",
    "ER33101","ER33201","ER33301","ER33401","ER33501","ER33601","ER33701",
    "ER33801","ER33901","ER34001","ER34101","ER34201","ER34301","ER34501",
    "ER34701","ER34901","ER35101"),

  age_ind = c(
    "ER30004","ER30023","ER30046","ER30070","ER30094","ER30120","ER30141",
    "ER30163","ER30191","ER30220","ER30249","ER30286","ER30316","ER30346",
    "ER30376","ER30402","ER30432","ER30466","ER30501","ER30538","ER30573",
    "ER30609","ER30645","ER30692","ER30736","ER30809","ER33104","ER33204",
    "ER33304","ER33404","ER33504","ER33604","ER33704","ER33804","ER33904",
    "ER34004","ER34104","ER34204","ER34305","ER34504","ER34704","ER34904",
    "ER35104"),

  # NA in 1969 — not collected that wave
  educ_ind = c(
    "ER30010",NA_character_,"ER30052","ER30076","ER30100","ER30126","ER30147",
    "ER30169","ER30197","ER30226","ER30255","ER30296","ER30326","ER30356",
    "ER30384","ER30413","ER30443","ER30478","ER30513","ER30549","ER30584",
    "ER30620","ER30657","ER30703","ER30748","ER30820","ER33115","ER33215",
    "ER33315","ER33415","ER33516","ER33616","ER33716","ER33817","ER33917",
    "ER34020","ER34119","ER34230","ER34349","ER34548","ER34752","ER34952",
    "ER35152"),

  # Employment status (starts 1979)
  emp_status = c(
    rep(NA_character_, 11),
    "ER30293","ER30323","ER30353","ER30382","ER30411","ER30441","ER30474",
    "ER30509","ER30545","ER30580","ER30616","ER30653","ER30699","ER30744",
    "ER30816","ER33111","ER33211","ER33311","ER33411","ER33512","ER33612",
    "ER33712","ER33813","ER33913","ER34016","ER34116","ER34216","ER34317",
    "ER34516","ER34716","ER34916","ER35116"),

  # Move in/out indicator
  moved_ind = c(
    "ER30006","ER30025","ER30048","ER30072","ER30096","ER30122","ER30143",
    "ER30165","ER30193","ER30222","ER30251","ER30288","ER30318","ER30348",
    "ER30378","ER30406","ER30436","ER30470","ER30505","ER30542","ER30577",
    "ER30613","ER30649","ER30696","ER30740","ER30813","ER33108","ER33208",
    "ER33308","ER33408","ER33508","ER33608","ER33708","ER33808","ER33908",
    "ER34008","ER34108","ER34208","ER34309","ER34508","ER34708","ER34908",
    "ER35108"),

  # Year individual moved in/out
  moved_yr_ind = c(
    "ER30008","ER30027","ER30050","ER30074","ER30098","ER30124","ER30145",
    "ER30167","ER30195","ER30224","ER30253","ER30290","ER30320","ER30350",
    "ER30380","ER30408","ER30438","ER30472","ER30507","ER30544","ER30579",
    "ER30615","ER30651","ER30698","ER30742","ER30815","ER33110","ER33210",
    "ER33310","ER33410","ER33510","ER33610","ER33710","ER33810","ER33910",
    "ER34010","ER34110","ER34210","ER34311","ER34510","ER34710","ER34910",
    "ER35110"),

  # Sequence number: role in family unit that wave.
  # 0 = not in sample; 1 = head; 2 = spouse/partner; 10-19 = children; etc.
  # Use seq_num > 0 (not interview_num > 0) to identify truly enumerated
  # person-years — interview_num is inherited from ER30001 and is never 0.
  seq_num = c(
    "ER30003","ER30021","ER30044","ER30068","ER30092","ER30118","ER30139",
    "ER30161","ER30189","ER30218","ER30247","ER30284","ER30314","ER30344",
    "ER30374","ER30400","ER30430","ER30464","ER30499","ER30536","ER30571",
    "ER30607","ER30643","ER30690","ER30734","ER30807",
    "ER33103","ER33203","ER33303","ER33403","ER33503","ER33603","ER33703",
    "ER33803","ER33903","ER34003","ER34103","ER34203","ER34304","ER34503",
    "ER34703","ER34903","ER35102")
)

stopifnot(all(map_int(ind_vars, length) == length(YEARS)))

# Retrospective individual variables (single code, not wave-varying)
ind_retro = c(
  sex_ind             = "ER32000",
  always_present      = "ER32001",
  sample_status       = "ER32006",
  total_births        = "ER32022",
  n_marriages         = "ER32034",
  last_marital_status = "ER32049"
)

# ---------------------------------------------------------------------------
# Step 1: Read and stack 43 family files
#
# For each wave: parse do-file → identify interview number variable (2nd var)
# → select only needed columns → read FWF → rename codes to concepts → add year
# ---------------------------------------------------------------------------

read_family_wave = function(yr) {
  paths    = fam_paths(yr)
  col_spec = parse_dofile(paths$do)

  interview_var = col_spec$name[2]   # always the 2nd variable in family files

  # PSID codes needed this wave (drop NAs, add interview_num)
  wave_codes = fam_vars |>
    filter(year == yr) |>
    select(-year) |>
    pivot_longer(everything(), names_to = "concept", values_to = "code") |>
    filter(!is.na(code)) |>
    add_row(concept = "interview_num", code = interview_var) |>
    distinct(code, .keep_all = TRUE)

  spec = col_spec |> filter(name %in% wave_codes$code)
  if (nrow(spec) == 0) { warning("No cols for year ", yr); return(NULL) }

  df = read_fwf(
    paths$txt,
    col_positions = fwf_positions(spec$start, spec$end, spec$name),
    col_types     = cols(.default = col_double()),
    show_col_types = FALSE,
    progress      = FALSE
  )

  # rename: old name (PSID code) → new name (concept)
  rename_vec = setNames(wave_codes$code, wave_codes$concept)
  rename_vec = rename_vec[rename_vec %in% names(df)]

  df |>
    rename(any_of(rename_vec)) |>
    mutate(year = yr, .before = everything())
}

message("Reading 43 family files...")
fam_panel = map_dfr(YEARS, function(yr) {
  message("  ", yr, appendLF = FALSE)
  read_family_wave(yr)
})
message("")
message(sprintf("Family panel stacked: %d rows x %d cols", nrow(fam_panel), ncol(fam_panel)))

# ---------------------------------------------------------------------------
# Step 2: Read individual file (wide)
# Parse do-file once, read only the columns we need.
# ---------------------------------------------------------------------------

message("Reading individual file...")

ind_dofile  = file.path(RAW_DIR, "ind2023er", "IND2023ER.do")
ind_txtfile = file.path(RAW_DIR, "ind2023er", "IND2023ER.txt")

ind_col_spec = parse_dofile(ind_dofile)

ind_codes_needed = unique(c(
  "ER30001", "ER30002",
  ind_vars$interview_num,
  na.omit(unlist(ind_vars |> select(-year, -interview_num))),
  unname(ind_retro)
))

ind_spec = ind_col_spec |> filter(name %in% ind_codes_needed)

ind_wide = read_fwf(
  ind_txtfile,
  col_positions = fwf_positions(ind_spec$start, ind_spec$end, ind_spec$name),
  col_types     = cols(.default = col_double()),
  show_col_types = FALSE,
  progress      = TRUE
) |>
  mutate(pid = ER30001 * 1000 + ER30002, .before = everything())

message(sprintf("Individual file: %d persons, %d cols read", nrow(ind_wide), ncol(ind_wide)))

# ---------------------------------------------------------------------------
# Step 3: Pivot individual file wide → long (person × wave)
# ---------------------------------------------------------------------------

message("Pivoting individual file to long format...")

annual_concepts = c("age_ind", "educ_ind", "emp_status", "moved_ind", "moved_yr_ind", "seq_num")

ind_long = map_dfr(seq_along(YEARS), function(i) {
  yr         = YEARS[i]
  wave_codes = ind_vars[i, ]

  row = tibble(pid = ind_wide$pid, year = yr)
  row$interview_num = ind_wide[[wave_codes$interview_num]]

  for (v in annual_concepts) {
    code    = wave_codes[[v]]
    row[[v]] = if (isTRUE(!is.na(code)) && code %in% names(ind_wide)) {
      ind_wide[[code]]
    } else {
      NA_real_
    }
  }

  row
})

message(sprintf("Individual long: %d person-year rows", nrow(ind_long)))

# ---------------------------------------------------------------------------
# Step 4: Merge
# Left join individual (long) onto family (long) on (year, interview_num).
# interview_num = 0 means not in sample → no family match → NA family vars.
# All 85,536 × 43 person-year rows are kept (NAs for absent waves).
# ---------------------------------------------------------------------------

message("Merging...")

panel = ind_long |>
  left_join(fam_panel, by = c("year", "interview_num"))

message(sprintf("Merged panel: %d rows x %d cols", nrow(panel), ncol(panel)))

# ---------------------------------------------------------------------------
# Step 5: Attach retrospective individual variables
# ---------------------------------------------------------------------------

retro_df = ind_wide |>
  select(pid, all_of(unname(ind_retro))) |>
  rename_with(
    ~ names(ind_retro)[match(., unname(ind_retro))],
    .cols = all_of(unname(ind_retro))
  )

panel = panel |> left_join(retro_df, by = "pid")

# ---------------------------------------------------------------------------
# Step 6: Recode standard PSID missing-value codes
# ---------------------------------------------------------------------------

panel = panel |>
  mutate(
    age_head            = if_else(age_head %in% c(0, 999),        NA_real_, age_head),
    age_ind             = if_else(age_ind  %in% c(0, 999),        NA_real_, age_ind),
    educ_head           = if_else(educ_head == 99,                 NA_real_, educ_head),
    educ_ind            = if_else(educ_ind  == 99,                 NA_real_, educ_ind),
    sex_head            = if_else(sex_head  %in% c(0, 9),          NA_real_, sex_head),
    house_value         = if_else(house_value %in% c(0, 9999999),  NA_real_, house_value),
    total_family_income = if_else(total_family_income < 0,         NA_real_, total_family_income),
    needs_census        = if_else(needs_census < 0,                NA_real_, needs_census)
  )

# ---------------------------------------------------------------------------
# Save as .rds and .csv.gz
# ---------------------------------------------------------------------------

rds_path = here::here("data", "psid_panel_long.rds")
csv_path = here::here("data", "psid_panel_long.csv.gz")

message("Saving .rds...")
saveRDS(panel, rds_path)

message("Saving .csv.gz...")
write_csv(panel, csv_path)

message(sprintf(
  "\nDone. %d person-year obs | %d individuals | %d variables\n  .rds: %s\n  .csv.gz: %s",
  nrow(panel), length(unique(panel$pid)), ncol(panel), rds_path, csv_path
))
