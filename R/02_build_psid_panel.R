# R/02_build_psid_panel.R
# Reshape the wide PSID extract into a long person-year panel.
#
# Input:  data/psid_wide.rds (from 01_load_psid_raw.R)
# Output: data/psid_panel.rds (person × year, long format)
#
# Strategy: define a wave crosswalk — a tribble with one row per survey wave —
# that maps each wave's PSID variable names to a common set of column names.
# Then loop over waves, select and rename, and bind_rows().
#
# Why not pivot_longer()? Because variables change names across waves and
# have no shared stem. A named crosswalk is more transparent and easier to
# audit for each dimension of the multiverse.

library(tidyverse)
library(here)

psid_wide = readRDS(here("data", "psid_wide.rds"))

# ── Wave crosswalk ────────────────────────────────────────────────────────────
#
# Each row is one survey wave. Column conventions:
#   year         Survey year (when interviews were conducted)
#   inc_year     Year the income variables refer to (usually year - 1)
#   intnum       Annual interview number (individual cross-year file) — use this
#                to link back to the PSID family files if needed later
#   seq          Sequence number (person's position in family unit)
#   rel          Relation to reference person/head
#   age_ind      Age of individual (from cross-year individual file)
#   marr_ind     Marital pairs indicator (individual file)
#   emp_ind      Employment status (individual file)
#   educ_ind     Years of education completed (individual file)
#   age_head     Age of head/reference person (family file)
#   sex_head     Sex of head/reference person
#   own_rent     Own/rent or neither (A19 in modern waves)
#   house_val    House value (A20)
#   moved        Moved since prior year/spring (A42 pre-2001, A49 2001+)
#   emp_head1    Employment status of head, 1st mention (B1/BC1)
#   wages_head   Wages and salaries of head (prior year income)
#   lab_inc_head Total labor income of head (prior year)
#   geo_mob      Geographic mobility of head/reference person
#   educ_head    Education of head completed (years)
#   educ_sp      Education of spouse/wife completed (years)
#   race_head    Race of head/reference person (1st mention)
#   state_grew   State where head grew up
#
# NA = variable not in this wave's extract (not selected at download or not
#      collected in that year). Coded NA in output after reshape.
#
# Sources: variable names verified against label variable statements in
# J362100.do (PSID Data Center, Job 362100, generated 2026-05-23).
#
# NOTE: Pre-1994 family variables use V-prefix names; ER-prefix for 1994+.
# The 1968-1978 individual cross-year variables (ER300xx) were in the first
# portion of the infix block — verify names against col_map if extending
# the crosswalk back before 1979.

waves = tribble(
  ~year, ~inc_year,
  ~intnum,    ~seq,        ~rel,        ~age_ind,    ~marr_ind,   ~emp_ind,    ~educ_ind,
  ~age_head,   ~sex_head,   ~own_rent,  ~house_val,  ~moved,
  ~emp_head1,  ~wages_head, ~lab_inc_head, ~geo_mob,
  ~educ_head,  ~educ_sp,    ~race_head,  ~state_grew,

  # ── Annual waves 1979–1993 (V-prefix family variables) ──────────────────

  1979, 1978,
  "ER30283","ER30284","ER30285","ER30286","ER30287","ER30293","ER30296",
  NA,       NA,       NA,       NA,       "V6484",
  NA,       NA,       "V6767",  "V6777",
  NA,       NA,       NA,       NA,

  1980, 1979,
  "ER30313","ER30314","ER30315","ER30316","ER30317","ER30323","ER30326",
  "V7067",  "V7068",  "V7084",  "V7517",  "V7089",
  "V7095",  "V6981",  NA,       "V7423",
  NA,       NA,       NA,       NA,

  1981, 1980,
  "ER30343","ER30344","ER30345","ER30346","ER30347","ER30353","ER30356",
  "V7658",  "V7659",  "V7675",  "V7517",  "V7700",
  "V7706",  "V7573",  NA,       "V8075",
  "V8085",  "V8086",  NA,       NA,

  1982, 1981,
  "ER30373","ER30374","ER30375","ER30376","ER30377","ER30382","ER30384",
  "V8352",  "V8353",  "V8364",  "V8217",  "V8369",
  "V8374",  "V8265",  NA,       "V8699",
  "V8709",  "V8710",  NA,       NA,

  1983, 1982,
  "ER30399","ER30400","ER30401","ER30402","ER30405","ER30411","ER30413",
  "V8961",  "V8962",  "V8974",  "V8817",  "V8999",
  "V9005",  "V8873",  NA,       "V9385",
  "V9395",  "V9396",  NA,       NA,

  1984, 1983,
  "ER30429","ER30430","ER30431","ER30432","ER30435","ER30441","ER30443",
  "V10419", "V10420", "V10437", "V10018", "V10447",
  "V10453", "V10256", NA,       "V11032",
  "V11042", "V11043", NA,       NA,

  1985, 1984,
  "ER30463","ER30464","ER30465","ER30466","ER30469","ER30474","ER30478",
  "V11606", "V11607", "V11618", "V11125", "V11628",
  "V11637", "V11397", NA,       "V12386",
  "V12400", "V12401", NA,       NA,

  1986, 1985,
  "ER30498","ER30499","ER30500","ER30501","ER30504","ER30509","ER30513",
  "V13011", "V13012", "V13023", "V12524", "V13037",
  "V13046", "V12796", NA,       "V13636",
  "V13640", "V13641", NA,       "V13542",

  1987, 1986,
  "ER30535","ER30536","ER30537","ER30538","ER30541","ER30545","ER30549",
  "V14114", "V14115", "V14126", "V13724", "V14140",
  "V14146", "V13898", NA,       "V14683",
  "V14687", "V14688", NA,       "V14589",

  1988, 1987,
  "ER30570","ER30571","ER30572","ER30573","ER30576","ER30580","ER30584",
  "V15130", "V15131", "V15140", "V14824", "V15148",
  "V15154", "V14913", NA,       "V16157",
  "V16161", "V16162", NA,       "V16063",

  1989, 1988,
  "ER30606","ER30607","ER30608","ER30609","ER30612","ER30616","ER30620",
  "V16631", "V16632", "V16641", "V16324", "V16649",
  "V16655", "V16413", NA,       "V17543",
  "V17545", "V17546", NA,       "V17460",

  1990, 1989,
  "ER30642","ER30643","ER30644","ER30645","ER30648","ER30653","ER30657",
  "V18049", "V18050", "V18072", "V17724", "V18087",
  "V18093", "V17829", NA,       "V18894",
  "V18898", "V18899", NA,       "V18791",

  1991, 1990,
  "ER30689","ER30690","ER30691","ER30692","ER30695","ER30699","ER30703",
  "V19349", "V19350", "V19372", "V19024", "V19387",
  "V19393", "V19129", NA,       "V20194",
  "V20198", "V20199", NA,       "V20091",

  1992, 1991,
  "ER30733","ER30734","ER30735","ER30736","ER30739","ER30744","ER30748",
  "V20651", "V20652", "V20672", "V20324", "V20687",
  "V20693", "V20429", NA,       "V21500",
  "V21504", "V21505", NA,       "V21397",

  1993, 1992,
  "ER30806","ER30807","ER30808","ER30809","ER30812","ER30816","ER30820",
  "V22406", "V22407", "V22427", "V21610", "V22441",
  "V22448", "V21739", NA,       "V23332",
  "V23333", "V23334", NA,       "V23254",

  # ── Annual waves 1994–1997 (ER-prefix family variables) ─────────────────

  1994, 1993,
  "ER33101","ER33102","ER33103","ER33104","ER33107","ER33111","ER33115",
  "ER2007",  "ER2008",  "ER2032",  "ER2033",  "ER2062",
  "ER2068",  "ER4122",  "ER4140",  "ER4157D",
  "ER4158",  "ER4159",  "ER3944",  "ER3919A",

  1995, 1994,
  "ER33201","ER33202","ER33203","ER33204","ER33207","ER33211","ER33215",
  "ER5006",  "ER5007",  "ER5031",  "ER5032",  "ER5061",
  "ER5067",  "ER6962",  "ER6980",  "ER6997D",
  "ER6998",  "ER6999",  "ER6814",  "ER6789A",

  1996, 1995,
  "ER33301","ER33302","ER33303","ER33304","ER33307","ER33311","ER33315",
  "ER7006",  "ER7007",  "ER7031",  "ER7032",  "ER7155",
  "ER7163",  "ER9213",  "ER9231",  "ER9248D",
  "ER9249",  "ER9250",  "ER9060",  "ER9035A",

  1997, 1996,
  "ER33401","ER33402","ER33403","ER33404","ER33407","ER33411","ER33415",
  "ER10009", "ER10010", "ER10035", "ER10036", "ER10072",
  "ER10081", "ER12196", "ER12080", "ER12221D",
  "ER12222", "ER12223", "ER11848", "ER11842",

  # ── Biennial waves 1999–2023 ─────────────────────────────────────────────
  # "moved" question changed from A42 (spring) to A49 (Jan 1) starting 2001.
  # "wife" relabeled "spouse" in 2013, "reference person" replaces "head" 2015+.
  # Coding of all three variables is comparable across waves for most purposes.

  1999, 1998,
  "ER33501","ER33502","ER33503","ER33504","ER33507","ER33512","ER33516",
  "ER13010", "ER13011", "ER13040", "ER13041", "ER13077",
  "ER13205", "ER16493", "ER16463", "ER16431B",
  "ER16516", "ER16517", "ER15928", "ER15922",

  2001, 2000,
  "ER33601","ER33602","ER33603","ER33604","ER33607","ER33612","ER33616",
  "ER17013", "ER17014", "ER17043", "ER17044", "ER17088",
  "ER17216", "ER20425", "ER20443", "ER20377B",
  "ER20457", "ER20458", "ER19989", "ER19983",

  2003, 2002,
  "ER33701","ER33702","ER33703","ER33704","ER33707","ER33712","ER33716",
  "ER21017", "ER21018", "ER21042", "ER21043", "ER21117",
  "ER21123", "ER24117", "ER24116", "ER24147",
  "ER24148", "ER24149", "ER23426", "ER23420",

  2005, 2004,
  "ER33801","ER33802","ER33803","ER33804","ER33807","ER33813","ER33817",
  "ER25017", "ER25018", "ER25028", "ER25029", "ER25098",
  "ER25104", "ER27913", "ER27931", "ER28046",
  "ER28047", "ER28048", "ER27393", "ER27386",

  2007, 2006,
  "ER33901","ER33902","ER33903","ER33904","ER33907","ER33913","ER33917",
  "ER36017", "ER36018", "ER36028", "ER36029", "ER36103",
  "ER36109", "ER40903", "ER40921", "ER41036",
  "ER41037", "ER41038", "ER40565", "ER40561",

  2009, 2008,
  "ER34001","ER34002","ER34003","ER34004","ER34007","ER34016","ER34020",
  "ER42017", "ER42018", "ER42029", "ER42030", "ER42132",
  "ER42140", "ER46811", "ER46829", "ER46978",
  "ER46981", "ER46982", "ER46543", "ER46538",

  2011, 2010,
  "ER34101","ER34102","ER34103","ER34104","ER34107","ER34116","ER34119",
  "ER47317", "ER47318", "ER47329", "ER47330", "ER47440",
  "ER47448", "ER52219", "ER52237", "ER52402",
  "ER52405", "ER52406", "ER51904", "ER51899",

  2013, 2012,
  "ER34201","ER34202","ER34203","ER34204","ER34207","ER34216","ER34230",
  "ER53017", "ER53018", "ER53029", "ER53030", "ER53140",
  "ER53148", "ER58020", "ER58038", "ER58220",
  "ER58223", "ER58224", "ER57659", "ER57654",

  2015, 2014,
  "ER34301","ER34302","ER34303","ER34305","ER34308","ER34317","ER34349",
  "ER60017", "ER60018", "ER60030", "ER60031", "ER60155",
  "ER60163", "ER65200", "ER65216", "ER65456",
  "ER65459", "ER65460", "ER64810", "ER64805",

  2017, 2016,
  "ER34501","ER34502","ER34503","ER34504","ER34507","ER34516","ER34548",
  "ER66017", "ER66018", "ER66030", "ER66031", "ER66156",
  "ER66164", "ER71277", "ER71293", "ER71535",
  "ER71538", "ER71539", "ER70882", "ER70877",

  2019, 2018,
  "ER34701","ER34702","ER34703","ER34704","ER34707","ER34716","ER34752",
  "ER72017", "ER72018", "ER72030", "ER72031", "ER72156",
  "ER72164", "ER77299", "ER77315", "ER77596",
  "ER77599", "ER77600", "ER76897", "ER76892",

  2021, 2020,
  "ER34901","ER34902","ER34903","ER34904","ER34907","ER34916","ER34952",
  "ER78017", "ER78018", "ER78031", "ER78032", "ER78158",
  "ER78167", "ER81626", "ER81642", "ER81923",
  "ER81926", "ER81927", "ER81144", "ER81139",

  2023, 2022,
  "ER35101","ER35102","ER35103","ER35104","ER35107","ER35116","ER35152",
  "ER82018", "ER82019", "ER82032", "ER82033", "ER82141",
  "ER82150", "ER85480", "ER85496", "ER85777",
  "ER85780", "ER85781", "ER85121", "ER85116"
)

# ── Build the long panel ──────────────────────────────────────────────────────
#
# For each wave: (1) select the variables listed in the crosswalk,
# (2) rename them to common names, (3) add year/inc_year columns.
# Bind all waves together.
#
# Variables present in the extract but not in the crosswalk above (e.g.,
# parental education, veteran status) stay in psid_wide and can be joined
# back by pid after this step if needed.

all_vars = names(waves)[-(1:2)]  # conceptual column names

build_wave = function(row, wide) {
  yr     = row$year
  varmap = unlist(row[all_vars])     # named vector: concept -> PSID varname
  present = varmap[!is.na(varmap)]   # drop NA entries (var not in extract)
  present = present[present %in% names(wide)]  # guard against typos

  if (length(present) == 0) return(NULL)

  wide |>
    select(pid, all_of(unname(present))) |>
    set_names(c("pid", names(present))) |>
    mutate(year = yr, inc_year = row$inc_year, .after = pid)
}

panel_list = waves |>
  rowwise() |>
  group_split() |>
  map(\(row) build_wave(as.list(row), psid_wide), .progress = TRUE)

psid_panel = bind_rows(panel_list)

message("Panel dimensions: ", nrow(psid_panel), " rows × ", ncol(psid_panel), " columns")
# Expected: up to 85,536 persons × 29 waves = ~2.5M rows (fewer because not
# all persons are observed in all waves).

# ── Recode PSID missing-value codes ─────────────────────────────────────────
#
# PSID encodes item non-response and inapplicability with large integers whose
# exact values depend on the variable. These are documented in J362100_formats.do
# and the codebook. Common patterns:
#
#   9 / 99 / 999 / 9999 = NA/DK/refused (for short variables)
#   0                   = inapplicable (e.g., no spouse in unit)
#   98 / 9998           = DK
#
# The safest approach is to recode per-variable after consulting the codebook.
# Below are recodes for the most commonly used variables. Extend as needed.

psid_panel = psid_panel |>
  mutate(
    # intnum = 0 means person not in sample that wave
    intnum = na_if(intnum, 0L),

    # own_rent: 8 = neither owns nor rents, 9 = NA/DK/refused
    own_rent = case_when(
      own_rent %in% c(8L, 9L) ~ NA_integer_,
      .default = own_rent
    ),

    # house_val: 0 = inap (rents/NA), 9999999 = DK/refused (varies by wave)
    house_val = if_else(house_val == 0L, NA_integer_, house_val),

    # moved: 5 = No, 1 = Yes (pre-2001 A42); 1 = Yes, 5 = No (A49 2001+)
    # Coding is consistent: 1 = moved, 5 = did not move, 8/9 = DK/NA
    moved = case_when(
      moved %in% c(8L, 9L) ~ NA_integer_,
      moved == 1L           ~ 1L,
      moved == 5L           ~ 0L,
      .default              = NA_integer_
    ),

    # wages_head and lab_inc_head: 9999999 = DK/refused (cap at survey max)
    # Do NOT recode here without checking the specific wave's max valid value.

    # geo_mob: PSID geographic mobility codes vary by wave — consult codebook
    # before recoding. Common: 1 = same house, 5 = moved within county, etc.
    # Left as-is; recode in analysis scripts once coding is verified.

    # race_head: 1 = White, 2 = Black/AA, 3 = Am. Indian/Alaska Native,
    #            4 = Asian, 5 = Other, 7 = NA (pre-2003 coding differs)
    race_head = if_else(race_head %in% c(0L, 9L), NA_integer_, race_head)
  )

# ── Save ──────────────────────────────────────────────────────────────────────
saveRDS(psid_panel, here("data", "psid_panel.rds"))
message("Saved panel to: ", here("data", "psid_panel.rds"))
