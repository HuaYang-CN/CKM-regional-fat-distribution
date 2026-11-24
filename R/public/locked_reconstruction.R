# Official NHANES snapshot validation and scientific derivations.
# The wrapper manages cache and outputs.
CYCLES <- data.frame(
  cycle = c("2011-2012", "2013-2014", "2015-2016", "2017-2018"),
  begin_year = c(2011L, 2013L, 2015L, 2017L), suffix = c("G", "H", "I", "J"),
  release_code = 7:10, official_n = c(9756L, 10175L, 9971L, 9254L),
  seqn_min = c(62161L, 73557L, 83732L, 93703L),
  seqn_max = c(71916L, 83731L, 93702L, 102956L), stringsAsFactors = FALSE)

REQUIRED_VARS <- list(
  DEMO = c("SEQN", "RIDAGEYR", "RIAGENDR", "RIDRETH3", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "SDDSRVYR", "RIDSTATR"),
  BMX = c("SEQN", "BMXBMI", "BMXWAIST"),
  DXX = c("SEQN", "DXDTRPF", "DXDLAPF", "DXDRAPF", "DXDLLPF", "DXDRLPF"),
  DXXAG = c("SEQN", "DXXVFATA"),
  BPX = c("SEQN", "BPXSY1", "BPXDI1", "BPXSY2", "BPXDI2", "BPXSY3", "BPXDI3", "BPXSY4", "BPXDI4"),
  BPQ = c("SEQN", "BPQ020", "BPQ050A"),
  DIQ = c("SEQN", "DIQ010", "DIQ050", "DIQ070", "DIQ160"),
  SMQ = c("SEQN", "SMQ020", "SMQ040"),
  MCQ = c("SEQN", "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", "MCQ160F"),
  RXQ_RX = c("SEQN", "RXDDRUG"), GHB = c("SEQN", "LBXGH"),
  GLU = c("SEQN", "WTSAF2YR", "LBXGLU"), HDL = c("SEQN", "LBDHDD"),
  TCHOL = c("SEQN", "LBXTC"), TRIGLY = c("SEQN", "WTSAF2YR", "LBXTR"),
  BIOPRO = c("SEQN", "LBXSCR"), ALB_CR = c("SEQN", "URDACT"))
PCA_VARS <- c("BMI", "VAT_area", "Trunk_pct", "LA_pct", "RA_pct", "LL_pct", "RL_pct")

read_xpt_upper <- function(path) {
  x <- haven::read_xpt(path)
  names(x) <- toupper(names(x))
  x
}


validate_xpt <- function(path, stub, cy, require_snapshot = TRUE) {
  if (!file.exists(path)) stop("File not found: ", basename(path))
  con <- file(path, "rb"); header <- readBin(con, what = "raw", n = 80L); close(con)
  if (length(header) < 80L || !startsWith(rawToChar(header), "HEADER RECORD")) stop("Not an XPT file: ", basename(path))
  x <- read_xpt_upper(path)
  missing <- setdiff(REQUIRED_VARS[[stub]], names(x))
  if (length(missing)) stop("Missing columns in ", basename(path), ": ", paste(missing, collapse = ", "))
  numeric_cols <- setdiff(REQUIRED_VARS[[stub]], "RXDDRUG")
  if (!all(vapply(x[numeric_cols], is.numeric, logical(1)))) stop("Expected raw numeric codes; translated or malformed data in ", basename(path))
  if (!nrow(x) || anyNA(x$SEQN) || any(!is.finite(x$SEQN)) || any(x$SEQN != floor(x$SEQN))) stop("Invalid participant IDs in ", basename(path))
  if (any(x$SEQN < cy$seqn_min | x$SEQN > cy$seqn_max)) stop("Wrong-cycle IDs in ", basename(path))
  if (stub != "RXQ_RX" && anyDuplicated(x$SEQN)) stop("Duplicate participant IDs in ", basename(path), "; no arbitrary deduplication performed.")
  if (stub == "DEMO") {
    expected_ids <- seq.int(cy$seqn_min, cy$seqn_max)
    if (nrow(x) != cy$official_n || !setequal(x$SEQN, expected_ids)) stop("Incomplete or noncanonical DEMO_", cy$suffix,
      ": found ", nrow(x), " rows; official file requires ", cy$official_n, " exact IDs.")
    if (anyNA(x$SDDSRVYR) || any(x$SDDSRVYR != cy$release_code)) stop("DEMO release code does not match its cycle.")
    if (anyNA(x$RIDAGEYR) || any(!is.finite(x$RIDAGEYR)) || any(x$RIDAGEYR < 0 | x$RIDAGEYR > 80)) stop("Invalid demographic ages.")
  }
  md5 <- unname(tools::md5sum(path))
  expected <- OFFICIAL_SNAPSHOT[OFFICIAL_SNAPSHOT$dataset == paste0(stub, "_", cy$suffix), ]
  if (require_snapshot && (nrow(expected) != 1L || md5 != expected$md5)) stop("File bytes differ from the verified 2026-10-19 CDC snapshot: ", basename(path),
    ". Do not accept a stale, partial or newly revised file without checking its provenance.")
  list(data = x, md5 = md5, bytes = unname(file.info(path)$size))
}


OFFICIAL_SNAPSHOT <- utils::read.csv(text = "dataset,md5,bytes
ALB_CR_G,d456924fffb6c84dc3b72eeff9e1a97b,377040
ALB_CR_H,c933991e58df7766ea1e3aa3b186b445,399600
ALB_CR_I,92a4e6ee3736575b64247a41060be9d0,552800
ALB_CR_J,8b23800e269697294707d88d9e03f3af,509760
BIOPRO_G,d2973177b945bbb0306fc5692b528801,1997040
BIOPRO_H,1039d447d520cb094e86eda1c3c99271,2127760
BIOPRO_I,f79720ea4aa861535f215730c66fd520,2056320
BIOPRO_J,1c1ab70913acc8233ad1d23fd95f89da,2106080
BMX_G,b00e503455cf22a080b1de168673c74e,1946720
BMX_H,c5bc2b552c8cd33f10557fd16b747920,2045520
BMX_I,d40ac66541b94ed27ec609e55211871a,1989600
BMX_J,4c51e08c3b105005a326da3a4fc6b223,1466000
BPQ_G,6bd9225ce7b707a031f1dbe31d4127f2,743920
BPQ_H,8b6463e652049319381be8cb056462d3,726720
BPQ_I,618e1b3dcb9e964aeb72b564d93c90d8,559120
BPQ_J,73389571d1c6c3042b233b0277e4bcb3,544560
BPX_G,58578da834609248e9c248495291a5b1,2021600
BPX_H,11381ab93e54c38b16ebd5955c2832d4,1809600
BPX_I,ea61a348c6baff8aa2892a021cdf4e87,1607120
BPX_J,6785e6261dffcb7c060c87e520fd8df8,1466000
DEMO_G,f4b048688d73dd5ef8d167526e888b56,3753760
DEMO_H,fe59a58ba1a5cc4814d7e6db4ebe820e,3833200
DEMO_I,7c3bcc3d4dd0adf238b33016b9692ef0,3756480
DEMO_J,3d5e48e4255b2ec80578a0eb45480d4c,3412720
DIQ_G,ffba08e08a59907aa7405b2d6e2a3e3a,3978560
DIQ_H,e3e54401502f4d29f8649764268571e6,4228960
DIQ_I,428a382a815c10f2e11db97f0f054c9b,4144720
DIQ_J,582c75421372266d1591162511ea73cb,3851840
DXXAG_G,24ad83cee53c3d93f7998166a65b6eb6,1051360
DXXAG_H,97064f074317b75b89b6b6d7bd28e07c,1114080
DXXAG_I,b2f1fc6fe784cedcf256b9d44450570f,1067520
DXXAG_J,a153f45274dd1514765651a83f9db8bd,945040
DXX_G,7e0574927daaa4e539a1627899fa0f5f,4248640
DXX_H,35b1124157cfd9951654137c1745998e,4502320
DXX_I,949086325613998fda20367907decb6c,4314080
DXX_J,e5f5314fec2c12b3767877d805cd4e1d,3818640
GHB_G,b14501f6d1357b2a3272db3b471be485,105840
GHB_H,86ba74cc83143f1e9ea3643ae2a5b1da,112720
GHB_I,c5fd828c4ac9c25159959337dbffd9a0,108960
GHB_J,3ed004ca802328e9de20bbc1b9091ae6,103520
GLU_G,a9cb7c8c50e3d40cd9d28bf423c84791,209200
GLU_H,6c34bf73c1e46416179af13718c93012,161440
GLU_I,df47783670402deb15069b48bcc92ca8,103440
GLU_J,de67c461f2688d4b6e2e5e2b17d27e5c,98480
HDL_G,9586d1c37707acd1447474aa410be64c,188960
HDL_H,6d6b89a5bd0d5628860b8ca0f7421867,200240
HDL_I,738ed038de50c634d246a09e4781f087,193760
HDL_J,7ceebc13b2db63ee7955b11b661cab13,179680
MCQ_G,1d12338ae6df629d5171918e24c90679,6905520
MCQ_H,9b2968321f390943fd7cb6b074b8a503,7439280
MCQ_I,aa003bf7879b46fd2a42145518ab3e77,6907360
MCQ_J,15eb9d220ee181b319b633064ebff760,5420800
RXQ_RX_G,9453938ceb9744e46630e0a66f587440,2994400
RXQ_RX_H,0b8a12d67db31632bb1a3a29d302c829,9897680
RXQ_RX_I,eb2b43ff7557bf7edc17dc9b58858e72,9629600
RXQ_RX_J,27f7fd5736b96386a29ee51e01418219,9627680
SMQ_G,d6255f3d78b1aff348ce4167fe2c0b65,1946960
SMQ_H,4e4ea631cc9d9672a6bc9c30d0986082,2170000
SMQ_I,59978e837bf7f37c550a4bed965e6835,2681040
SMQ_J,2a4fb99269a678cfc9ea7b0bd77a6eae,2305600
TCHOL_G,a56972222c8a0e76d9a33d69e24c714e,188960
TCHOL_H,f9c6a765433bdad4f2a229fdc86bbb92,200240
TCHOL_I,891377b3a3e6ce0b6f38ab237e7eef3a,193760
TCHOL_J,8a66907b0f4d0173872a69de2b71a45d,179680
TRIGLY_G,2f18536d5ef4bd3cd6ec333ee0fd047a,157120
TRIGLY_H,6826ed9932fe7ea0c5c3f0058a8238d2,161440
TRIGLY_I,ce87f1a0d64a73ab6af7cd6a0680c62a,154800
TRIGLY_J,511c95235354c8325f704150adccee92,245040
", stringsAsFactors = FALSE)

# =============================================================================
# METHOD PROVENANCE
# Adapted from HuaYang-CN/CKM-regional-fat-distribution, release v1.0.2.
# 07a SHA256: 851d3443542a2da91e4dad66ac37342415caebfa8692fdd6834909a568796f10
# 07b SHA256: 2867b270789016e7b64ac14cda54b3de9f7bb9cff5c119b7dcf828878ba5d21d
# Harmonization and revised CKM criteria below retain that release's definitions.
# Changes: explicit raw directory; checked joins; PREVENT output validation;
# no initial/clamped staging; no global rm(), source(), install(), or old outputs.
# PREVENT package for this audit: 0.12.0, an explicit replication choice.
# The original analysis sessionInfo did not record its PREVENT package version.
# =============================================================================

# MIT License
# Copyright (c) 2026 HuaYang-CN
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

yes_no_from_code <- function(x) {
  case_when(
    x == 1 ~ TRUE,
    x == 2 ~ FALSE,
    TRUE ~ NA
  )
}

row_mean_positive <- function(dat, vars) {
  m <- as.matrix(dat[, vars, drop = FALSE])
  m[m <= 0] <- NA_real_
  out <- rowMeans(m, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

clamp <- function(x, lo, hi) {
  pmin(pmax(x, lo), hi)
}

calc_egfr_2021 <- function(scr_mg_dl, age, female) {
  kappa <- ifelse(female, 0.7, 0.9)
  alpha <- ifelse(female, -0.241, -0.302)

  142 *
    pmin(scr_mg_dl / kappa, 1)^alpha *
    pmax(scr_mg_dl / kappa, 1)^(-1.200) *
    (0.9938^age) *
    ifelse(female, 1.012, 1)
}

kdigo_risk <- function(egfr, uacr) {
  case_when(
    is.na(egfr) | is.na(uacr) ~ NA_character_,

    # G1/G2
    egfr >= 60 & uacr < 30 ~ "low",
    egfr >= 60 & uacr >= 30 & uacr < 300 ~ "moderate",
    egfr >= 60 & uacr >= 300 ~ "high",

    # G3a
    egfr >= 45 & egfr < 60 & uacr < 30 ~ "moderate",
    egfr >= 45 & egfr < 60 & uacr >= 30 & uacr < 300 ~ "high",
    egfr >= 45 & egfr < 60 & uacr >= 300 ~ "very_high",

    # G3b
    egfr >= 30 & egfr < 45 & uacr < 30 ~ "high",
    egfr >= 30 & egfr < 45 & uacr >= 30 ~ "very_high",

    # G4/G5
    egfr < 30 ~ "very_high",

    TRUE ~ NA_character_
  )
}

harmonize_cycle <- function(cycle_label, suffix, raw_dir, audit_state) {
  dataset_path <- function(stub, sf) file.path(raw_dir, paste0(stub, "_", sf, ".XPT"))
  checked_join <- function(x, y, by) {
    if (anyDuplicated(x$SEQN) || anyDuplicated(y$SEQN))
      stop("Join is not one-to-one in ", cycle_label, ": ", paste(names(y), collapse = ","))
    before <- nrow(x)
    z <- dplyr::left_join(x, y, by = by, relationship = "one-to-one")
    stopifnot(nrow(z) == before, identical(z$SEQN, x$SEQN))
    audit_state$joins[[length(audit_state$joins) + 1L]] <- data.frame(
      cycle = cycle_label, joined_fields = paste(setdiff(names(y), "SEQN"), collapse = ";"),
      n_before = before, n_after = nrow(z), n_source = nrow(y),
      n_matched_ids = sum(x$SEQN %in% y$SEQN), stringsAsFactors = FALSE)
    z
  }

  message("Harmonizing ", cycle_label, " ...")

  demo   <- read_xpt_upper(dataset_path("DEMO", suffix))
  bmx    <- read_xpt_upper(dataset_path("BMX", suffix))
  dxx    <- read_xpt_upper(dataset_path("DXX", suffix))
  dxxag  <- read_xpt_upper(dataset_path("DXXAG", suffix))
  bpx    <- read_xpt_upper(dataset_path("BPX", suffix))
  bpq    <- read_xpt_upper(dataset_path("BPQ", suffix))
  diq    <- read_xpt_upper(dataset_path("DIQ", suffix))
  smq    <- read_xpt_upper(dataset_path("SMQ", suffix))
  mcq    <- read_xpt_upper(dataset_path("MCQ", suffix))
  rx     <- read_xpt_upper(dataset_path("RXQ_RX", suffix))
  ghb    <- read_xpt_upper(dataset_path("GHB", suffix))
  glu    <- read_xpt_upper(dataset_path("GLU", suffix))
  hdl    <- read_xpt_upper(dataset_path("HDL", suffix))
  tchol  <- read_xpt_upper(dataset_path("TCHOL", suffix))
  trigly <- read_xpt_upper(dataset_path("TRIGLY", suffix))
  bio    <- read_xpt_upper(dataset_path("BIOPRO", suffix))
  albcr  <- read_xpt_upper(dataset_path("ALB_CR", suffix))

  # ---- statin indicator from generic drug names ----
  statin_regex <- paste(
    c(
      "atorvastatin",
      "fluvastatin",
      "lovastatin",
      "pitavastatin",
      "pravastatin",
      "rosuvastatin",
      "simvastatin",
      "cerivastatin"
    ),
    collapse = "|"
  )

  statin_tbl <- rx %>%
    transmute(
      SEQN,
      drug = tolower(as.character(RXDDRUG))
    ) %>%
    group_by(SEQN) %>%
    summarise(
      statin = any(str_detect(drug, statin_regex), na.rm = TRUE),
      .groups = "drop"
    )

  # Keep only one fasting weight source and verify against TRIGLY.
  glu_small <- glu %>%
    select(SEQN, WTSAF2YR, LBXGLU)

  trigly_small <- trigly %>%
    select(SEQN, WTSAF2YR_TRIG = WTSAF2YR, LBXTR)

  out <- demo %>%
    select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3,
           WTMEC2YR, SDMVPSU, SDMVSTRA) %>%

    checked_join(
      bmx %>% select(SEQN, BMXBMI, BMXWAIST),
      by = "SEQN"
    ) %>%

    checked_join(
      dxx %>% select(
        SEQN, DXDTRPF, DXDLAPF, DXDRAPF, DXDLLPF, DXDRLPF
      ),
      by = "SEQN"
    ) %>%

    checked_join(
      dxxag %>% select(SEQN, DXXVFATA),
      by = "SEQN"
    ) %>%

    checked_join(
      bpx %>% select(
        SEQN,
        BPXSY1, BPXDI1, BPXSY2, BPXDI2,
        BPXSY3, BPXDI3, BPXSY4, BPXDI4
      ),
      by = "SEQN"
    ) %>%

    checked_join(
      bpq %>% select(SEQN, BPQ020, BPQ050A),
      by = "SEQN"
    ) %>%

    checked_join(
      diq %>% select(SEQN, DIQ010, DIQ050, DIQ070, DIQ160),
      by = "SEQN"
    ) %>%

    checked_join(
      smq %>% select(SEQN, SMQ020, SMQ040),
      by = "SEQN"
    ) %>%

    checked_join(
      mcq %>% select(
        SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F
      ),
      by = "SEQN"
    ) %>%

    checked_join(statin_tbl, by = "SEQN") %>%

    checked_join(
      ghb %>% select(SEQN, LBXGH),
      by = "SEQN"
    ) %>%

    checked_join(glu_small, by = "SEQN") %>%

    checked_join(
      hdl %>% select(SEQN, LBDHDD),
      by = "SEQN"
    ) %>%

    checked_join(
      tchol %>% select(SEQN, LBXTC),
      by = "SEQN"
    ) %>%

    checked_join(trigly_small, by = "SEQN") %>%

    checked_join(
      bio %>% select(SEQN, LBXSCR),
      by = "SEQN"
    ) %>%

    checked_join(
      albcr %>% select(SEQN, URDACT),
      by = "SEQN"
    ) %>%

    mutate(
      cycle = cycle_label,
      cycle_suffix = suffix,
      statin = coalesce(statin, FALSE)
    )

  # Fasting weights should agree where both are nonmissing.
  bad_weight <- out %>%
    filter(
      !is.na(WTSAF2YR),
      !is.na(WTSAF2YR_TRIG),
      abs(WTSAF2YR - WTSAF2YR_TRIG) > 1e-6
    ) %>%
    nrow()

  if (bad_weight > 0) {
    warning(cycle_label, ": fasting weights differ between GLU and TRIGLY for ",
            bad_weight, " rows. WTSAF2YR from GLU is retained.")
  }

  if (bad_weight > 0L) stop("GLU/TRIGLY fasting weights disagree; investigate input versions before analysis.")
  stopifnot(nrow(out) == nrow(demo), setequal(out$SEQN, demo$SEQN))
  out
}

derive_harmonized_variables <- function(nhanes_all) {
nhanes_all <- nhanes_all %>%
  mutate(
    age = RIDAGEYR,
    sex = case_when(
      RIAGENDR == 1 ~ "male",
      RIAGENDR == 2 ~ "female",
      TRUE ~ NA_character_
    ),
    female = sex == "female",
    non_hispanic_asian = RIDRETH3 == 6,

    # Adiposity variables that will later enter the external PCA
    BMI = BMXBMI,
    waist_cm = BMXWAIST,
    VAT_area = DXXVFATA,
    Trunk_pct = DXDTRPF,
    LA_pct = DXDLAPF,
    RA_pct = DXDRAPF,
    LL_pct = DXDLLPF,
    RL_pct = DXDRLPF,

    fasting_glucose = LBXGLU,
    hba1c = LBXGH,
    triglycerides = LBXTR,
    hdl = LBDHDD,
    total_chol = LBXTC,
    serum_creatinine = LBXSCR,
    uacr = URDACT
  )

nhanes_all$sbp <- row_mean_positive(
  nhanes_all,
  c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")
)

nhanes_all$dbp <- row_mean_positive(
  nhanes_all,
  c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
)

nhanes_all <- nhanes_all %>%
  mutate(
    # Hypertension treatment:
    # if medication question is skipped because participant was never told HBP,
    # treat as FALSE; otherwise retain NA for unresolved responses.
    bp_tx = case_when(
      BPQ050A == 1 ~ TRUE,
      BPQ050A == 2 ~ FALSE,
      BPQ020 == 2 ~ FALSE,
      TRUE ~ NA
    ),

    self_report_htn = case_when(
      BPQ020 == 1 ~ TRUE,
      BPQ020 == 2 ~ FALSE,
      TRUE ~ NA
    ),

    self_report_dm = case_when(
      DIQ010 == 1 ~ TRUE,
      DIQ010 %in% c(2, 3) ~ FALSE,
      TRUE ~ NA
    ),

    insulin_use = case_when(
      DIQ050 == 1 ~ TRUE,
      DIQ050 == 2 ~ FALSE,
      DIQ010 %in% c(2, 3) ~ FALSE,
      TRUE ~ NA
    ),

    diabetes_pills = case_when(
      DIQ070 == 1 ~ TRUE,
      DIQ070 == 2 ~ FALSE,
      DIQ010 %in% c(2, 3) ~ FALSE,
      TRUE ~ NA
    ),

    self_report_prediabetes = case_when(
      DIQ160 == 1 ~ TRUE,
      DIQ160 == 2 ~ FALSE,
      DIQ010 == 1 ~ FALSE,
      TRUE ~ NA
    ),

    current_smoker = case_when(
      SMQ020 == 2 ~ FALSE,
      SMQ020 == 1 & SMQ040 %in% c(1, 2) ~ TRUE,
      SMQ020 == 1 & SMQ040 == 3 ~ FALSE,
      TRUE ~ NA
    ),

    clinical_cvd = case_when(
      MCQ160B == 1 | MCQ160C == 1 | MCQ160D == 1 |
        MCQ160E == 1 | MCQ160F == 1 ~ TRUE,

      MCQ160B == 2 & MCQ160C == 2 & MCQ160D == 2 &
        MCQ160E == 2 & MCQ160F == 2 ~ FALSE,

      TRUE ~ NA
    ),

    egfr = calc_egfr_2021(
      scr_mg_dl = serum_creatinine,
      age = age,
      female = female
    ),

    kdigo = kdigo_risk(egfr, uacr)
  )
  nhanes_all
}


derive_revised_staging <- function(nhanes_all, risk_function = preventr::estimate_risk) {
nhanes_30_59 <- nhanes_all %>%
  filter(age >= 30, age <= 59)

message("Age 30-59 cohort: n = ", nrow(nhanes_30_59))


# ==============================================================================
# 5. RE-DERIVE DIABETES
# ==============================================================================

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    diabetes = case_when(
      self_report_dm %in% TRUE ~ TRUE,
      insulin_use %in% TRUE ~ TRUE,
      diabetes_pills %in% TRUE ~ TRUE,

      !is.na(fasting_glucose) &
        fasting_glucose >= 126 ~ TRUE,

      !is.na(hba1c) &
        hba1c >= 6.5 ~ TRUE,

      self_report_dm %in% FALSE &
        insulin_use %in% FALSE &
        diabetes_pills %in% FALSE &
        !is.na(fasting_glucose) &
        !is.na(hba1c) ~ FALSE,

      TRUE ~ NA
    )
  )


# ==============================================================================
# 6. REVISED PRIMARY PREVENT ELIGIBILITY
#
# Key revision:
#   - No clamping is used in the PRIMARY analysis.
#   - PREVENT is calculated only among participants without clinical CVD.
#   - Participants with established clinical CVD can be Stage 4 directly and
#     do not need a PREVENT result.
#
# The limits below match the ranges accepted by the preventr implementation
# used in the original pipeline.
# ==============================================================================

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    prevent_no_cvd =
      clinical_cvd %in% FALSE,

    prevent_inputs_complete =
      complete.cases(
        age,
        sex,
        sbp,
        bp_tx,
        total_chol,
        hdl,
        statin,
        diabetes,
        current_smoker,
        egfr,
        BMI
      ),

    prevent_sbp_in_range =
      !is.na(sbp) &
      sbp >= 90 &
      sbp <= 180,

    prevent_total_c_in_range =
      !is.na(total_chol) &
      total_chol >= 130 &
      total_chol <= 320,

    prevent_hdl_in_range =
      !is.na(hdl) &
      hdl >= 20 &
      hdl <= 100,

    prevent_egfr_in_range =
      !is.na(egfr) &
      egfr >= 15 &
      egfr <= 140,

    prevent_bmi_in_range =
      !is.na(BMI) &
      BMI >= 18.5 &
      BMI <= 39.9,

    prevent_inputs_in_range =
      prevent_sbp_in_range &
      prevent_total_c_in_range &
      prevent_hdl_in_range &
      prevent_egfr_in_range &
      prevent_bmi_in_range,

    prevent_eligible_primary =
      prevent_no_cvd &
      prevent_inputs_complete &
      prevent_inputs_in_range
  )


# ==============================================================================
# 7. CALCULATE PRIMARY PREVENT RISK WITHOUT CLAMPING
# ==============================================================================

prevent_input_primary <- nhanes_30_59 %>%
  filter(prevent_eligible_primary) %>%
  transmute(
    SEQN,
    age = age,
    sex = sex,
    sbp = sbp,
    bp_tx = bp_tx,
    total_c = total_chol,
    hdl_c = hdl,
    statin = statin,
    dm = diabetes,
    smoking = current_smoker,
    egfr = egfr,
    bmi = BMI
  )

if (nrow(prevent_input_primary) == 0) {
  stop(
    "No participants are eligible for revised primary PREVENT calculation.",
    call. = FALSE
  )
}

message(
  "Participants eligible for revised primary PREVENT: n = ",
  nrow(prevent_input_primary)
)

prevent_output_primary <- risk_function(
  use_dat = prevent_input_primary,
  model = "base",
  time = "10yr",
  chol_unit = "mg/dL",
  quiet = TRUE,
  add_to_dat = TRUE,
  progress = FALSE
)

if (!"SEQN" %in% names(prevent_output_primary) ||
    anyDuplicated(prevent_output_primary$SEQN) ||
    nrow(prevent_output_primary) != nrow(prevent_input_primary) ||
    !setequal(prevent_output_primary$SEQN, prevent_input_primary$SEQN)) {
  stop("PREVENT returned an unexpected participant set; results were not accepted.")
}
if (!"total_cvd" %in% names(prevent_output_primary)) {
  stop(
    paste0(
      "preventr output does not contain a variable named 'total_cvd'.\n",
      "Available output variables:\n",
      paste(names(prevent_output_primary), collapse = ", ")
    ),
    call. = FALSE
  )
}

if (any(!is.finite(prevent_output_primary$total_cvd)) ||
    any(prevent_output_primary$total_cvd < 0 | prevent_output_primary$total_cvd > 1)) {
  stop("PREVENT must return a finite probability in [0,1] for every eligible participant.")
}

prevent_results_primary <- prevent_output_primary %>%
  transmute(
    SEQN,
    prevent_total_cvd_10y_primary = total_cvd,
    prevent_model_primary =
      if ("model" %in% names(prevent_output_primary)) model else NA_character_,
    prevent_input_problems_primary =
      if ("input_problems" %in% names(prevent_output_primary)) {
        as.character(input_problems)
      } else {
        NA_character_
      }
  )

nhanes_30_59 <- nhanes_30_59 %>%
  left_join(
    prevent_results_primary,
    by = "SEQN"
  )


# ==============================================================================
# 8. RE-DERIVE CKM STAGE 0-4 CRITERIA
# ==============================================================================

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    # --------------------------------------------------------------------------
    # Stage 1 adiposity / prediabetes criteria
    # --------------------------------------------------------------------------

    high_bmi_primary =
      !is.na(BMI) &
      BMI >= 25,

    high_waist_primary = case_when(
      sex == "female" & !is.na(waist_cm) ~ waist_cm >= 88,
      sex == "male"   & !is.na(waist_cm) ~ waist_cm >= 102,
      TRUE ~ NA
    ),

    prediabetes = case_when(
      diabetes %in% TRUE ~ FALSE,

      self_report_prediabetes %in% TRUE ~ TRUE,

      !is.na(fasting_glucose) &
        fasting_glucose >= 100 &
        fasting_glucose < 126 ~ TRUE,

      !is.na(hba1c) &
        hba1c >= 5.7 &
        hba1c < 6.5 ~ TRUE,

      self_report_prediabetes %in% FALSE &
        !is.na(fasting_glucose) &
        !is.na(hba1c) &
        fasting_glucose < 100 &
        hba1c < 5.7 ~ FALSE,

      TRUE ~ NA
    ),

    # --------------------------------------------------------------------------
    # Stage 2 metabolic / kidney criteria
    # --------------------------------------------------------------------------

    hypertension = case_when(
      self_report_htn %in% TRUE ~ TRUE,
      bp_tx %in% TRUE ~ TRUE,

      !is.na(sbp) &
        sbp >= 130 ~ TRUE,

      !is.na(dbp) &
        dbp >= 80 ~ TRUE,

      self_report_htn %in% FALSE &
        bp_tx %in% FALSE &
        !is.na(sbp) &
        !is.na(dbp) &
        sbp < 130 &
        dbp < 80 ~ FALSE,

      TRUE ~ NA
    ),

    hypertriglyceridemia_135 =
      if_else(
        !is.na(triglycerides),
        triglycerides >= 135,
        NA
      ),

    low_hdl = case_when(
      sex == "male" &
        !is.na(hdl) ~ hdl < 40,

      sex == "female" &
        !is.na(hdl) ~ hdl < 50,

      TRUE ~ NA
    ),

    mets_tg =
      if_else(
        !is.na(triglycerides),
        triglycerides >= 150,
        NA
      ),

    mets_bp = case_when(
      bp_tx %in% TRUE ~ TRUE,

      !is.na(sbp) &
        sbp >= 130 ~ TRUE,

      !is.na(dbp) &
        dbp >= 80 ~ TRUE,

      bp_tx %in% FALSE &
        !is.na(sbp) &
        !is.na(dbp) &
        sbp < 130 &
        dbp < 80 ~ FALSE,

      TRUE ~ NA
    ),

    mets_glucose = case_when(
      diabetes %in% TRUE ~ TRUE,
      prediabetes %in% TRUE ~ TRUE,

      diabetes %in% FALSE &
        prediabetes %in% FALSE ~ FALSE,

      TRUE ~ NA
    )
  )


# ==============================================================================
# 9. METABOLIC SYNDROME COMPONENT COUNT
# ==============================================================================

mets_primary_mat <- nhanes_30_59 %>%
  transmute(
    high_waist_primary,
    low_hdl,
    mets_tg,
    mets_bp,
    mets_glucose
  )

nhanes_30_59$mets_count_primary <- ifelse(
  complete.cases(mets_primary_mat),
  rowSums(
    as.data.frame(
      lapply(mets_primary_mat, as.integer)
    )
  ),
  NA_integer_
)

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    metabolic_syndrome_primary =
      if_else(
        !is.na(mets_count_primary),
        mets_count_primary >= 3,
        NA
      ),

    ckd_moderate_or_high = case_when(
      kdigo %in% c("moderate", "high") ~ TRUE,
      kdigo %in% c("low", "very_high") ~ FALSE,
      TRUE ~ NA
    ),

    ckd_very_high = case_when(
      kdigo == "very_high" ~ TRUE,
      kdigo %in% c("low", "moderate", "high") ~ FALSE,
      TRUE ~ NA
    ),

    stage1_primary_criterion =
      high_bmi_primary |
      high_waist_primary |
      prediabetes,

    stage2_primary_criterion =
      hypertriglyceridemia_135 |
      hypertension |
      diabetes |
      metabolic_syndrome_primary |
      ckd_moderate_or_high,

    # Revised primary Stage 3:
    # very-high-risk CKD OR PREVENT 10-year total CVD risk >=20%.
    stage3_prevent_criterion =
      ckd_very_high |
      (
        !is.na(prevent_total_cvd_10y_primary) &
        prevent_total_cvd_10y_primary >= 0.20
      ),

    # Stage 4:
    # established clinical CVD.
    stage4_criterion =
      clinical_cvd
  )


# ==============================================================================
# 10. REVISED COMPLETE-STAGING ASCERTAINMENT
#
# Key revision:
#   clinical CVD = TRUE directly establishes Stage 4.
#   PREVENT is NOT required in such participants.
# ==============================================================================

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    complete_lower_stage_information =
      complete.cases(
        BMI,
        waist_cm,
        fasting_glucose,
        hba1c,
        triglycerides,
        hdl,
        sbp,
        dbp,
        bp_tx,
        diabetes,
        prediabetes,
        egfr,
        uacr,
        kdigo,
        clinical_cvd
      ),

    stage3_ascertainable = case_when(
      # Very-high-risk CKD independently establishes Stage 3.
      ckd_very_high %in% TRUE ~ TRUE,

      # When very-high-risk CKD is absent, a valid PREVENT result
      # is required to establish/exclude the PREVENT Stage-3 criterion.
      ckd_very_high %in% FALSE &
        !is.na(prevent_total_cvd_10y_primary) ~ TRUE,

      TRUE ~ FALSE
    ),

    complete_staging_primary_revised = case_when(
      # Established clinical CVD directly establishes Stage 4.
      clinical_cvd %in% TRUE ~ TRUE,

      # Participants without clinical CVD require complete lower-stage
      # information and ascertainment of Stage 3.
      clinical_cvd %in% FALSE &
        complete_lower_stage_information &
        stage3_ascertainable ~ TRUE,

      TRUE ~ FALSE
    )
  )


# ==============================================================================
# 11. ASSIGN REVISED PRIMARY CKM STAGE
# ==============================================================================

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    ckm_stage_primary_revised = case_when(
      !complete_staging_primary_revised ~ NA_integer_,

      stage4_criterion %in% TRUE ~ 4L,

      stage3_prevent_criterion %in% TRUE ~ 3L,

      stage2_primary_criterion %in% TRUE ~ 2L,

      stage1_primary_criterion %in% TRUE ~ 1L,

      TRUE ~ 0L
    ),

    advanced_ckm_primary_revised = case_when(
      is.na(ckm_stage_primary_revised) ~ NA_integer_,
      ckm_stage_primary_revised >= 3 ~ 1L,
      TRUE ~ 0L
    )
  )


# ==============================================================================
# 12. RECREATE THE 8-YEAR FASTING SUBSAMPLE WEIGHT
# ==============================================================================

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    WTSAF8YR = if_else(
      !is.na(WTSAF2YR) &
        WTSAF2YR > 0,
      WTSAF2YR / 4,
      NA_real_
    ),

    STRATA_8YR =
      interaction(
        cycle,
        SDMVSTRA,
        drop = TRUE
      ),

    PSU_8YR =
      interaction(
        cycle,
        SDMVSTRA,
        SDMVPSU,
        drop = TRUE
      )
  )


# ==============================================================================
# 13. DEFINE THE REVISED PCA-READY EXTERNAL VALIDATION SAMPLE
# ==============================================================================

pca_vars <- c(
  "BMI",
  "VAT_area",
  "Trunk_pct",
  "LA_pct",
  "RA_pct",
  "LL_pct",
  "RL_pct"
)

nhanes_external_ready_revised <- nhanes_30_59 %>%
  filter(
    complete_staging_primary_revised,
    !is.na(WTSAF8YR),
    WTSAF8YR > 0,
    complete.cases(
      across(
        all_of(pca_vars)
      )
    )
  )

message(
  "Revised external-validation sample: n = ",
  nrow(nhanes_external_ready_revised)
)
  list(age_eligible = nhanes_30_59, final = nhanes_external_ready_revised, prevent_input = prevent_input_primary)
}


# =============================================================================
# Audit, historical comparison, corrected flow products, and execution
# =============================================================================

flow_counts <- function(all_n, dat) {
  w <- is.finite(dat$WTSAF8YR) & dat$WTSAF8YR > 0
  s <- dat$complete_staging_primary_revised %in% TRUE
  p <- stats::complete.cases(dat[, PCA_VARS, drop = FALSE])
  c(all = all_n, age = nrow(dat), fasting = sum(w), staging = sum(s),
    both = sum(w & s), missing_adiposity = sum(w & s & !p), final = sum(w & s & p))
}

