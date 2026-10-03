# =============================================================================
# PUBLIC RELEASE SCRIPT: NHANES 2011-2018 download, harmonization and initial staging
# Repository paths are relative and no participant-level data are bundled.
# See README.md and documentation/RESTRICTED_INPUTS.md before running.
# =============================================================================

# ========================================================================
# NHANES 2011-2018 External Replication Preparation
# Purpose:
#   1) Download all NHANES raw files needed for adiposity + CKM staging
#   2) Lock the real variable names for each 2-year cycle
#   3) Operationalize CKM stages 0-4 using an NHANES-adapted 2023 AHA framework
#   4) Calculate PREVENT 10-year total CVD risk for primary Stage 3 ascertainment
#   5) Construct the correct 8-year fasting-subsample survey weight
#
# Target manuscript:
#   External cross-platform replication of BIA-derived regional fat-distribution
#   phenotypes using NHANES DXA data.
#
# Primary external cohort:
#   NHANES 2011-2018, age 30-59 years.
#   Reason: whole-body DXA is available through age 59, while PREVENT is validated
#   for age 30-79. Restricting the primary cohort to 30-59 permits complete
#   PREVENT-based Stage 3 ascertainment.
#
# IMPORTANT DECISIONS LOCKED IN THIS SCRIPT
# ------------------------------------------------------------------------
# A. Primary CKM framework:
#    NHANES-adapted 2023 AHA CKM staging, consistent with published NHANES
#    operationalizations (e.g., Aggarwal et al., JAMA 2024).
#
# B. PREVENT:
#    PRIMARY, not merely sensitivity, because Stage 3 in the 2023 AHA framework
#    includes high predicted 10-year CVD risk (>=20%) as a risk equivalent.
#    A sensitivity outcome that removes PREVENT from Stage 3 is also generated.
#
# C. Sample weight:
#    WTSAF2YR (fasting subsample weight), because CKM staging uses fasting glucose
#    and fasting triglycerides. For four 2-year cycles:
#        WTSAF8YR = WTSAF2YR / 4
#    DXA does not provide a more restrictive special analytic weight than the
#    fasting-laboratory subsample used here.
#
# D. Primary adiposity cutoffs in NHANES:
#    To maximize comparability with the published US NHANES CKM staging literature,
#    primary Stage 1 uses BMI >=25 kg/m^2 and waist >=88 cm (women) / >=102 cm (men).
#    An Asian-specific sensitivity staging is also generated:
#    BMI >=23 kg/m^2 and waist >=80/90 cm among non-Hispanic Asian participants.
#
# E. PREVENT implementation:
#    Uses CRAN package 'preventr' and explicitly requests the BASE PREVENT model.
#    This avoids mixing optional HbA1c/UACR-enhanced PREVENT models into the CKM
#    staging algorithm. All PREVENT-required continuous predictors are winsorized
#    to the allowable ranges documented by the installed preventr version.
#
# References for methodological decisions:
# - 2023 AHA CKM Presidential Advisory:
#   https://www.ahajournals.org/doi/10.1161/CIR.0000000000001184
# - Aggarwal et al. JAMA 2024:
#   https://jamanetwork.com/journals/jama/fullarticle/2818457
# - NHANES data portal:
#   https://wwwn.cdc.gov/nchs/nhanes/
# - PREVENT information:
#   https://professional.heart.org/en/guidelines-and-statements/about-prevent-calculator
# - preventr:
#   https://cran.r-project.org/package=preventr
# ========================================================================


# ========================================================================
# 0. USER PATH
# ========================================================================

source("00_config.R", encoding = "UTF-8")
BASE_DIR <- PUBLIC_NHANES_DIR

DIR_RAW       <- file.path(BASE_DIR, "data_raw")
DIR_PROCESSED <- file.path(BASE_DIR, "data_processed")
DIR_META      <- file.path(BASE_DIR, "00_metadata")
DIR_QC        <- file.path(BASE_DIR, "QC")

dir.create(BASE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_RAW, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_PROCESSED, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_META, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_QC, recursive = TRUE, showWarnings = FALSE)


# ========================================================================
# 1. PACKAGES
# ========================================================================

required_pkgs <- c(
  "haven",
  "dplyr",
  "tidyr",
  "purrr",
  "stringr",
  "tibble",
  "readr",
  "survey",
  "preventr",
  "nhanesA"
)

to_install <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                   logical(1), quietly = TRUE)]
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(readr)
  library(survey)
  library(preventr)
})

options(survey.lonely.psu = "adjust")
options(stringsAsFactors = FALSE)
set.seed(20260826)


# ========================================================================
# 2. NHANES CYCLES
# ========================================================================

cycles <- tribble(
  ~cycle,      ~begin_year, ~suffix,
  "2011-2012", 2011L,       "G",
  "2013-2014", 2013L,       "H",
  "2015-2016", 2015L,       "I",
  "2017-2018", 2017L,       "J"
)

# Files required for the four requested tasks.
file_stubs <- c(
  "DEMO",      # demographics, design variables
  "BMX",       # BMI, waist
  "DXX",       # whole-body DXA regional percent fat
  "DXXAG",     # DXA VAT area
  "BPX",       # measured blood pressure
  "BPQ",       # hypertension diagnosis/treatment
  "DIQ",       # diabetes/prediabetes diagnosis/medication
  "SMQ",       # smoking status for PREVENT
  "MCQ",       # clinical CVD history
  "RXQ_RX",    # prescription medications, used to identify statins
  "GHB",       # HbA1c
  "GLU",       # fasting plasma glucose + fasting weight
  "HDL",       # HDL-C
  "TCHOL",     # total cholesterol
  "TRIGLY",    # fasting triglycerides
  "BIOPRO",    # serum creatinine
  "ALB_CR"     # urine albumin/creatinine ratio
)


# ========================================================================
# 3. LOCKED REAL VARIABLE NAMES
#    Names are identical across G/H/I/J for the variables used here.
#    After reading XPT files, all names are normalized to UPPERCASE.
# ========================================================================

variable_specs <- tribble(
  ~file_stub, ~variable,    ~role,

  "DEMO",     "SEQN",       "participant identifier",
  "DEMO",     "RIDAGEYR",   "age in years",
  "DEMO",     "RIAGENDR",   "sex",
  "DEMO",     "RIDRETH3",   "race/ethnicity incl. non-Hispanic Asian",
  "DEMO",     "WTMEC2YR",   "2-year MEC exam weight",
  "DEMO",     "SDMVPSU",    "masked PSU",
  "DEMO",     "SDMVSTRA",   "masked stratum",

  "BMX",      "SEQN",       "participant identifier",
  "BMX",      "BMXBMI",     "BMI kg/m^2",
  "BMX",      "BMXWAIST",   "waist circumference cm",

  "DXX",      "SEQN",       "participant identifier",
  "DXX",      "DXDTRPF",    "trunk percent fat",
  "DXX",      "DXDLAPF",    "left arm percent fat",
  "DXX",      "DXDRAPF",    "right arm percent fat",
  "DXX",      "DXDLLPF",    "left leg percent fat",
  "DXX",      "DXDRLPF",    "right leg percent fat",

  "DXXAG",    "SEQN",       "participant identifier",
  "DXXAG",    "DXXVFATA",   "visceral adipose tissue area",

  "BPX",      "SEQN",       "participant identifier",
  "BPX",      "BPXSY1",     "systolic BP reading 1",
  "BPX",      "BPXDI1",     "diastolic BP reading 1",
  "BPX",      "BPXSY2",     "systolic BP reading 2",
  "BPX",      "BPXDI2",     "diastolic BP reading 2",
  "BPX",      "BPXSY3",     "systolic BP reading 3",
  "BPX",      "BPXDI3",     "diastolic BP reading 3",
  "BPX",      "BPXSY4",     "systolic BP reading 4",
  "BPX",      "BPXDI4",     "diastolic BP reading 4",

  "BPQ",      "SEQN",       "participant identifier",
  "BPQ",      "BPQ020",     "ever told hypertension",
  "BPQ",      "BPQ050A",    "now taking prescribed medicine for HBP",

  "DIQ",      "SEQN",       "participant identifier",
  "DIQ",      "DIQ010",     "doctor told diabetes",
  "DIQ",      "DIQ050",     "currently taking insulin",
  "DIQ",      "DIQ070",     "currently taking diabetes pills",
  "DIQ",      "DIQ160",     "doctor told prediabetes/borderline diabetes",

  "SMQ",      "SEQN",       "participant identifier",
  "SMQ",      "SMQ020",     "smoked at least 100 cigarettes in life",
  "SMQ",      "SMQ040",     "now smoke every day/some days/not at all",

  "MCQ",      "SEQN",       "participant identifier",
  "MCQ",      "MCQ160B",    "congestive heart failure",
  "MCQ",      "MCQ160C",    "coronary heart disease",
  "MCQ",      "MCQ160D",    "angina/angina pectoris",
  "MCQ",      "MCQ160E",    "heart attack",
  "MCQ",      "MCQ160F",    "stroke",

  "RXQ_RX",   "SEQN",       "participant identifier",
  "RXQ_RX",   "RXDUSE",     "prescription medicine use past 30 days",
  "RXQ_RX",   "RXDDRUG",    "generic drug name",
  "RXQ_RX",   "RXDDRGID",   "generic drug code",

  "GHB",      "SEQN",       "participant identifier",
  "GHB",      "LBXGH",      "HbA1c percent",

  "GLU",      "SEQN",       "participant identifier",
  "GLU",      "WTSAF2YR",   "fasting subsample 2-year MEC weight",
  "GLU",      "LBXGLU",     "fasting plasma glucose mg/dL",

  "HDL",      "SEQN",       "participant identifier",
  "HDL",      "LBDHDD",     "direct HDL cholesterol mg/dL",

  "TCHOL",    "SEQN",       "participant identifier",
  "TCHOL",    "LBXTC",      "total cholesterol mg/dL",

  "TRIGLY",   "SEQN",       "participant identifier",
  "TRIGLY",   "WTSAF2YR",   "fasting subsample 2-year MEC weight",
  "TRIGLY",   "LBXTR",      "fasting triglycerides mg/dL",

  "BIOPRO",   "SEQN",       "participant identifier",
  "BIOPRO",   "LBXSCR",     "serum creatinine mg/dL",

  "ALB_CR",   "SEQN",       "participant identifier",
  "ALB_CR",   "URDACT",     "urine albumin-creatinine ratio mg/g"
)

variable_manifest <- tidyr::crossing(cycles, variable_specs) %>%
  mutate(
    dataset_name = paste0(file_stub, "_", suffix),
    raw_filename = paste0(dataset_name, ".XPT")
  ) %>%
  select(cycle, begin_year, suffix, dataset_name, file_stub,
         raw_filename, variable, role)

readr::write_csv(
  variable_manifest,
  file.path(DIR_META, "NHANES_2011_2018_variable_manifest.csv")
)


# ========================================================================
# 4. CKM OPERATIONAL DEFINITION TABLE
# ========================================================================

ckm_definition <- tribble(
  ~stage, ~label, ~operational_definition,
  0L, "No CKM risk factors",
  "Does not meet any criteria for stages 1-4 after complete staging ascertainment.",
  1L, "Excess or dysfunctional adiposity",
  "Overweight/obesity (primary NHANES: BMI >=25 kg/m^2), elevated waist (women >=88 cm; men >=102 cm), or prediabetes (self-report, fasting glucose 100-125 mg/dL, or HbA1c 5.7-6.4%), without criteria for a higher stage.",
  2L, "Metabolic risk factors or moderate/high-risk CKD",
  "Any of: fasting triglycerides >=135 mg/dL; hypertension (self-report, mean SBP >=130 mmHg, mean DBP >=80 mmHg, or current antihypertensive treatment); diabetes (self-report, glucose-lowering medication/insulin, fasting glucose >=126 mg/dL, or HbA1c >=6.5%); metabolic syndrome >=3 components; or KDIGO moderate/high CKD risk, without criteria for a higher stage.",
  3L, "Subclinical CVD risk equivalent",
  "PREVENT 10-year total CVD risk >=20% OR KDIGO very-high-risk CKD, without clinical CVD. NHANES lacks a uniform public subclinical imaging/biomarker panel across 2011-2018; this is a data-available operationalization.",
  4L, "Clinical CVD",
  "Self-reported physician diagnosis of congestive heart failure, coronary heart disease, angina, heart attack, or stroke."
)

readr::write_csv(
  ckm_definition,
  file.path(DIR_META, "CKM_stage_operational_definition.csv")
)

decision_text <- c(
  "PREVENT decision: PRIMARY for Stage 3 ascertainment in the primary external cohort.",
  "Reason: the 2023 AHA CKM framework defines Stage 3 to include high predicted 10-year CVD risk as a risk equivalent; published NHANES implementations use PREVENT >=20%.",
  "Primary age range: 30-59 years, because PREVENT applies from age 30 and NHANES whole-body DXA is available through age 59.",
  "Sensitivity outcome also created: Stage 3 defined by very-high-risk CKD only (PREVENT removed), with Stage 4 unchanged.",
  "",
  "Survey-weight decision: use WTSAF2YR because fasting glucose and fasting triglycerides enter CKM staging.",
  "For 2011-2018 (four equal 2-year cycles), use WTSAF8YR = WTSAF2YR / 4.",
  "Use SDMVSTRA and SDMVPSU with cycle-specific identifiers to prevent accidental pooling of masked strata/PSU labels across cycles."
)
writeLines(decision_text,
           con = file.path(DIR_META, "PREVENT_and_weight_decisions.txt"),
           useBytes = TRUE)


# ========================================================================
# 5. DOWNLOAD RAW NHANES XPT FILES
# ========================================================================

download_one_nhanes <- function(begin_year, suffix, file_stub, out_dir = DIR_RAW) {

  dataset <- paste0(file_stub, "_", suffix)
  dest <- file.path(out_dir, paste0(dataset, ".XPT"))

  if (file.exists(dest) && file.info(dest)$size > 1000) {
    message("[exists] ", basename(dest))
    return(dest)
  }

  url <- paste0(
    "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",
    begin_year,
    "/DataFiles/",
    dataset,
    ".xpt"
  )

  message("[download] ", dataset)

  ok <- tryCatch({
    utils::download.file(
      url = url,
      destfile = dest,
      method = "libcurl",
      mode = "wb",
      quiet = TRUE
    )
    file.exists(dest) && file.info(dest)$size > 1000
  }, error = function(e) FALSE)

  # Fallback through nhanesA if direct CDC transfer fails.
  if (!ok) {
    message("  direct download failed; trying nhanesA::nhanes() ...")
    if (file.exists(dest)) unlink(dest)

    dat <- tryCatch(
      nhanesA::nhanes(dataset),
      error = function(e) NULL
    )

    if (is.null(dat)) {
      stop("Failed to download dataset: ", dataset)
    }

    haven::write_xpt(dat, dest)
  }

  dest
}

download_manifest <- tidyr::crossing(cycles, file_stub = file_stubs) %>%
  mutate(
    dataset_name = paste0(file_stub, "_", suffix),
    filepath = purrr::pmap_chr(
      list(begin_year, suffix, file_stub),
      ~ download_one_nhanes(..1, ..2, ..3)
    ),
    file_size_mb = file.info(filepath)$size / 1024^2
  )

readr::write_csv(
  download_manifest,
  file.path(DIR_META, "download_manifest.csv")
)


# ========================================================================
# 6. READ + NORMALIZE VARIABLE NAMES TO UPPERCASE
# ========================================================================

read_xpt_upper <- function(path) {
  x <- haven::read_xpt(path)
  names(x) <- toupper(names(x))
  x
}

dataset_path <- function(file_stub, suffix) {
  file.path(DIR_RAW, paste0(file_stub, "_", suffix, ".XPT"))
}

assert_vars <- function(dat, vars, dataset_name) {
  vars <- toupper(vars)
  miss <- setdiff(vars, names(dat))
  if (length(miss) > 0) {
    stop(
      "Missing expected variable(s) in ", dataset_name, ": ",
      paste(miss, collapse = ", ")
    )
  }
  invisible(TRUE)
}

required_vars_by_file <- split(
  unique(toupper(variable_specs$variable)),
  toupper(variable_specs$file_stub)
)


# ========================================================================
# 7. CHECK ALL FOUR CYCLES AGAINST THE LOCKED VARIABLE DICTIONARY
# ========================================================================

variable_qc <- list()

for (i in seq_len(nrow(cycles))) {

  cy <- cycles$cycle[i]
  sf <- cycles$suffix[i]

  for (stub in file_stubs) {

    dat <- read_xpt_upper(dataset_path(stub, sf))
    expected <- toupper(variable_specs$variable[
      toupper(variable_specs$file_stub) == toupper(stub)
    ])

    missing_vars <- setdiff(expected, names(dat))

    variable_qc[[length(variable_qc) + 1L]] <- tibble(
      cycle = cy,
      dataset = paste0(stub, "_", sf),
      expected_n = length(expected),
      missing_n = length(missing_vars),
      missing_variables = paste(missing_vars, collapse = ";")
    )
  }
}

variable_qc <- bind_rows(variable_qc)

readr::write_csv(
  variable_qc,
  file.path(DIR_QC, "variable_name_verification.csv")
)

if (any(variable_qc$missing_n > 0)) {
  print(variable_qc %>% filter(missing_n > 0))
  stop("Variable verification failed. See QC/variable_name_verification.csv")
}


# ========================================================================
# 8. HELPER FUNCTIONS
# ========================================================================

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


# ========================================================================
# 9. READ AND HARMONIZE EACH 2-YEAR CYCLE
# ========================================================================

harmonize_cycle <- function(cycle_label, suffix) {

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

    left_join(
      bmx %>% select(SEQN, BMXBMI, BMXWAIST),
      by = "SEQN"
    ) %>%

    left_join(
      dxx %>% select(
        SEQN, DXDTRPF, DXDLAPF, DXDRAPF, DXDLLPF, DXDRLPF
      ),
      by = "SEQN"
    ) %>%

    left_join(
      dxxag %>% select(SEQN, DXXVFATA),
      by = "SEQN"
    ) %>%

    left_join(
      bpx %>% select(
        SEQN,
        BPXSY1, BPXDI1, BPXSY2, BPXDI2,
        BPXSY3, BPXDI3, BPXSY4, BPXDI4
      ),
      by = "SEQN"
    ) %>%

    left_join(
      bpq %>% select(SEQN, BPQ020, BPQ050A),
      by = "SEQN"
    ) %>%

    left_join(
      diq %>% select(SEQN, DIQ010, DIQ050, DIQ070, DIQ160),
      by = "SEQN"
    ) %>%

    left_join(
      smq %>% select(SEQN, SMQ020, SMQ040),
      by = "SEQN"
    ) %>%

    left_join(
      mcq %>% select(
        SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F
      ),
      by = "SEQN"
    ) %>%

    left_join(statin_tbl, by = "SEQN") %>%

    left_join(
      ghb %>% select(SEQN, LBXGH),
      by = "SEQN"
    ) %>%

    left_join(glu_small, by = "SEQN") %>%

    left_join(
      hdl %>% select(SEQN, LBDHDD),
      by = "SEQN"
    ) %>%

    left_join(
      tchol %>% select(SEQN, LBXTC),
      by = "SEQN"
    ) %>%

    left_join(trigly_small, by = "SEQN") %>%

    left_join(
      bio %>% select(SEQN, LBXSCR),
      by = "SEQN"
    ) %>%

    left_join(
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

  out
}

nhanes_all <- purrr::map2_dfr(
  cycles$cycle,
  cycles$suffix,
  harmonize_cycle
)


# ========================================================================
# 10. DERIVE ANALYTIC VARIABLES
# ========================================================================

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


# ========================================================================
# 11. PRIMARY EXTERNAL COHORT AGE RANGE
# ========================================================================

# Primary: 30-59 years.
# - PREVENT lower age limit = 30.
# - NHANES whole-body DXA public data are available through age 59.
nhanes_30_59 <- nhanes_all %>%
  filter(age >= 30, age <= 59)


# ========================================================================
# 12. PREVENT 10-YEAR TOTAL CVD RISK
# ========================================================================

# The current 'preventr' implementation requires the following base-model inputs:
# age, sex, SBP, BP treatment, total cholesterol, HDL-C, statin, diabetes,
# current smoking, eGFR, BMI.
#
# For primary CKM staging, use the BASE model only. Do not pass optional HbA1c/UACR
# into PREVENT because those variables already contribute to CKM staging elsewhere.

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    diabetes = case_when(
      self_report_dm %in% TRUE ~ TRUE,
      insulin_use %in% TRUE ~ TRUE,
      diabetes_pills %in% TRUE ~ TRUE,
      !is.na(fasting_glucose) & fasting_glucose >= 126 ~ TRUE,
      !is.na(hba1c) & hba1c >= 6.5 ~ TRUE,

      self_report_dm %in% FALSE &
        insulin_use %in% FALSE &
        diabetes_pills %in% FALSE &
        !is.na(fasting_glucose) &
        !is.na(hba1c) ~ FALSE,

      TRUE ~ NA
    ),

    # Values used only inside PREVENT are clamped to the allowable ranges
    # documented by the installed preventr version.
    sbp_prevent = clamp(sbp, 90, 180),
    total_c_prevent = clamp(total_chol, 130, 320),
    hdl_c_prevent = clamp(hdl, 20, 100),
    egfr_prevent = clamp(egfr, 15, 140),
    bmi_prevent = clamp(BMI, 18.5, 39.9),

    prevent_complete = complete.cases(
      age,
      sex,
      sbp_prevent,
      bp_tx,
      total_c_prevent,
      hdl_c_prevent,
      statin,
      diabetes,
      current_smoker,
      egfr_prevent,
      bmi_prevent
    )
  )

prevent_input <- nhanes_30_59 %>%
  filter(prevent_complete) %>%
  transmute(
    SEQN,
    age = age,
    sex = sex,
    sbp = sbp_prevent,
    bp_tx = bp_tx,
    total_c = total_c_prevent,
    hdl_c = hdl_c_prevent,
    statin = statin,
    dm = diabetes,
    smoking = current_smoker,
    egfr = egfr_prevent,
    bmi = bmi_prevent
  )

if (nrow(prevent_input) == 0) {
  stop("No complete PREVENT inputs after harmonization.")
}

prevent_output <- preventr::estimate_risk(
  use_dat = prevent_input,
  model = "base",
  time = "10yr",
  chol_unit = "mg/dL",
  quiet = TRUE,
  add_to_dat = TRUE,
  progress = TRUE
)

prevent_results <- prevent_output %>%
  transmute(
    SEQN,
    prevent_total_cvd_10y = total_cvd,
    prevent_model = model,
    prevent_input_problems = input_problems
  )

nhanes_30_59 <- nhanes_30_59 %>%
  left_join(prevent_results, by = "SEQN")


# Save PREVENT input-clamping QC.
prevent_clamp_qc <- nhanes_30_59 %>%
  summarise(
    n_age_30_59 = n(),
    n_prevent_complete = sum(prevent_complete, na.rm = TRUE),
    n_sbp_clamped = sum(!is.na(sbp) & sbp != sbp_prevent),
    n_total_c_clamped = sum(!is.na(total_chol) & total_chol != total_c_prevent),
    n_hdl_clamped = sum(!is.na(hdl) & hdl != hdl_c_prevent),
    n_egfr_clamped = sum(!is.na(egfr) & egfr != egfr_prevent),
    n_bmi_clamped = sum(!is.na(BMI) & BMI != bmi_prevent)
  )

readr::write_csv(
  prevent_clamp_qc,
  file.path(DIR_QC, "PREVENT_clamping_QC.csv")
)


# ========================================================================
# 13. CKM STAGE 0-4 OPERATIONALIZATION
# ========================================================================

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    # ---- Primary US NHANES adiposity thresholds ----
    high_bmi_primary = !is.na(BMI) & BMI >= 25,

    high_waist_primary = case_when(
      sex == "female" & !is.na(waist_cm) ~ waist_cm >= 88,
      sex == "male" & !is.na(waist_cm) ~ waist_cm >= 102,
      TRUE ~ NA
    ),

    # ---- Asian-specific sensitivity thresholds ----
    high_bmi_asian_sens = case_when(
      non_hispanic_asian %in% TRUE & !is.na(BMI) ~ BMI >= 23,
      non_hispanic_asian %in% FALSE & !is.na(BMI) ~ BMI >= 25,
      TRUE ~ NA
    ),

    high_waist_asian_sens = case_when(
      non_hispanic_asian %in% TRUE & sex == "female" & !is.na(waist_cm) ~ waist_cm >= 80,
      non_hispanic_asian %in% TRUE & sex == "male" & !is.na(waist_cm) ~ waist_cm >= 90,
      non_hispanic_asian %in% FALSE & sex == "female" & !is.na(waist_cm) ~ waist_cm >= 88,
      non_hispanic_asian %in% FALSE & sex == "male" & !is.na(waist_cm) ~ waist_cm >= 102,
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

    hypertension = case_when(
      self_report_htn %in% TRUE ~ TRUE,
      bp_tx %in% TRUE ~ TRUE,
      !is.na(sbp) & sbp >= 130 ~ TRUE,
      !is.na(dbp) & dbp >= 80 ~ TRUE,

      self_report_htn %in% FALSE &
        bp_tx %in% FALSE &
        !is.na(sbp) &
        !is.na(dbp) &
        sbp < 130 &
        dbp < 80 ~ FALSE,

      TRUE ~ NA
    ),

    hypertriglyceridemia_135 =
      if_else(!is.na(triglycerides), triglycerides >= 135, NA),

    low_hdl = case_when(
      sex == "male" & !is.na(hdl) ~ hdl < 40,
      sex == "female" & !is.na(hdl) ~ hdl < 50,
      TRUE ~ NA
    ),

    # Metabolic syndrome components.
    # Following published NHANES-adapted CKM definitions:
    # 1 elevated waist
    # 2 HDL low
    # 3 TG >=150
    # 4 BP >=130/80 or BP medication
    # 5 prediabetes/hyperglycemia
    mets_tg = if_else(!is.na(triglycerides), triglycerides >= 150, NA),

    mets_bp = case_when(
      bp_tx %in% TRUE ~ TRUE,
      !is.na(sbp) & sbp >= 130 ~ TRUE,
      !is.na(dbp) & dbp >= 80 ~ TRUE,
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
      diabetes %in% FALSE & prediabetes %in% FALSE ~ FALSE,
      TRUE ~ NA
    )
  )

# Count MetS components only when all five are observed.
mets_primary_mat <- nhanes_30_59 %>%
  transmute(
    high_waist_primary,
    low_hdl,
    mets_tg,
    mets_bp,
    mets_glucose
  )

mets_asian_mat <- nhanes_30_59 %>%
  transmute(
    high_waist_asian_sens,
    low_hdl,
    mets_tg,
    mets_bp,
    mets_glucose
  )

nhanes_30_59$mets_count_primary <- ifelse(
  complete.cases(mets_primary_mat),
  rowSums(as.data.frame(lapply(mets_primary_mat, as.integer))),
  NA_integer_
)

nhanes_30_59$mets_count_asian_sens <- ifelse(
  complete.cases(mets_asian_mat),
  rowSums(as.data.frame(lapply(mets_asian_mat, as.integer))),
  NA_integer_
)

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    metabolic_syndrome_primary =
      if_else(!is.na(mets_count_primary), mets_count_primary >= 3, NA),

    metabolic_syndrome_asian_sens =
      if_else(!is.na(mets_count_asian_sens), mets_count_asian_sens >= 3, NA),

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
      high_bmi_primary | high_waist_primary | prediabetes,

    stage1_asian_criterion =
      high_bmi_asian_sens | high_waist_asian_sens | prediabetes,

    stage2_primary_criterion =
      hypertriglyceridemia_135 |
      hypertension |
      diabetes |
      metabolic_syndrome_primary |
      ckd_moderate_or_high,

    stage2_asian_criterion =
      hypertriglyceridemia_135 |
      hypertension |
      diabetes |
      metabolic_syndrome_asian_sens |
      ckd_moderate_or_high,

    stage3_prevent_criterion =
      ckd_very_high |
      (!is.na(prevent_total_cvd_10y) & prevent_total_cvd_10y >= 0.20),

    stage3_no_prevent_criterion =
      ckd_very_high,

    stage4_criterion =
      clinical_cvd
  )


# ========================================================================
# 14. COMPLETE STAGING ASCERTAINMENT
# ========================================================================

# Complete-case staging is intentionally explicit for the primary external
# replication cohort. This avoids silently assigning lower CKM stages when
# PREVENT or kidney/metabolic information is missing.

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    complete_staging_primary = complete.cases(
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
      clinical_cvd,
      prevent_total_cvd_10y
    )
  )


# ========================================================================
# 15. ASSIGN PRIMARY CKM STAGE
# ========================================================================

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    ckm_stage_primary = case_when(
      !complete_staging_primary ~ NA_integer_,

      stage4_criterion %in% TRUE ~ 4L,
      stage3_prevent_criterion %in% TRUE ~ 3L,
      stage2_primary_criterion %in% TRUE ~ 2L,
      stage1_primary_criterion %in% TRUE ~ 1L,
      TRUE ~ 0L
    ),

    advanced_ckm_primary = case_when(
      is.na(ckm_stage_primary) ~ NA_integer_,
      ckm_stage_primary >= 3 ~ 1L,
      TRUE ~ 0L
    ),

    # Asian-cutoff sensitivity staging.
    ckm_stage_asian_sensitivity = case_when(
      !complete_staging_primary ~ NA_integer_,

      stage4_criterion %in% TRUE ~ 4L,
      stage3_prevent_criterion %in% TRUE ~ 3L,
      stage2_asian_criterion %in% TRUE ~ 2L,
      stage1_asian_criterion %in% TRUE ~ 1L,
      TRUE ~ 0L
    ),

    # PREVENT-removal sensitivity:
    # Stage 3 relies on very-high-risk CKD only; Stage 4 unchanged.
    ckm_stage_no_prevent_sensitivity = case_when(
      !complete_staging_primary ~ NA_integer_,

      stage4_criterion %in% TRUE ~ 4L,
      stage3_no_prevent_criterion %in% TRUE ~ 3L,
      stage2_primary_criterion %in% TRUE ~ 2L,
      stage1_primary_criterion %in% TRUE ~ 1L,
      TRUE ~ 0L
    ),

    advanced_ckm_no_prevent_sensitivity = case_when(
      is.na(ckm_stage_no_prevent_sensitivity) ~ NA_integer_,
      ckm_stage_no_prevent_sensitivity >= 3 ~ 1L,
      TRUE ~ 0L
    )
  )


# ========================================================================
# 16. CORRECT 2011-2018 FASTING SUBSAMPLE WEIGHT
# ========================================================================

# Primary analytic weight:
#   WTSAF2YR is the most restrictive released subsample weight required by the
#   variables used in staging (fasting glucose and fasting triglycerides).
#
# Combining four equal 2-year cycles:
#   WTSAF8YR = WTSAF2YR / 4

nhanes_30_59 <- nhanes_30_59 %>%
  mutate(
    WTSAF8YR = if_else(
      !is.na(WTSAF2YR) & WTSAF2YR > 0,
      WTSAF2YR / 4,
      NA_real_
    ),

    # Make strata/PSU identifiers cycle-specific for pooled 8-year analysis.
    STRATA_8YR = interaction(cycle, SDMVSTRA, drop = TRUE),
    PSU_8YR = interaction(cycle, SDMVSTRA, SDMVPSU, drop = TRUE)
  )


# ========================================================================
# 17. DEFINE THE PCA-READY EXTERNAL VALIDATION SAMPLE
# ========================================================================

pca_vars <- c(
  "BMI",
  "VAT_area",
  "Trunk_pct",
  "LA_pct",
  "RA_pct",
  "LL_pct",
  "RL_pct"
)

nhanes_external_ready <- nhanes_30_59 %>%
  filter(
    complete_staging_primary,
    !is.na(WTSAF8YR),
    WTSAF8YR > 0,
    complete.cases(across(all_of(pca_vars)))
  )


# ========================================================================
# 18. SAMPLE ATTRITION QC
# ========================================================================

attrition <- tibble(
  step = c(
    "All NHANES 2011-2018 participants",
    "Age 30-59",
    "Age 30-59 with positive fasting weight",
    "Age 30-59 with complete CKM staging",
    "Final external sample: complete staging + 7 adiposity variables"
  ),
  n = c(
    nrow(nhanes_all),
    nrow(nhanes_30_59),
    sum(!is.na(nhanes_30_59$WTSAF8YR) & nhanes_30_59$WTSAF8YR > 0),
    sum(nhanes_30_59$complete_staging_primary, na.rm = TRUE),
    nrow(nhanes_external_ready)
  )
)

readr::write_csv(
  attrition,
  file.path(DIR_QC, "sample_attrition.csv")
)


# ========================================================================
# 19. CHECK STAGE DISTRIBUTION
# ========================================================================

stage_unweighted <- nhanes_external_ready %>%
  count(ckm_stage_primary, name = "n") %>%
  mutate(percent = 100 * n / sum(n))

readr::write_csv(
  stage_unweighted,
  file.path(DIR_QC, "CKM_stage_distribution_unweighted.csv")
)

if (nrow(nhanes_external_ready) > 0) {

  design_external <- survey::svydesign(
    ids = ~PSU_8YR,
    strata = ~STRATA_8YR,
    weights = ~WTSAF8YR,
    nest = TRUE,
    data = nhanes_external_ready
  )

  weighted_stage_tab <- prop.table(
    survey::svytable(~ckm_stage_primary, design_external)
  )

  weighted_stage_df <- tibble(
    ckm_stage_primary = as.integer(names(weighted_stage_tab)),
    weighted_proportion = as.numeric(weighted_stage_tab),
    weighted_percent = 100 * as.numeric(weighted_stage_tab)
  )

  readr::write_csv(
    weighted_stage_df,
    file.path(DIR_QC, "CKM_stage_distribution_weighted.csv")
  )
}


# ========================================================================
# 20. SAVE PROCESSED DATA
# ========================================================================

saveRDS(
  nhanes_all,
  file.path(DIR_PROCESSED, "NHANES_2011_2018_harmonized_all.rds"),
  compress = "xz"
)

saveRDS(
  nhanes_30_59,
  file.path(DIR_PROCESSED, "NHANES_2011_2018_age30_59_CKM_staged.rds"),
  compress = "xz"
)

saveRDS(
  nhanes_external_ready,
  file.path(DIR_PROCESSED, "NHANES_2011_2018_external_validation_ready.rds"),
  compress = "xz"
)

# A compact CSV containing the variables needed for the next PCA/association step.
external_compact <- nhanes_external_ready %>%
  select(
    SEQN, cycle, age, sex, RIDRETH3,
    SDMVSTRA, SDMVPSU, STRATA_8YR, PSU_8YR,
    WTMEC2YR, WTSAF2YR, WTSAF8YR,

    BMI, VAT_area, Trunk_pct, LA_pct, RA_pct, LL_pct, RL_pct,

    waist_cm, fasting_glucose, hba1c, triglycerides,
    hdl, total_chol, sbp, dbp, serum_creatinine, egfr, uacr, kdigo,

    bp_tx, statin, current_smoker, diabetes, prediabetes,
    hypertension, metabolic_syndrome_primary,
    clinical_cvd,

    prevent_total_cvd_10y,

    ckm_stage_primary, advanced_ckm_primary,
    ckm_stage_asian_sensitivity,
    ckm_stage_no_prevent_sensitivity,
    advanced_ckm_no_prevent_sensitivity
  )

readr::write_csv(
  external_compact,
  file.path(DIR_PROCESSED, "NHANES_2011_2018_external_validation_ready.csv")
)


# ========================================================================
# 21. SESSION INFO AND FINAL SUMMARY
# ========================================================================

capture.output(
  sessionInfo(),
  file = file.path(DIR_META, "sessionInfo.txt")
)

cat("\n============================================================\n")
cat("NHANES external-validation preparation completed.\n")
cat("Base directory:\n", BASE_DIR, "\n\n")
cat("Primary external cohort: NHANES 2011-2018, age 30-59.\n")
cat("Primary PREVENT use: YES, Stage 3 threshold = 10-year total CVD risk >=20%.\n")
cat("Primary survey weight: WTSAF8YR = WTSAF2YR / 4.\n")
cat("Final PCA-ready sample N =", nrow(nhanes_external_ready), "\n\n")
cat("Key output:\n")
cat(file.path(DIR_PROCESSED,
              "NHANES_2011_2018_external_validation_ready.rds"), "\n")
cat("============================================================\n")
