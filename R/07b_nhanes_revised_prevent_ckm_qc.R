# =============================================================================
# PUBLIC RELEASE SCRIPT: Revised PREVENT and CKM staging QC
# Repository paths are relative and no participant-level data are bundled.
# See README.md and documentation/RESTRICTED_INPUTS.md before running.
# =============================================================================

# ==============================================================================
# 02_revised_PREVENT_CKM_QC_v2.R
#
# Purpose:
#   Recalculate the primary PREVENT component without clamping, revise CKM
#   stage ascertainment so that established clinical CVD directly establishes
#   Stage 4 without requiring PREVENT, and regenerate:
#
#   1) PREVENT_primary_revised_QC.csv
#   2) CKM_stage_distribution_primary_revised.csv
#   3) sample_attrition_revised.csv
#
# v2 fix:
#   Corrects the survey-weighted prevalence calculation for the combined
#   Stage 3-4 row and adds an internal consistency check.
#
# This script DOES NOT redownload NHANES data.
#
# Required input created by the previous script:
#   data_processed/NHANES_2011_2018_harmonized_all.rds
#
# Base directory:
#   data/public/nhanes
# ==============================================================================


# ==============================================================================
# 0. GLOBAL SETTINGS
# ==============================================================================

rm(list = ls())
source("00_config.R", encoding = "UTF-8")

options(
  timeout = 600,
  scipen = 999,
  survey.lonely.psu = "adjust"
)

set.seed(20260826)

BASE_DIR <- PUBLIC_NHANES_DIR

DIR_PROCESSED <- file.path(BASE_DIR, "data_processed")
DIR_QC        <- file.path(BASE_DIR, "QC")

dir.create(DIR_PROCESSED, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_QC, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# 1. PACKAGES
# ==============================================================================

required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "tibble",
  "survey",
  "preventr"
)

to_install <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(to_install) > 0) {
  install.packages(to_install)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(survey)
  library(preventr)
})


# ==============================================================================
# 2. LOAD THE HARMONIZED NHANES 2011-2018 DATA
# ==============================================================================

input_rds <- file.path(
  DIR_PROCESSED,
  "NHANES_2011_2018_harmonized_all.rds"
)

if (!file.exists(input_rds)) {
  stop(
    paste0(
      "Required input file was not found:\n",
      input_rds,
      "\n\nRun the original harmonization/download script first."
    ),
    call. = FALSE
  )
}

nhanes_all <- readRDS(input_rds)

message("Loaded harmonized NHANES data: n = ", nrow(nhanes_all))


# ==============================================================================
# 3. VERIFY VARIABLES REQUIRED BY THIS REVISED QC SCRIPT
# ==============================================================================

required_vars <- c(
  "SEQN",
  "cycle",
  "age",
  "sex",
  "BMI",
  "waist_cm",
  "fasting_glucose",
  "hba1c",
  "triglycerides",
  "total_chol",
  "hdl",
  "sbp",
  "dbp",
  "bp_tx",
  "self_report_htn",
  "self_report_dm",
  "insulin_use",
  "diabetes_pills",
  "self_report_prediabetes",
  "current_smoker",
  "statin",
  "egfr",
  "uacr",
  "kdigo",
  "clinical_cvd",
  "WTSAF2YR",
  "SDMVSTRA",
  "SDMVPSU",
  "VAT_area",
  "Trunk_pct",
  "LA_pct",
  "RA_pct",
  "LL_pct",
  "RL_pct"
)

missing_required_vars <- setdiff(required_vars, names(nhanes_all))

if (length(missing_required_vars) > 0) {
  stop(
    paste0(
      "The harmonized RDS is missing required variables:\n",
      paste(missing_required_vars, collapse = ", ")
    ),
    call. = FALSE
  )
}

message("Required-variable check passed.")


# ==============================================================================
# 4. PRIMARY EXTERNAL COHORT AGE RANGE: 30-59 YEARS
# ==============================================================================

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

prevent_output_primary <- preventr::estimate_risk(
  use_dat = prevent_input_primary,
  model = "base",
  time = "10yr",
  chol_unit = "mg/dL",
  quiet = TRUE,
  add_to_dat = TRUE,
  progress = TRUE
)

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


# ==============================================================================
# 14. QC FILE 1:
#     REVISED PRIMARY PREVENT QC
# ==============================================================================

prevent_primary_revised_qc <- nhanes_30_59 %>%
  summarise(
    n_age_30_59 = n(),

    n_clinical_cvd_true =
      sum(clinical_cvd %in% TRUE, na.rm = TRUE),

    n_clinical_cvd_false =
      sum(clinical_cvd %in% FALSE, na.rm = TRUE),

    n_clinical_cvd_missing =
      sum(is.na(clinical_cvd)),

    n_no_clinical_cvd =
      sum(prevent_no_cvd, na.rm = TRUE),

    n_prevent_inputs_complete =
      sum(
        prevent_no_cvd &
          prevent_inputs_complete,
        na.rm = TRUE
      ),

    n_prevent_sbp_out_of_range =
      sum(
        prevent_no_cvd &
          prevent_inputs_complete &
          !prevent_sbp_in_range,
        na.rm = TRUE
      ),

    n_prevent_total_c_out_of_range =
      sum(
        prevent_no_cvd &
          prevent_inputs_complete &
          !prevent_total_c_in_range,
        na.rm = TRUE
      ),

    n_prevent_hdl_out_of_range =
      sum(
        prevent_no_cvd &
          prevent_inputs_complete &
          !prevent_hdl_in_range,
        na.rm = TRUE
      ),

    n_prevent_egfr_out_of_range =
      sum(
        prevent_no_cvd &
          prevent_inputs_complete &
          !prevent_egfr_in_range,
        na.rm = TRUE
      ),

    n_prevent_bmi_out_of_range =
      sum(
        prevent_no_cvd &
          prevent_inputs_complete &
          !prevent_bmi_in_range,
        na.rm = TRUE
      ),

    n_prevent_all_inputs_in_range =
      sum(
        prevent_no_cvd &
          prevent_inputs_complete &
          prevent_inputs_in_range,
        na.rm = TRUE
      ),

    n_prevent_eligible_primary =
      sum(
        prevent_eligible_primary,
        na.rm = TRUE
      ),

    n_prevent_result =
      sum(
        !is.na(prevent_total_cvd_10y_primary),
        na.rm = TRUE
      ),

    n_prevent_result_missing_after_eligibility =
      sum(
        prevent_eligible_primary &
          is.na(prevent_total_cvd_10y_primary),
        na.rm = TRUE
      ),

    n_prevent_ge20 =
      sum(
        !is.na(prevent_total_cvd_10y_primary) &
          prevent_total_cvd_10y_primary >= 0.20,
        na.rm = TRUE
      ),

    n_very_high_risk_ckd =
      sum(
        ckd_very_high %in% TRUE,
        na.rm = TRUE
      ),

    n_stage4_bypassing_prevent =
      sum(
        clinical_cvd %in% TRUE,
        na.rm = TRUE
      ),

    prevent_risk_min =
      ifelse(
        any(!is.na(prevent_total_cvd_10y_primary)),
        min(prevent_total_cvd_10y_primary, na.rm = TRUE),
        NA_real_
      ),

    prevent_risk_median =
      ifelse(
        any(!is.na(prevent_total_cvd_10y_primary)),
        median(prevent_total_cvd_10y_primary, na.rm = TRUE),
        NA_real_
      ),

    prevent_risk_max =
      ifelse(
        any(!is.na(prevent_total_cvd_10y_primary)),
        max(prevent_total_cvd_10y_primary, na.rm = TRUE),
        NA_real_
      )
  )

readr::write_csv(
  prevent_primary_revised_qc,
  file.path(
    DIR_QC,
    "PREVENT_primary_revised_QC.csv"
  )
)


# ==============================================================================
# 15. QC FILE 2:
#     REVISED CKM STAGE DISTRIBUTION
#
# Output combines unweighted and survey-weighted distribution in one CSV.
# ==============================================================================

stage_unweighted_revised <- nhanes_external_ready_revised %>%
  count(
    ckm_stage_primary_revised,
    name = "n"
  ) %>%
  complete(
    ckm_stage_primary_revised = 0:4,
    fill = list(n = 0L)
  ) %>%
  arrange(ckm_stage_primary_revised) %>%
  mutate(
    unweighted_percent =
      100 * n / sum(n)
  )

if (nrow(nhanes_external_ready_revised) > 0) {

  design_external_revised <- survey::svydesign(
    ids = ~PSU_8YR,
    strata = ~STRATA_8YR,
    weights = ~WTSAF8YR,
    nest = TRUE,
    data = nhanes_external_ready_revised
  )

  weighted_stage_tab <- prop.table(
    survey::svytable(
      ~ckm_stage_primary_revised,
      design_external_revised
    )
  )

  weighted_stage_revised <- tibble(
    ckm_stage_primary_revised =
      as.integer(names(weighted_stage_tab)),

    weighted_proportion =
      as.numeric(weighted_stage_tab),

    weighted_percent =
      100 * as.numeric(weighted_stage_tab)
  )

} else {

  weighted_stage_revised <- tibble(
    ckm_stage_primary_revised = integer(),
    weighted_proportion = numeric(),
    weighted_percent = numeric()
  )
}

stage_distribution_revised <- stage_unweighted_revised %>%
  left_join(
    weighted_stage_revised,
    by = "ckm_stage_primary_revised"
  ) %>%
  mutate(
    stage = as.character(ckm_stage_primary_revised)
  ) %>%
  select(
    stage,
    n,
    unweighted_percent,
    weighted_proportion,
    weighted_percent
  )

# Add a Stage 3-4 combined row because this is the primary binary outcome.
advanced_n <- nhanes_external_ready_revised %>%
  summarise(
    n = sum(
      ckm_stage_primary_revised >= 3,
      na.rm = TRUE
    )
  ) %>%
  pull(n)

advanced_unweighted_percent <-
  if (nrow(nhanes_external_ready_revised) > 0) {
    100 * advanced_n / nrow(nhanes_external_ready_revised)
  } else {
    NA_real_
  }

advanced_weighted_proportion <-
  if (nrow(nhanes_external_ready_revised) > 0) {

    # advanced_ckm_primary_revised is already coded as numeric 0/1.
    # Taking the survey-weighted mean of this variable directly gives
    # the weighted prevalence of Stage 3-4 CKM.
    as.numeric(
      coef(
        survey::svymean(
          ~advanced_ckm_primary_revised,
          design_external_revised,
          na.rm = TRUE
        )
      )[1]
    )

  } else {
    NA_real_
  }

advanced_row <- tibble(
  stage = "3-4 combined",
  n = advanced_n,
  unweighted_percent = advanced_unweighted_percent,
  weighted_proportion = advanced_weighted_proportion,
  weighted_percent = 100 * advanced_weighted_proportion
)

# Internal consistency check:
# weighted prevalence of Stage 3-4 should equal weighted Stage 3 + Stage 4.
if (nrow(nhanes_external_ready_revised) > 0) {

  stage34_sum_check <- stage_distribution_revised %>%
    filter(stage %in% c("3", "4")) %>%
    summarise(
      weighted_sum = sum(weighted_proportion, na.rm = TRUE)
    ) %>%
    pull(weighted_sum)

  if (
    is.finite(stage34_sum_check) &&
    is.finite(advanced_weighted_proportion) &&
    abs(stage34_sum_check - advanced_weighted_proportion) > 1e-8
  ) {
    warning(
      paste0(
        "Stage 3-4 weighted prevalence consistency check failed. ",
        "Direct weighted prevalence = ",
        signif(advanced_weighted_proportion, 6),
        "; Stage 3 + Stage 4 = ",
        signif(stage34_sum_check, 6)
      )
    )
  }
}

stage_distribution_revised <- bind_rows(
  stage_distribution_revised,
  advanced_row
)

readr::write_csv(
  stage_distribution_revised,
  file.path(
    DIR_QC,
    "CKM_stage_distribution_primary_revised.csv"
  )
)


# ==============================================================================
# 16. QC FILE 3:
#     REVISED SAMPLE ATTRITION
# ==============================================================================

sample_attrition_revised <- tibble(
  step = c(
    "All NHANES 2011-2018 participants",
    "Age 30-59",
    "Age 30-59 with positive fasting subsample weight",
    "Age 30-59 with revised complete primary CKM staging",
    "Revised complete CKM staging + positive fasting weight",
    "Final external sample: revised staging + positive fasting weight + 7 adiposity variables"
  ),

  n = c(
    nrow(nhanes_all),

    nrow(nhanes_30_59),

    sum(
      !is.na(nhanes_30_59$WTSAF8YR) &
        nhanes_30_59$WTSAF8YR > 0,
      na.rm = TRUE
    ),

    sum(
      nhanes_30_59$complete_staging_primary_revised,
      na.rm = TRUE
    ),

    sum(
      nhanes_30_59$complete_staging_primary_revised &
        !is.na(nhanes_30_59$WTSAF8YR) &
        nhanes_30_59$WTSAF8YR > 0,
      na.rm = TRUE
    ),

    nrow(nhanes_external_ready_revised)
  )
) %>%
  mutate(
    percent_of_previous = c(
      100,
      100 * n[-1] / n[-length(n)]
    ),

    percent_of_all =
      100 * n / first(n)
  )

readr::write_csv(
  sample_attrition_revised,
  file.path(
    DIR_QC,
    "sample_attrition_revised.csv"
  )
)


# ==============================================================================
# 17. SAVE REVISED PROCESSED OBJECTS FOR THE NEXT ANALYSIS STEP
#
# These files are not additional analyses. They simply preserve the corrected
# staging so that PCA/external-replication analyses can start from the revised
# dataset without rerunning PREVENT.
# ==============================================================================

saveRDS(
  nhanes_30_59,
  file.path(
    DIR_PROCESSED,
    "NHANES_2011_2018_age30_59_CKM_staged_REVISED.rds"
  ),
  compress = "xz"
)

saveRDS(
  nhanes_external_ready_revised,
  file.path(
    DIR_PROCESSED,
    "NHANES_2011_2018_external_validation_ready_REVISED.rds"
  ),
  compress = "xz"
)


# ==============================================================================
# 18. CONSOLE SUMMARY
# ==============================================================================

message("")
message("============================================================")
message("REVISED QC COMPLETE")
message("============================================================")

message(
  "Revised PREVENT QC:\n  ",
  file.path(DIR_QC, "PREVENT_primary_revised_QC.csv")
)

message(
  "Revised CKM stage distribution:\n  ",
  file.path(DIR_QC, "CKM_stage_distribution_primary_revised.csv")
)

message(
  "Revised attrition table:\n  ",
  file.path(DIR_QC, "sample_attrition_revised.csv")
)

message(
  "Revised staged RDS:\n  ",
  file.path(
    DIR_PROCESSED,
    "NHANES_2011_2018_age30_59_CKM_staged_REVISED.rds"
  )
)

message(
  "Revised external-ready RDS:\n  ",
  file.path(
    DIR_PROCESSED,
    "NHANES_2011_2018_external_validation_ready_REVISED.rds"
  )
)

message("")
message(
  "Final revised external sample n = ",
  nrow(nhanes_external_ready_revised)
)

message(
  "Advanced CKM (Stage 3-4) n = ",
  sum(
    nhanes_external_ready_revised$advanced_ckm_primary_revised == 1,
    na.rm = TRUE
  )
)

message("")
message("Stage distribution:")
print(stage_distribution_revised)

message("")
message("Attrition:")
print(sample_attrition_revised)

message("")
message("PREVENT QC:")
print(prevent_primary_revised_qc)

message("============================================================")
