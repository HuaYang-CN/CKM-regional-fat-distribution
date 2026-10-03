#!/usr/bin/env Rscript

# v2 syntax fix (2026-09-12): repaired all malformed double-bracket indexing
# introduced as `x[ [ ... ] ]`; these are now valid `x[[ ... ]]` expressions.

# ==============================================================================
# 03_CKM_supplementary_tables_S1_S26_v8_reconciled.R
#
# Purpose
# -------
# Generate the COMPLETE Supplementary Tables S1-S26 for the current CKM
# manuscript from the locked analysis object produced by:
#
#   01_CKM_refactored_analysis_v6_fullsupp_support.R
#
# This script complements:
#
#   02_CKM_tables_figures_highimpact_v4_visualfix.R
#
# It does NOT change the locked PCA definition or the primary association,
# external-replication, or transportability results. It reconstructs
# presentation tables from the saved analysis object and, where required,
# uses the same raw longitudinal / NHANES alcohol files already specified in
# 00_CKM_refactored_config.R.
#
# CURRENT NUMBERING follows the latest manuscript:
#   S1  CKM staging criteria
#   S2  China PC loadings
#   S3  China PCA variance explained
#   S4  Crude and adjusted logistic regression
#   S5  Six-algorithm strict nested-CV performance
#   S6  Algorithm-selection frequency
#   S7  GBM comparator-model performance
#   S8  Continuous NRI / IDI
#   S9  Calibration metrics
#   S10 Repeated-measures baseline characteristics by transition group
#   S11 Within-progressor primary and sensitivity analyses
#   S12 Dynamic-reference longitudinal comparison
#   S13 NHANES external-cohort characteristics
#   S14 Alcohol harmonization by NHANES cycle
#   S15 China and NHANES PCA loadings
#   S16 Loading-vector similarity
#   S17 Bootstrap loading congruence
#   S18 NHANES PCA variance explained
#   S19 Primary external association + Stage 2-4 sensitivity
#   S20 Alcohol sensitivity
#   S21 PC2-by-sex interaction
#   S22 Leave-one-cycle-out robustness
#   S23 External weighted AUROC / Brier point estimates
#   S24 Primary vs age-adjusted PC associations
#   S25 PC2-by-age interactions
#   S26 Age-sensitivity analytic-sample audit
#
# Outputs
# -------
# Under:
#   <DIR_TABLE>/Supplementary_Tables_v4/
#
# the script writes:
#   - CKM_Supplementary_Tables_S1-S26.xlsx
#   - one CSV per table
#   - CKM_Supplementary_Tables_S1-S26.docx (if officer/flextable installed)
#   - Supplementary_Table_manifest.csv
#
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999)
options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)

# ==============================================================================
# 0. Locate and source configuration
# ==============================================================================

get_current_script_dir <- function() {
  frames <- sys.frames()
  ofiles <- vapply(
    frames,
    function(fr) {
      if (!is.null(fr$ofile) && length(fr$ofile) >= 1L) {
        as.character(fr$ofile[[1]])
      } else {
        NA_character_
      }
    },
    character(1)
  )
  ofiles <- ofiles[!is.na(ofiles) & nzchar(ofiles)]
  if (length(ofiles)) {
    return(
      dirname(
        normalizePath(
          tail(ofiles, 1L),
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    p <- sub("^--file=", "", file_arg[[1]])
    return(
      dirname(
        normalizePath(
          p,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }

  normalizePath(
    getwd(),
    winslash = "/",
    mustWork = FALSE
  )
}

SCRIPT_DIR <- get_current_script_dir()
CONFIG_NAME <- "00_CKM_refactored_config.R"

CONFIG_CANDIDATES <- unique(
  c(
    file.path(SCRIPT_DIR, CONFIG_NAME),
    file.path(getwd(), CONFIG_NAME),
    file.path(Sys.getenv("CKM_REPO_ROOT", unset = getwd()), "R", CONFIG_NAME)
  )
)

existing_config <- CONFIG_CANDIDATES[
  file.exists(CONFIG_CANDIDATES)
]

if (!length(existing_config)) {
  stop(
    paste0(
      "Could not find ", CONFIG_NAME, ".\nChecked:\n  ",
      paste(CONFIG_CANDIDATES, collapse = "\n  "),
      "\n\nPlace the config file in the same folder as this script ",
      "or edit CONFIG_CANDIDATES."
    ),
    call. = FALSE
  )
}

CONFIG_FILE <- normalizePath(
  existing_config[[1]],
  winslash = "/",
  mustWork = TRUE
)

message("Using config: ", CONFIG_FILE)
source(CONFIG_FILE, encoding = "UTF-8")

# ==============================================================================
# 1. Packages and locked result object
# ==============================================================================

core_pkgs <- c(
  "dplyr",
  "tidyr",
  "tibble",
  "openxlsx",
  "survey",
  "readxl",
  "haven",
  "pROC",
  "PRROC"
)

missing_pkgs <- core_pkgs[
  !vapply(
    core_pkgs,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_pkgs)) {
  stop(
    "Install required packages first: ",
    paste(missing_pkgs, collapse = ", "),
    call. = FALSE
  )
}

master_file <- file.path(
  DIR_ANALYSIS,
  "CKM_refactored_master_results.rds"
)

assert_file(
  master_file,
  "Refactored master-results RDS"
)

res <- readRDS(master_file)

if (is.null(res$supplementary)) {
  stop(
    paste0(
      "The locked master RDS does not contain full supplementary support.\n",
      "Run 01_CKM_refactored_analysis_v6_fullsupp_support.R first."
    ),
    call. = FALSE
  )
}

sup <- res$supplementary

OUT_DIR <- file.path(
  DIR_TABLE,
  "Supplementary_Tables_v4"
)

CSV_DIR <- file.path(
  OUT_DIR,
  "individual_csv"
)

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  CSV_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# ==============================================================================
# 2. General helpers
# ==============================================================================

format_p2 <- function(p) {
  ifelse(
    is.na(p),
    "NA",
    ifelse(
      p < 0.001,
      "<0.001",
      sprintf("%.3f", p)
    )
  )
}

fmt_or <- function(or, lo, hi, digits = 2) {
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f–%.", digits, "f)"),
    or,
    lo,
    hi
  )
}

fmt_mean_sd2 <- function(x, digits = 2) {
  x <- as.numeric(x)
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)"),
    mean(x, na.rm = TRUE),
    stats::sd(x, na.rm = TRUE)
  )
}

fmt_mean_sd_pm <- function(x, digits = 2) {
  x <- as.numeric(x)
  sprintf(
    paste0("%.", digits, "f ± %.", digits, "f"),
    mean(x, na.rm = TRUE),
    stats::sd(x, na.rm = TRUE)
  )
}

fmt_med_iqr2 <- function(x, digits = 2, bracket = FALSE) {
  q <- stats::quantile(
    x,
    c(.25, .50, .75),
    na.rm = TRUE,
    names = FALSE
  )

  if (bracket) {
    sprintf(
      paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"),
      q[2],
      q[1],
      q[3]
    )
  } else {
    sprintf(
      paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"),
      q[2],
      q[1],
      q[3]
    )
  }
}

choose_column_local <- function(
    dat,
    candidates,
    label,
    required = TRUE
) {
  nms <- names(dat)

  hit <- candidates[
    candidates %in% nms
  ]

  if (length(hit)) {
    return(hit[1])
  }

  key <- function(x) {
    gsub(
      "[^[:alnum:]一-龥]+",
      "",
      tolower(trimws(x))
    )
  }

  nk <- key(nms)
  ck <- key(candidates)

  for (k in ck) {
    idx <- which(nk == k)
    if (length(idx)) {
      return(nms[idx[1]])
    }
  }

  if (required) {
    stop(
      "Could not identify ",
      label,
      ". Available columns: ",
      paste(nms, collapse = ", "),
      call. = FALSE
    )
  }

  NA_character_
}

normalise_time_local <- function(x) {
  z <- tolower(trimws(as.character(x)))

  out <- rep(
    NA_integer_,
    length(z)
  )

  out[
    z %in% c(
      "0", "0.0",
      "baseline", "base",
      "t0", "visit 0", "visit0",
      "基线", "初始"
    )
  ] <- 0L

  out[
    z %in% c(
      "1", "1.0",
      "followup", "follow-up",
      "follow up", "t1",
      "visit 1", "visit1",
      "随访", "复查"
    )
  ] <- 1L

  bad <- unique(
    z[
      is.na(out) &
        !is.na(z) &
        nzchar(z)
    ]
  )

  if (length(bad)) {
    stop(
      "Unrecognized longitudinal time value(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }

  out
}

extract_glm_term <- function(fit, term) {
  sm <- summary(fit)$coefficients

  if (!term %in% rownames(sm)) {
    stop(
      "Term not found in glm: ",
      term,
      call. = FALSE
    )
  }

  b <- sm[term, 1]
  se <- sm[term, 2]
  p <- sm[term, ncol(sm)]

  data.frame(
    term = term,
    OR = exp(b),
    Lower = exp(b - 1.96 * se),
    Upper = exp(b + 1.96 * se),
    P = p,
    stringsAsFactors = FALSE
  )
}

extract_svy_term <- function(fit, term) {
  sm <- summary(fit)$coefficients

  if (!term %in% rownames(sm)) {
    stop(
      "Term not found in survey model: ",
      term,
      call. = FALSE
    )
  }

  b <- sm[term, 1]
  se <- sm[term, 2]
  p <- sm[term, ncol(sm)]

  ci <- tryCatch(
    stats::confint(fit)[term, ],
    error = function(e) c(
      b - 1.96 * se,
      b + 1.96 * se
    )
  )

  data.frame(
    term = term,
    OR = exp(b),
    Lower = exp(ci[1]),
    Upper = exp(ci[2]),
    P = p,
    stringsAsFactors = FALSE
  )
}

first_non_intercept_term <- function(fit) {
  rn <- rownames(
    summary(fit)$coefficients
  )
  rn <- setdiff(
    rn,
    "(Intercept)"
  )

  if (!length(rn)) {
    stop(
      "No non-intercept term found.",
      call. = FALSE
    )
  }

  rn[1]
}

safe_chisq_p <- function(x, g) {
  tab <- table(
    x,
    g,
    useNA = "no"
  )

  if (
    nrow(tab) < 2 ||
      ncol(tab) < 2
  ) {
    return(NA_real_)
  }

  expected <- tryCatch(
    suppressWarnings(
      stats::chisq.test(tab)$expected
    ),
    error = function(e) NULL
  )

  if (
    is.null(expected) ||
      any(expected < 5)
  ) {
    tryCatch(
      stats::fisher.test(tab)$p.value,
      error = function(e) NA_real_
    )
  } else {
    tryCatch(
      suppressWarnings(
        stats::chisq.test(tab)$p.value
      ),
      error = function(e) NA_real_
    )
  }
}

add_percent_ci <- function(est, lo, hi, digits = 1) {
  paste0(
    sprintf(
      paste0("%.", digits, "f"),
      100 * est
    ),
    "% (",
    sprintf(
      paste0("%.", digits, "f"),
      100 * lo
    ),
    "–",
    sprintf(
      paste0("%.", digits, "f"),
      100 * hi
    ),
    "%)"
  )
}

# ==============================================================================
# 3. Supplementary Table S1
# ==============================================================================

S1 <- data.frame(
  `CKM stage` = c(
    "Stage 0: No CKM risk factors",
    "Stage 1: Excess or dysfunctional adiposity",
    "Stage 2: Metabolic risk factors and CKD",
    "Stage 3: Subclinical CVD in CKM",
    "Stage 4: Clinical CVD in CKM"
  ),
  `Definition and stage-defining criteria` = c(
    paste0(
      "Individuals with normal BMI and waist circumference, normoglycemia, ",
      "normotension, a normal lipid profile, and no evidence of CKD or ",
      "subclinical or clinical CVD."
    ),
    paste0(
      "Individuals with overweight/obesity, abdominal obesity, or dysfunctional ",
      "adipose tissue, without other metabolic risk factors or CKD. Criteria ",
      "included BMI ≥25 kg/m² (or ≥23 kg/m² if Asian ancestry); waist ",
      "circumference ≥88/102 cm in women/men (or ≥80/90 cm in women/men if ",
      "Asian ancestry); or fasting blood glucose 100–124 mg/dL or HbA1c ",
      "5.7%–6.4%."
    ),
    paste0(
      "Individuals with metabolic risk factors (hypertriglyceridemia ",
      "[≥135 mg/dL], hypertension, metabolic syndrome, or diabetes) or CKD."
    ),
    paste0(
      "Subclinical ASCVD or subclinical HF among individuals with excess/",
      "dysfunctional adiposity, other metabolic risk factors, or CKD. ",
      "Subclinical ASCVD was principally diagnosed by coronary artery ",
      "calcification; subclinical atherosclerosis identified by coronary ",
      "catheterization or CT angiography also met criteria. Subclinical HF was ",
      "diagnosed by elevated cardiac biomarkers (NT-proBNP ≥125 pg/mL; ",
      "hs-troponin T ≥14 ng/L for women and ≥22 ng/L for men; hs-troponin I ",
      "≥10 ng/L for women and ≥12 ng/L for men) or by echocardiographic ",
      "parameters. Risk equivalents included very-high-risk CKD (G4/G5 or ",
      "very high risk per KDIGO) and high predicted 10-year CVD risk."
    ),
    paste0(
      "Clinical CVD (coronary heart disease, HF, stroke, peripheral artery ",
      "disease, or atrial fibrillation) among individuals with excess/",
      "dysfunctional adiposity, other CKM risk factors, or CKD. Stage 4a: no ",
      "kidney failure. Stage 4b: kidney failure present."
    )
  ),
  check.names = FALSE
)

# ==============================================================================
# 4. Supplementary Tables S2-S4: China PCA and association
# ==============================================================================

rot_ch <- res$china$pca_lock$rotation

S2 <- data.frame(
  Variable = rownames(rot_ch),
  PC1 = rot_ch[, "PC1"],
  PC2 = rot_ch[, "PC2"],
  row.names = NULL,
  check.names = FALSE
)

eig_ch <- res$china$pca_lock$fit$sdev^2
var_ch <- eig_ch / sum(eig_ch)

S3 <- data.frame(
  Principal_Component = paste0(
    "PC",
    seq_along(var_ch)
  ),
  Variance_Explained = var_ch,
  Cumulative_Variance = cumsum(var_ch),
  Variance_Explained_Percent = 100 * var_ch,
  Cumulative_Variance_Percent = 100 * cumsum(var_ch),
  check.names = FALSE
)

china <- res$china$raw_analysis

if (!"severe" %in% names(china)) {
  china$severe <- as.integer(
    as.character(china$group_factor) == "Severe"
  )
}

# Stored crude PC models.
crude_pc1 <- extract_glm_term(
  res$china$primary_models$pc1_crude,
  "PC1_Score"
)

crude_pc2 <- extract_glm_term(
  res$china$primary_models$pc2_crude,
  "PC2_Score"
)

# Reconstruct the three single-covariate crude models used in the supplement.
crude_sex_fit <- stats::glm(
  severe ~ Sex_f,
  family = stats::binomial(),
  data = china
)

crude_smoke_fit <- stats::glm(
  severe ~ Smoke_f,
  family = stats::binomial(),
  data = china
)

crude_alcohol_fit <- stats::glm(
  severe ~ Alcohol_f,
  family = stats::binomial(),
  data = china
)

crude_sex <- extract_glm_term(
  crude_sex_fit,
  first_non_intercept_term(
    crude_sex_fit
  )
)

crude_smoke <- extract_glm_term(
  crude_smoke_fit,
  first_non_intercept_term(
    crude_smoke_fit
  )
)

crude_alcohol <- extract_glm_term(
  crude_alcohol_fit,
  first_non_intercept_term(
    crude_alcohol_fit
  )
)

adj_fit <- res$china$primary_models$adjusted

adjusted_terms <- rownames(
  summary(adj_fit)$coefficients
)

find_term <- function(pattern) {
  z <- grep(
    pattern,
    adjusted_terms,
    value = TRUE
  )

  if (!length(z)) {
    stop(
      "Adjusted-model term not found for pattern: ",
      pattern,
      call. = FALSE
    )
  }

  z[1]
}

adj_pc1 <- extract_glm_term(
  adj_fit,
  "PC1_Score"
)

adj_pc2 <- extract_glm_term(
  adj_fit,
  "PC2_Score"
)

adj_sex <- extract_glm_term(
  adj_fit,
  find_term("^Sex_f")
)

adj_smoke <- extract_glm_term(
  adj_fit,
  find_term("^Smoke_f")
)

adj_alcohol <- extract_glm_term(
  adj_fit,
  find_term("^Alcohol_f")
)

make_s4_row <- function(
    label,
    crude,
    adj
) {
  data.frame(
    Variable = label,
    Crude_OR_95CI = fmt_or(
      crude$OR,
      crude$Lower,
      crude$Upper
    ),
    Crude_P = format_p2(
      crude$P
    ),
    Adjusted_OR_95CI = fmt_or(
      adj$OR,
      adj$Lower,
      adj$Upper
    ),
    Adjusted_P = format_p2(
      adj$P
    ),
    check.names = FALSE
  )
}

S4 <- dplyr::bind_rows(
  make_s4_row(
    "Alcohol use",
    crude_alcohol,
    adj_alcohol
  ),
  make_s4_row(
    "PC1 score",
    crude_pc1,
    adj_pc1
  ),
  make_s4_row(
    "PC2 score",
    crude_pc2,
    adj_pc2
  ),
  make_s4_row(
    "Sex",
    crude_sex,
    adj_sex
  ),
  make_s4_row(
    "Current smoking",
    crude_smoke,
    adj_smoke
  )
)

# ==============================================================================
# 5. Supplementary Tables S5-S9: internal secondary performance
# ==============================================================================

if (is.null(res$ml) || is.null(sup$ml)) {
  stop(
    paste0(
      "ML/supplementary ML results are missing from the master RDS.\n",
      "Run Stage 01 with RUN_INTERNAL_ML=TRUE and RUN_NRI_IDI=TRUE."
    ),
    call. = FALSE
  )
}

# ---- S5: six-algorithm benchmark ---------------------------------------------

oof_alg <- res$ml$oof_algorithms
y_alg <- as.integer(
  oof_alg$Outcome == "Severe"
)

algorithms <- c(
  "LR",
  "RF",
  "GBM",
  "SVM",
  "ElasticNet",
  "KNN"
)

S5 <- dplyr::bind_rows(
  lapply(
    algorithms,
    function(nm) {
      p <- oof_alg[[paste0("prob_", nm)]]

      ok <- is.finite(
        y_alg
      ) & is.finite(p)

      rr <- pROC::roc(
        y_alg[ok],
        p[ok],
        levels = c(0, 1),
        direction = "<",
        quiet = TRUE
      )

      ci <- suppressWarnings(
        as.numeric(
          pROC::ci.auc(
            rr,
            method = "delong"
          )
        )
      )

      pr <- PRROC::pr.curve(
        scores.class0 = p[
          ok & y_alg == 1
        ],
        scores.class1 = p[
          ok & y_alg == 0
        ],
        curve = FALSE
      )

      brier <- mean(
        (
          y_alg[ok] -
            p[ok]
        )^2
      )

      data.frame(
        Model = nm,
        AUC = as.numeric(
          pROC::auc(rr)
        ),
        AUC_Lower = ci[1],
        AUC_Upper = ci[3],
        AUPRC = pr$auc.integral,
        Brier = brier,
        RMSE = sqrt(brier),
        MAE = mean(
          abs(
            y_alg[ok] -
              p[ok]
          )
        ),
        stringsAsFactors = FALSE
      )
    }
  )
)

# ---- S6: inner-CV selection frequency ----------------------------------------

S6 <- sup$ml$selection_frequency |>
  dplyr::filter(
    Selected_N_Folds > 0
  ) |>
  dplyr::transmute(
    Algorithm = Model,
    Selected_N_Folds = Selected_N_Folds,
    Selection_Percent = 100 *
      Selected_N_Folds /
      sum(Selected_N_Folds)
  ) |>
  dplyr::arrange(
    dplyr::desc(
      Selected_N_Folds
    )
  )

# ---- S7: GBM comparator performance ------------------------------------------

perf7 <- sup$ml$comparator_performance
diff7 <- sup$ml$comparator_auc_differences

display_model <- c(
  Baseline = "Baseline",
  BMI = "+BMI",
  VFA = "+VFA",
  FDP = "+FDP"
)

S7 <- dplyr::bind_rows(
  lapply(
    c(
      "Baseline",
      "BMI",
      "VFA",
      "FDP"
    ),
    function(nm) {
      z <- perf7[
        perf7$Model == nm,
        ,
        drop = FALSE
      ]

      if (nm == "FDP") {
        delta <- "Reference"
        ptxt <- "—"
      } else {
        d <- diff7[
          diff7$Reference == nm,
          ,
          drop = FALSE
        ]

        delta <- sprintf(
          "%.3f",
          d$Delta_AUROC_FDP_minus_ref
        )

        ptxt <- format_p2(
          d$P
        )
      }

      data.frame(
        Model = unname(
          display_model[nm]
        ),
        AUROC_95CI = sprintf(
          "%.3f (%.3f–%.3f)",
          z$AUROC,
          z$AUROC_Lower,
          z$AUROC_Upper
        ),
        Delta_AUROC_vs_FDP = delta,
        DeLong_P = ptxt,
        Brier_Score = sprintf(
          "%.4f",
          z$Brier
        ),
        check.names = FALSE
      )
    }
  )
)

# ---- S8: NRI / IDI with deterministic stratified bootstrap ------------------

continuous_nri_idi_local <- function(
    y,
    p_ref,
    p_new
) {
  d <- p_new - p_ref

  event <- y == 1
  nonevent <- y == 0

  nri_event <- mean(
    d[event] > 0
  ) - mean(
    d[event] < 0
  )

  nri_nonevent <- mean(
    d[nonevent] < 0
  ) - mean(
    d[nonevent] > 0
  )

  disc_ref <- mean(
    p_ref[event]
  ) - mean(
    p_ref[nonevent]
  )

  disc_new <- mean(
    p_new[event]
  ) - mean(
    p_new[nonevent]
  )

  c(
    NRI_Event = nri_event,
    NRI_Nonevent = nri_nonevent,
    Continuous_NRI = nri_event +
      nri_nonevent,
    IDI = disc_new -
      disc_ref
  )
}

oof_comp <- res$ml$oof_comparators
y_comp <- as.integer(
  oof_comp$Outcome == "Severe"
)

N_RECLASS_BOOT <- 2000L

set.seed(
  SEED_MAIN +
    7200L
)

event_idx <- which(
  y_comp == 1
)

nonevent_idx <- which(
  y_comp == 0
)

S8_rows <- list()

for (ref in c(
  "Baseline",
  "BMI",
  "VFA"
)) {
  p_ref <- oof_comp[[paste0(
      "prob_",
      ref
    )]]

  p_new <- oof_comp$prob_FDP

  point <- continuous_nri_idi_local(
    y_comp,
    p_ref,
    p_new
  )

  boot <- matrix(
    NA_real_,
    nrow = N_RECLASS_BOOT,
    ncol = length(point),
    dimnames = list(
      NULL,
      names(point)
    )
  )

  for (b in seq_len(
    N_RECLASS_BOOT
  )) {
    ii <- c(
      sample(
        event_idx,
        length(event_idx),
        replace = TRUE
      ),
      sample(
        nonevent_idx,
        length(nonevent_idx),
        replace = TRUE
      )
    )

    boot[b, ] <- continuous_nri_idi_local(
      y_comp[ii],
      p_ref[ii],
      p_new[ii]
    )
  }

  for (metric in names(
    point
  )) {
    lo <- stats::quantile(
      boot[, metric],
      .025,
      na.rm = TRUE
    )

    hi <- stats::quantile(
      boot[, metric],
      .975,
      na.rm = TRUE
    )

    # Empirical two-sided bootstrap sign probability.
    p_boot <- min(
      1,
      2 * min(
        (
          sum(
            boot[, metric] <= 0,
            na.rm = TRUE
          ) + 1
        ) /
          (
            sum(
              is.finite(
                boot[, metric]
              )
            ) + 1
          ),
        (
          sum(
            boot[, metric] >= 0,
            na.rm = TRUE
          ) + 1
        ) /
          (
            sum(
              is.finite(
                boot[, metric]
              )
            ) + 1
          )
      )
    )

    S8_rows[[length(
        S8_rows
      ) + 1L]] <- data.frame(
      Comparison = paste0(
        "FDP vs ",
        ref
      ),
      Metric = metric,
      Estimate = unname(
        point[metric]
      ),
      CI_Lower = as.numeric(
        lo
      ),
      CI_Upper = as.numeric(
        hi
      ),
      Bootstrap_P = p_boot,
      Estimate_95CI = sprintf(
        "%.3f (%.3f to %.3f)",
        point[metric],
        lo,
        hi
      ),
      P_Value = format_p2(
        p_boot
      ),
      check.names = FALSE
    )
  }
}

S8 <- dplyr::bind_rows(
  S8_rows
)

# ---- S9: calibration metrics -------------------------------------------------

calibration_point_local <- function(
    y,
    p
) {
  eps <- 1e-6

  p <- pmin(
    pmax(
      as.numeric(p),
      eps
    ),
    1 - eps
  )

  y <- as.integer(y)

  ok <- is.finite(
    y
  ) & is.finite(p)

  y <- y[ok]
  p <- p[ok]

  lp <- stats::qlogis(
    p
  )

  fit_intercept <- stats::glm(
    y ~ 1 +
      offset(lp),
    family = stats::binomial()
  )

  fit_recal <- stats::glm(
    y ~ lp,
    family = stats::binomial()
  )

  # Flexible calibration error.
  dd <- data.frame(
    y = y,
    p = p
  )

  lo <- tryCatch(
    stats::loess(
      y ~ p,
      data = dd,
      span = .75,
      degree = 1,
      control = stats::loess.control(
        surface = "direct"
      )
    ),
    error = function(e) NULL
  )

  if (is.null(lo)) {
    calibrated <- rep(
      mean(y),
      length(y)
    )
  } else {
    calibrated <- as.numeric(
      stats::predict(
        lo,
        newdata = data.frame(
          p = p
        )
      )
    )

    fallback <- mean(
      y
    )

    calibrated[
      !is.finite(calibrated)
    ] <- fallback
  }

  calibrated <- pmin(
    pmax(
      calibrated,
      0
    ),
    1
  )

  abs_err <- abs(
    calibrated -
      p
  )

  c(
    Calibration_Intercept = unname(
      stats::coef(
        fit_intercept
      )[1]
    ),
    Calibration_Slope = unname(
      stats::coef(
        fit_recal
      )["lp"]
    ),
    Recalibration_Intercept = unname(
      stats::coef(
        fit_recal
      )[1]
    ),
    Brier = mean(
      (
        y -
          p
      )^2
    ),
    ICI = mean(
      abs_err
    ),
    E50 = stats::quantile(
      abs_err,
      .50,
      names = FALSE
    ),
    E90 = stats::quantile(
      abs_err,
      .90,
      names = FALSE
    )
  )
}

N_CAL_BOOT <- 2000L
set.seed(
  SEED_MAIN +
    7300L
)

S9_rows <- list()

for (nm in c(
  "Baseline",
  "BMI",
  "FDP",
  "VFA"
)) {
  p <- oof_comp[[paste0(
      "prob_",
      nm
    )]]

  point <- calibration_point_local(
    y_comp,
    p
  )

  boot <- matrix(
    NA_real_,
    nrow = N_CAL_BOOT,
    ncol = 2,
    dimnames = list(
      NULL,
      c(
        "Calibration_Intercept",
        "Calibration_Slope"
      )
    )
  )

  for (b in seq_len(
    N_CAL_BOOT
  )) {
    ii <- sample.int(
      length(y_comp),
      length(y_comp),
      replace = TRUE
    )

    z <- tryCatch(
      calibration_point_local(
        y_comp[ii],
        p[ii]
      ),
      error = function(e) {
        rep(
          NA_real_,
          7
        )
      }
    )

    boot[b, ] <- z[
      c(
        "Calibration_Intercept",
        "Calibration_Slope"
      )
    ]
  }

  int_ci <- stats::quantile(
    boot[
      ,
      "Calibration_Intercept"
    ],
    c(.025, .975),
    na.rm = TRUE
  )

  slope_ci <- stats::quantile(
    boot[
      ,
      "Calibration_Slope"
    ],
    c(.025, .975),
    na.rm = TRUE
  )

  S9_rows[[length(
      S9_rows
    ) + 1L]] <- data.frame(
    Model = nm,
    Calibration_Intercept = point[
      "Calibration_Intercept"
    ],
    Calibration_Slope = point[
      "Calibration_Slope"
    ],
    Recalibration_Intercept = point[
      "Recalibration_Intercept"
    ],
    Brier = point[
      "Brier"
    ],
    ICI = point[
      "ICI"
    ],
    E50 = point[
      "E50"
    ],
    E90 = point[
      "E90"
    ],
    CI_Lower_Calibration_Intercept = int_ci[1],
    CI_Upper_Calibration_Intercept = int_ci[2],
    CI_Lower_Calibration_Slope = slope_ci[1],
    CI_Upper_Calibration_Slope = slope_ci[2],
    check.names = FALSE
  )
}

S9 <- dplyr::bind_rows(
  S9_rows
)

# ==============================================================================
# 6. Supplementary Table S10: repeated-measures baseline characteristics
# ==============================================================================

assert_file(
  CHINA_LONG_FILE,
  "Repeated-measures source file"
)

long_raw <- readxl::read_excel(
  CHINA_LONG_FILE,
  .name_repair = "minimal"
)

long_raw <- as.data.frame(
  long_raw,
  check.names = FALSE
)

id_col <- choose_column_local(
  long_raw,
  c(
    "ID",
    "id",
    "subject_id",
    "Subject_ID",
    "patient_id",
    "Patient_ID",
    "编号",
    "患者编号",
    "病例号"
  ),
  "longitudinal ID"
)

time_col <- choose_column_local(
  long_raw,
  c(
    "time",
    "Time",
    "timepoint",
    "Timepoint",
    "visit",
    "Visit",
    "时间点",
    "随访时间点",
    "时间"
  ),
  "longitudinal time"
)

long_raw$.ID <- as.character(
  long_raw[[id_col]]
)

long_raw$.time <- normalise_time_local(
  long_raw[[time_col]]
)

baseline <- long_raw[
  long_raw$.time == 0,
  ,
  drop = FALSE
]

transition <- res$longitudinal$paired[
  ,
  c(
    "ID",
    "Progression_Status"
  ),
  drop = FALSE
]

transition$ID <- as.character(
  transition$ID
)

baseline <- dplyr::left_join(
  baseline,
  transition,
  by = c(
    ".ID" = "ID"
  )
)

baseline <- baseline[
  baseline$Progression_Status %in%
    c(
      "Progressor",
      "Stable Severe"
    ),
  ,
  drop = FALSE
]

baseline$Transition <- factor(
  baseline$Progression_Status,
  levels = c(
    "Progressor",
    "Stable Severe"
  )
)

# Add locked PCA scores at baseline.
for (v in CHINA_FAT_VARS) {
  baseline[[v]] <- as.numeric(
    baseline[[v]]
  )
}

pc_base <- stats::predict(
  res$china$pca_lock$fit,
  newdata = baseline[
    ,
    CHINA_FAT_VARS,
    drop = FALSE
  ]
)

baseline$PC1_Score <- pc_base[
  ,
  "PC1"
]

baseline$PC2_Score <- pc_base[
  ,
  "PC2"
]

# Candidate source columns.
col_age <- choose_column_local(
  baseline,
  c(
    "Age",
    "age",
    "年龄"
  ),
  "Age"
)

col_sex <- choose_column_local(
  baseline,
  c(
    "Sex",
    "sex",
    "性别"
  ),
  "Sex"
)

col_smoke <- choose_column_local(
  baseline,
  c(
    "Smoke",
    "smoke",
    "Smoking",
    "吸烟"
  ),
  "Smoke"
)

col_alcohol <- choose_column_local(
  baseline,
  c(
    "Alcohol Use",
    "Alcohol_Use",
    "Alcohol",
    "alcohol",
    "饮酒"
  ),
  "Alcohol use"
)

continuous_candidates <- list(
  `Age, years` = col_age,
  `SBP, mmHg` = choose_column_local(
    baseline,
    c("SBP"),
    "SBP",
    FALSE
  ),
  `DBP, mmHg` = choose_column_local(
    baseline,
    c("DBP"),
    "DBP",
    FALSE
  ),
  `TC, mmol/L` = choose_column_local(
    baseline,
    c("TC"),
    "TC",
    FALSE
  ),
  `TG, mmol/L` = choose_column_local(
    baseline,
    c("TG"),
    "TG",
    FALSE
  ),
  `HDL-C, mmol/L` = choose_column_local(
    baseline,
    c("HDL-C", "HDL_C", "HDLC"),
    "HDL-C",
    FALSE
  ),
  `LDL-C, mmol/L` = choose_column_local(
    baseline,
    c("LDL-C", "LDL_C", "LDLC"),
    "LDL-C",
    FALSE
  ),
  `BMI, kg/m²` = "BMI",
  `VFA, cm²` = "VFA",
  `PC1 score` = "PC1_Score",
  `PC2 score` = "PC2_Score"
)

continuous_candidates <- continuous_candidates[
  !vapply(
    continuous_candidates,
    function(x) {
      is.na(x)
    },
    logical(1)
  )
]

median_vars <- c(
  "TG, mmol/L",
  "PC1 score",
  "PC2 score"
)

S10_rows <- list()

for (
  label in names(
    continuous_candidates
  )
) {
  v <- continuous_candidates[[label]]

  x <- as.numeric(
    baseline[[v]]
  )

  g <- baseline$Transition

  if (label %in% median_vars) {
    overall_txt <- fmt_med_iqr2(
      x,
      2,
      TRUE
    )

    prog_txt <- fmt_med_iqr2(
      x[g == "Progressor"],
      2,
      TRUE
    )

    stable_txt <- fmt_med_iqr2(
      x[g == "Stable Severe"],
      2,
      TRUE
    )

    p <- tryCatch(
      stats::wilcox.test(
        x ~ g,
        exact = FALSE
      )$p.value,
      error = function(e) NA_real_
    )
  } else {
    overall_txt <- fmt_mean_sd2(
      x,
      2
    )

    prog_txt <- fmt_mean_sd2(
      x[g == "Progressor"],
      2
    )

    stable_txt <- fmt_mean_sd2(
      x[g == "Stable Severe"],
      2
    )

    p <- tryCatch(
      stats::t.test(
        x ~ g
      )$p.value,
      error = function(e) NA_real_
    )
  }

  S10_rows[[length(
      S10_rows
    ) + 1L]] <- data.frame(
    Characteristic = label,
    Overall = overall_txt,
    `Progressors (n=37)` = prog_txt,
    `Stable severe (n=63)` = stable_txt,
    `P value` = format_p2(
      p
    ),
    check.names = FALSE
  )
}

# Categorical rows.
cat_specs <- list(
  `Sex, n (%)` = normalize_sex(
    baseline[[col_sex]]
  ),
  `Current smoking, n (%)` = normalize_binary_factor(
    baseline[[col_smoke]],
    variable = "Longitudinal smoking"
  ),
  `Alcohol use, n (%)` = normalize_binary_factor(
    baseline[[col_alcohol]],
    variable = "Longitudinal alcohol use"
  )
)

cat_table_rows <- list()

for (
  label in names(
    cat_specs
  )
) {
  f <- cat_specs[[label]]

  g <- baseline$Transition

  p <- safe_chisq_p(
    f,
    g
  )

  cat_table_rows[[length(
      cat_table_rows
    ) + 1L]] <- data.frame(
    Characteristic = label,
    Overall = "",
    `Progressors (n=37)` = "",
    `Stable severe (n=63)` = "",
    `P value` = format_p2(
      p
    ),
    check.names = FALSE
  )

  for (lv in levels(
    f
  )) {
    n_all <- sum(
      f == lv,
      na.rm = TRUE
    )

    n_prog <- sum(
      f == lv &
        g == "Progressor",
      na.rm = TRUE
    )

    n_stable <- sum(
      f == lv &
        g == "Stable Severe",
      na.rm = TRUE
    )

    d_all <- sum(
      !is.na(f)
    )

    d_prog <- sum(
      g == "Progressor" &
        !is.na(f)
    )

    d_stable <- sum(
      g == "Stable Severe" &
        !is.na(f)
    )

    cat_table_rows[[length(
        cat_table_rows
      ) + 1L]] <- data.frame(
      Characteristic = paste0(
        "  ",
        lv
      ),
      Overall = sprintf(
        "%d (%.1f)",
        n_all,
        100 * n_all /
          d_all
      ),
      `Progressors (n=37)` = sprintf(
        "%d (%.1f)",
        n_prog,
        100 * n_prog /
          d_prog
      ),
      `Stable severe (n=63)` = sprintf(
        "%d (%.1f)",
        n_stable,
        100 * n_stable /
          d_stable
      ),
      `P value` = "",
      check.names = FALSE
    )
  }
}

# Keep clinically intuitive ordering.
S10_cont <- dplyr::bind_rows(
  S10_rows
)

S10_cat <- dplyr::bind_rows(
  cat_table_rows
)

S10 <- dplyr::bind_rows(
  S10_cont[
    S10_cont$Characteristic == "Age, years",
    ,
    drop = FALSE
  ],
  S10_cat,
  S10_cont[
    S10_cont$Characteristic != "Age, years",
    ,
    drop = FALSE
  ]
)

# ==============================================================================
# 7. Supplementary Tables S11-S12: longitudinal change
# ==============================================================================

prog <- res$longitudinal$progressors
stable <- res$longitudinal$stable_severe

get_test_p <- function(name) {
  z <- res$longitudinal$tests[
    res$longitudinal$tests$Outcome == name,
    "P"
  ]

  if (!length(z)) {
    NA_real_
  } else {
    z[1]
  }
}

get_t_p <- function(name) {
  z <- res$longitudinal$t_sensitivity[
    res$longitudinal$t_sensitivity$Outcome == name,
    "P"
  ]

  if (!length(z)) {
    NA_real_
  } else {
    z[1]
  }
}

S11 <- data.frame(
  Variable = c(
    "PC1",
    "PC2"
  ),
  Baseline_Median_IQR = c(
    fmt_med_iqr2(
      prog$PC1_Score_0
    ),
    fmt_med_iqr2(
      prog$PC2_Score_0
    )
  ),
  Followup_Median_IQR = c(
    fmt_med_iqr2(
      prog$PC1_Score_1
    ),
    fmt_med_iqr2(
      prog$PC2_Score_1
    )
  ),
  Change_Median_IQR = c(
    fmt_med_iqr2(
      prog$Delta_PC1
    ),
    fmt_med_iqr2(
      prog$Delta_PC2
    )
  ),
  Wilcoxon_Signed_Rank_P = c(
    format_p2(
      get_test_p(
        "PC1 within progressors"
      )
    ),
    format_p2(
      get_test_p(
        "PC2 within progressors"
      )
    )
  ),
  Paired_t_Sensitivity_P = c(
    format_p2(
      get_t_p(
        "PC1 within progressors"
      )
    ),
    format_p2(
      get_t_p(
        "PC2 within progressors"
      )
    )
  ),
  check.names = FALSE
)

S12 <- data.frame(
  Variable = c(
    "ΔPC1",
    "ΔPC2"
  ),
  Progressors_Median_IQR = c(
    fmt_med_iqr2(
      prog$Delta_PC1
    ),
    fmt_med_iqr2(
      prog$Delta_PC2
    )
  ),
  Stable_Severe_Median_IQR = c(
    fmt_med_iqr2(
      stable$Delta_PC1
    ),
    fmt_med_iqr2(
      stable$Delta_PC2
    )
  ),
  Wilcoxon_Rank_Sum_P = c(
    format_p2(
      get_test_p(
        "Delta PC1: progressor vs stable severe"
      )
    ),
    format_p2(
      get_test_p(
        "Delta PC2: progressor vs stable severe"
      )
    )
  ),
  check.names = FALSE
)

# ==============================================================================
# 8. Build NHANES alcohol harmonization data used in S13-S14 and S20
# ==============================================================================

nh <- res$nhanes$data

alq_info <- data.frame(
  cycle = c(
    "2011-2012",
    "2013-2014",
    "2015-2016",
    "2017-2018"
  ),
  suffix = c(
    "G",
    "H",
    "I",
    "J"
  ),
  stringsAsFactors = FALSE
)

alq_info$file <- file.path(
  NHANES_RAW_DIR,
  paste0(
    "ALQ_",
    alq_info$suffix,
    ".XPT"
  )
)

alcohol_available <- all(
  file.exists(
    alq_info$file
  )
)

nh_alc <- nh

if (alcohol_available) {
  alq_list <- lapply(
    seq_len(
      nrow(
        alq_info
      )
    ),
    function(i) {
      z <- haven::read_xpt(
        alq_info$file[i]
      )

      names(z) <- toupper(
        names(z)
      )

      if (
        alq_info$suffix[i] %in%
          c(
            "G",
            "H",
            "I"
          )
      ) {
        if (!all(
          c(
            "SEQN",
            "ALQ120Q"
          ) %in% names(z)
        )) {
          stop(
            "Required alcohol variable ALQ120Q missing in ",
            basename(
              alq_info$file[i]
            ),
            call. = FALSE
          )
        }

        use <- dplyr::case_when(
          as.numeric(
            z$ALQ120Q
          ) == 0 ~ FALSE,
          as.numeric(
            z$ALQ120Q
          ) > 0 &
            !as.numeric(
              z$ALQ120Q
            ) %in%
            c(
              777,
              999
            ) ~ TRUE,
          TRUE ~ NA
        )
      } else {
        if (!all(
          c(
            "SEQN",
            "ALQ121"
          ) %in% names(z)
        )) {
          stop(
            "Required alcohol variable ALQ121 missing in ",
            basename(
              alq_info$file[i]
            ),
            call. = FALSE
          )
        }

        use <- dplyr::case_when(
          as.numeric(
            z$ALQ121
          ) == 0 ~ FALSE,
          as.numeric(
            z$ALQ121
          ) >= 1 &
            as.numeric(
              z$ALQ121
            ) <= 10 ~ TRUE,
          TRUE ~ NA
        )
      }

      data.frame(
        SEQN = as.numeric(
          z$SEQN
        ),
        cycle = alq_info$cycle[i],
        alcohol_past12m = use
      )
    }
  )

  alq <- dplyr::bind_rows(
    alq_list
  )

  nh_alc <- dplyr::left_join(
    nh,
    alq,
    by = c(
      "SEQN",
      "cycle"
    )
  )
} else {
  warning(
    paste0(
      "ALQ_G/H/I/J.XPT were not all found in NHANES_RAW_DIR.\n",
      "S13 will omit the alcohol row, S14 will contain a warning row, ",
      "and S20 will use saved alcohol sensitivity results if available."
    ),
    call. = FALSE
  )

  nh_alc$alcohol_past12m <- NA
}

# ==============================================================================
# 9. Supplementary Table S13: NHANES cohort characteristics
# ==============================================================================

nh_alc$male_ind <- as.numeric(
  as.character(
    nh_alc$sex_f
  ) == "Male"
)

nh_alc$smoke_ind <- as.numeric(
  as.character(
    nh_alc$smoke_f
  ) == "Yes"
)

nh_alc$alcohol_ind <- ifelse(
  is.na(
    nh_alc$alcohol_past12m
  ),
  NA_real_,
  as.numeric(
    nh_alc$alcohol_past12m
  )
)

for (s in 0:4) {
  nh_alc[[paste0(
      "stage_",
      s
    )]] <- as.numeric(
    nh_alc$stage ==
      s
  )
}

nh_alc$advanced_ind <- as.numeric(
  nh_alc$advanced_ckm ==
    1
)

des_nh_tab <- survey::svydesign(
  ids = ~PSU_8YR,
  strata = ~STRATA_8YR,
  weights = ~WTSAF8YR,
  nest = TRUE,
  data = nh_alc
)

svy_mean_ci_local <- function(
    design,
    var
) {
  f <- stats::as.formula(
    paste0(
      "~",
      var
    )
  )

  z <- survey::svymean(
    f,
    design,
    na.rm = TRUE
  )

  ci <- stats::confint(
    z
  )

  c(
    estimate = as.numeric(
      stats::coef(
        z
      )[1]
    ),
    lower = ci[
      1,
      1
    ],
    upper = ci[
      1,
      2
    ]
  )
}

svy_prop_ci_local <- function(
    design,
    var
) {
  svy_mean_ci_local(
    design,
    var
  )
}

S13_rows <- list()

add_nh_cont <- function(
    label,
    var,
    digits = 2
) {
  x <- nh_alc[[var]]

  z <- svy_mean_ci_local(
    des_nh_tab,
    var
  )

  data.frame(
    Characteristic = label,
    `Unweighted N` = sum(
      is.finite(
        x
      )
    ),
    `Unweighted summary` = fmt_mean_sd_pm(
      x,
      digits
    ),
    `Survey-weighted estimate (95% CI)` = sprintf(
      paste0(
        "%.",
        digits,
        "f (%.",
        digits,
        "f–%.",
        digits,
        "f)"
      ),
      z["estimate"],
      z["lower"],
      z["upper"]
    ),
    check.names = FALSE
  )
}

add_nh_binary <- function(
    label,
    var,
    denominator_filter = NULL
) {
  if (is.null(
    denominator_filter
  )) {
    keep <- !is.na(
      nh_alc[[var]]
    )
  } else {
    keep <- denominator_filter &
      !is.na(
        nh_alc[[var]]
      )
  }

  subdes <- subset(
    des_nh_tab,
    keep
  )

  z <- svy_prop_ci_local(
    subdes,
    var
  )

  x <- nh_alc[
    keep,
    var
  ]

  n1 <- sum(
    x == 1,
    na.rm = TRUE
  )

  nn <- sum(
    !is.na(
      x
    )
  )

  data.frame(
    Characteristic = label,
    `Unweighted N` = nn,
    `Unweighted summary` = sprintf(
      "%d/%d (%.1f%%)",
      n1,
      nn,
      100 * n1 /
        nn
    ),
    `Survey-weighted estimate (95% CI)` = add_percent_ci(
      z["estimate"],
      z["lower"],
      z["upper"],
      1
    ),
    check.names = FALSE
  )
}

S13_rows[[length(
    S13_rows
  ) + 1L]] <- add_nh_cont(
  "Age, years",
  "age",
  2
)

S13_rows[[length(
    S13_rows
  ) + 1L]] <- add_nh_binary(
  "Male sex",
  "male_ind"
)

S13_rows[[length(
    S13_rows
  ) + 1L]] <- add_nh_binary(
  "Current smoking",
  "smoke_ind"
)

if (alcohol_available) {
  S13_rows[[length(
      S13_rows
    ) + 1L]] <- add_nh_binary(
    "Past-12-month alcohol use",
    "alcohol_ind"
  )
}

for (
  sp in list(
    c(
      "BMI, kg/m²",
      "BMI"
    ),
    c(
      "Visceral adipose tissue area",
      "VAT_area"
    ),
    c(
      "Trunk fat, %",
      "Trunk_pct"
    ),
    c(
      "Left arm fat, %",
      "LA_pct"
    ),
    c(
      "Right arm fat, %",
      "RA_pct"
    ),
    c(
      "Left leg fat, %",
      "LL_pct"
    ),
    c(
      "Right leg fat, %",
      "RL_pct"
    )
  )
) {
  S13_rows[[length(
      S13_rows
    ) + 1L]] <- add_nh_cont(
    sp[1],
    sp[2],
    2
  )
}

for (s in 0:4) {
  S13_rows[[length(
      S13_rows
    ) + 1L]] <- add_nh_binary(
    paste0(
      "CKM Stage ",
      s
    ),
    paste0(
      "stage_",
      s
    )
  )
}

S13_rows[[length(
    S13_rows
  ) + 1L]] <- add_nh_binary(
  "Advanced CKM, Stage 3–4",
  "advanced_ind"
)

S13 <- dplyr::bind_rows(
  S13_rows
)

# ==============================================================================
# 10. Supplementary Table S14: alcohol harmonization by cycle
# ==============================================================================

if (alcohol_available) {
  S14 <- nh_alc |>
    dplyr::group_by(
      cycle
    ) |>
    dplyr::summarise(
      `n external` = dplyr::n(),
      `n alcohol observed` = sum(
        !is.na(
          alcohol_past12m
        )
      ),
      `alcohol complete percent` = 100 *
        sum(
          !is.na(
            alcohol_past12m
          )
        ) /
        dplyr::n(),
      `n current drinker` = sum(
        alcohol_past12m %in%
          TRUE,
        na.rm = TRUE
      ),
      `n no past12m drinking` = sum(
        alcohol_past12m %in%
          FALSE,
        na.rm = TRUE
      ),
      `n alcohol missing` = sum(
        is.na(
          alcohol_past12m
        )
      ),
      `advanced events total` = sum(
        advanced_ckm == 1,
        na.rm = TRUE
      ),
      `advanced events alcohol complete` = sum(
        advanced_ckm == 1 &
          !is.na(
            alcohol_past12m
          ),
        na.rm = TRUE
      ),
      .groups = "drop"
    )
} else {
  S14 <- data.frame(
    Message = paste0(
      "ALQ files were unavailable in NHANES_RAW_DIR: ",
      NHANES_RAW_DIR
    )
  )
}

# ==============================================================================
# 11. Supplementary Tables S15-S18: PCA replication
# ==============================================================================

lc <- res$nhanes$loading_comparison

canonical_to_source <- c(
  BMI = "BMI",
  VAT = "VAT_area",
  Trunk = "Trunk_pct",
  `Left arm` = "LA_pct",
  `Right arm` = "RA_pct",
  `Left leg` = "LL_pct",
  `Right leg` = "RL_pct"
)

S15 <- lc |>
  dplyr::transmute(
    Variable = unname(
      canonical_to_source[
        Variable
      ]
    ),
    China_PC1 = China_PC1,
    China_PC2 = China_PC2,
    NHANES_PC1 = NHANES_PC1,
    NHANES_PC2 = NHANES_PC2
  )

similarity_rows <- lapply(
  c(
    "PC1",
    "PC2"
  ),
  function(pc) {
    ch <- lc[[paste0(
        "China_",
        pc
      )]]

    nhv <- lc[[paste0(
        "NHANES_",
        pc
      )]]

    data.frame(
      Component = pc,
      Pearson_r = stats::cor(
        ch,
        nhv,
        method = "pearson"
      ),
      Spearman_rho = stats::cor(
        ch,
        nhv,
        method = "spearman"
      ),
      Tucker_phi = sum(
        ch *
          nhv
      ) /
        sqrt(
          sum(
            ch^2
          ) *
            sum(
              nhv^2
            )
        ),
      RMS_loading_difference = sqrt(
        mean(
          (
            ch -
              nhv
          )^2
        )
      ),
      NHANES_matched_component = pc,
      stringsAsFactors = FALSE
    )
  }
)

S16 <- dplyr::bind_rows(
  similarity_rows
)

boot_phi_raw <- res$nhanes$bootstrap_phi_raw
boot_phi_summary <- res$nhanes$bootstrap_phi

S17 <- dplyr::bind_rows(
  lapply(
    c(
      "PC1",
      "PC2"
    ),
    function(pc) {
      x <- boot_phi_raw[
        ,
        pc
      ]

      z <- boot_phi_summary[
        boot_phi_summary$Component ==
          pc,
        ,
        drop = FALSE
      ]

      data.frame(
        Component = pc,
        Point_Tucker_phi = z$Point,
        Bootstrap_mean = mean(
          x,
          na.rm = TRUE
        ),
        Lower_95 = stats::quantile(
          x,
          .025,
          na.rm = TRUE
        ),
        Upper_95 = stats::quantile(
          x,
          .975,
          na.rm = TRUE
        ),
        Proportion_phi_ge_0_90 = mean(
          x >= .90,
          na.rm = TRUE
        ),
        Proportion_phi_ge_0_95 = mean(
          x >= .95,
          na.rm = TRUE
        ),
        stringsAsFactors = FALSE
      )
    }
  )
)

eig_nh <- res$nhanes$pca$sdev^2
var_nh <- eig_nh /
  sum(
    eig_nh
  )

S18 <- data.frame(
  Component = paste0(
    "PC",
    seq_along(
      eig_nh
    )
  ),
  eigenvalue = eig_nh,
  variance_explained = var_nh,
  cumulative_variance = cumsum(
    var_nh
  ),
  check.names = FALSE
)

# ==============================================================================
# 12. Supplementary Table S19: external association + Stage 2-4
# ==============================================================================

m_nh <- res$nhanes$models$nhanes_pca
m_fixed <- res$nhanes$models$fixed
m_fixed234 <- res$nhanes$models$fixed_stage234

ext_rows <- list(
  list(
    Analysis = "NHANES-derived PCA: primary Stage 0–2 vs Stage 3–4",
    Effect = "PC1",
    z = extract_svy_term(
      m_nh,
      "PC1_z"
    )
  ),
  list(
    Analysis = "NHANES-derived PCA: primary Stage 0–2 vs Stage 3–4",
    Effect = "PC2",
    z = extract_svy_term(
      m_nh,
      "PC2_z"
    )
  ),
  list(
    Analysis = "Fixed China-loading vector transferred to NHANES",
    Effect = "PC1",
    z = extract_svy_term(
      m_fixed,
      "China_loading_PC1_z"
    )
  ),
  list(
    Analysis = "Fixed China-loading vector transferred to NHANES",
    Effect = "PC2",
    z = extract_svy_term(
      m_fixed,
      "China_loading_PC2_z"
    )
  ),
  list(
    Analysis = "Fixed China-loading vector: Stage 2 vs Stage 3–4",
    Effect = "PC1",
    z = extract_svy_term(
      m_fixed234,
      "China_loading_PC1_z"
    )
  ),
  list(
    Analysis = "Fixed China-loading vector: Stage 2 vs Stage 3–4",
    Effect = "PC2",
    z = extract_svy_term(
      m_fixed234,
      "China_loading_PC2_z"
    )
  )
)

S19 <- dplyr::bind_rows(
  lapply(
    ext_rows,
    function(a) {
      data.frame(
        Analysis = a$Analysis,
        Effect = a$Effect,
        `OR (95% CI)` = fmt_or(
          a$z$OR,
          a$z$Lower,
          a$z$Upper
        ),
        `P value` = format_p2(
          a$z$P
        ),
        check.names = FALSE
      )
    }
  )
)

# ==============================================================================
# 13. Supplementary Table S20: alcohol sensitivity
# ==============================================================================

if (!is.null(
  sup$alcohol_full_or
)) {
  S20 <- sup$alcohol_full_or |>
    dplyr::transmute(
      Analysis = Analysis,
      `OR (95% CI)` = fmt_or(
        OR,
        Lower,
        Upper
      ),
      `P value` = format_p2(
        P
      )
    )
} else if (!is.null(
  res$nhanes$alcohol_sensitivity
)) {
  a0 <- res$nhanes$alcohol_sensitivity$no_alcohol_adjustment
  a1 <- res$nhanes$alcohol_sensitivity$alcohol_adjusted

  S20 <- data.frame(
    Analysis = c(
      "Fixed China loading: same alcohol-complete sample, no alcohol adjustment",
      "Fixed China loading: alcohol-adjusted"
    ),
    `OR (95% CI)` = c(
      fmt_or(
        a0$OR,
        a0$Lower,
        a0$Upper
      ),
      fmt_or(
        a1$OR,
        a1$Lower,
        a1$Upper
      )
    ),
    `P value` = c(
      format_p2(
        a0$P
      ),
      format_p2(
        a1$P
      )
    ),
    check.names = FALSE
  )
} else {
  S20 <- data.frame(
    Message = paste0(
      "Alcohol sensitivity results were unavailable. ",
      "Ensure RUN_NHANES_ALCOHOL=TRUE and ALQ files are present."
    )
  )
}

# ==============================================================================
# 14. Supplementary Table S21: PC2-by-sex interaction
# ==============================================================================

sex_fit <- res$nhanes$models$fixed_sex_interaction

sex_terms <- rownames(
  summary(
    sex_fit
  )$coefficients
)

sex_term <- grep(
  "China_loading_PC2_z:sex_f|sex_f.*:China_loading_PC2_z",
  sex_terms,
  value = TRUE
)

if (!length(
  sex_term
)) {
  stop(
    "Could not identify PC2-by-sex interaction term.",
    call. = FALSE
  )
}

sex_z <- extract_svy_term(
  sex_fit,
  sex_term[1]
)

S21 <- data.frame(
  Term = sex_term[1],
  `OR (95% CI)` = fmt_or(
    sex_z$OR,
    sex_z$Lower,
    sex_z$Upper
  ),
  `P value` = format_p2(
    sex_z$P
  ),
  check.names = FALSE
)

# ==============================================================================
# 15. Supplementary Table S22: leave-one-cycle-out robustness
# ==============================================================================

cycles_all <- sort(
  unique(
    as.character(
      nh$cycle
    )
  )
)

primary_fixed <- res$nhanes$external_or[
  res$nhanes$external_or$Analysis ==
    "Fixed China-loading PC2",
  ,
  drop = FALSE
]

S22_primary <- data.frame(
  Analysis = "Exclude None",
  N = nrow(
    nh
  ),
  `Advanced CKM events` = sum(
    nh$advanced_ckm == 1,
    na.rm = TRUE
  ),
  `OR (95% CI)` = fmt_or(
    primary_fixed$OR,
    primary_fixed$Lower,
    primary_fixed$Upper
  ),
  `P value` = format_p2(
    primary_fixed$P
  ),
  `Included cycles` = paste(
    cycles_all,
    collapse = "; "
  ),
  check.names = FALSE
)

S22_loco <- res$nhanes$loco |>
  dplyr::rowwise() |>
  dplyr::mutate(
    Analysis = paste0(
      "Exclude ",
      Excluded_cycle
    ),
    `Advanced CKM events` = Events,
    `OR (95% CI)` = fmt_or(
      OR,
      Lower,
      Upper
    ),
    `P value` = format_p2(
      P
    ),
    `Included cycles` = paste(
      setdiff(
        cycles_all,
        as.character(
          Excluded_cycle
        )
      ),
      collapse = "; "
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    Analysis,
    N,
    `Advanced CKM events`,
    `OR (95% CI)`,
    `P value`,
    `Included cycles`
  )

S22 <- dplyr::bind_rows(
  S22_primary,
  S22_loco
)

# ==============================================================================
# 16. Supplementary Table S23: external weighted AUROC/Brier point estimates
# ==============================================================================

model_label_external <- c(
  Baseline = "Baseline",
  BMI = "+BMI",
  VAT = "+VAT",
  FDP = "+FDP"
)

S23 <- res$nhanes$performance |>
  dplyr::transmute(
    Model = unname(
      model_label_external[
        Model
      ]
    ),
    `Weighted AUROC` = Weighted_AUROC,
    `Weighted Brier score` = Weighted_Brier
  )

# ==============================================================================
# 17. Supplementary Table S24: primary vs age-adjusted associations
# ==============================================================================

attenuation_to_null <- function(
    primary_or,
    age_or
) {
  if (
    !is.finite(
      primary_or
    ) ||
      !is.finite(
        age_or
      ) ||
      primary_or <= 0 ||
      age_or <= 0 ||
      abs(
        log(
          primary_or
        )
      ) <
      1e-12
  ) {
    return(
      NA_real_
    )
  }

  100 *
    (
      abs(
        log(
          primary_or
        )
      ) -
        abs(
          log(
            age_or
          )
        )
    ) /
    abs(
      log(
        primary_or
      )
    )
}

# China
ch_primary_pc1 <- res$china$primary_or[
  res$china$primary_or$Exposure ==
    "PC1" &
    res$china$primary_or$Model ==
    "Adjusted",
  ,
  drop = FALSE
]

ch_primary_pc2 <- res$china$primary_or[
  res$china$primary_or$Exposure ==
    "PC2" &
    res$china$primary_or$Model ==
    "Adjusted",
  ,
  drop = FALSE
]

ch_age_pc1 <- extract_glm_term(
  res$china$age_sensitivity$fit,
  "PC1_Score"
)

ch_age_pc2 <- extract_glm_term(
  res$china$age_sensitivity$fit,
  "PC2_Score"
)

# NHANES-derived
nh_primary_pc1 <- res$nhanes$external_or[
  res$nhanes$external_or$Analysis ==
    "NHANES-derived PC1",
  ,
  drop = FALSE
]

nh_primary_pc2 <- res$nhanes$external_or[
  res$nhanes$external_or$Analysis ==
    "NHANES-derived PC2",
  ,
  drop = FALSE
]

nh_age_pc1 <- extract_svy_term(
  res$nhanes$models$age,
  "PC1_z"
)

nh_age_pc2 <- extract_svy_term(
  res$nhanes$models$age,
  "PC2_z"
)

# Fixed China-loading
fx_primary_pc1 <- res$nhanes$external_or[
  res$nhanes$external_or$Analysis ==
    "Fixed China-loading PC1",
  ,
  drop = FALSE
]

fx_primary_pc2 <- res$nhanes$external_or[
  res$nhanes$external_or$Analysis ==
    "Fixed China-loading PC2",
  ,
  drop = FALSE
]

fx_age_pc1 <- extract_svy_term(
  res$nhanes$models$fixed_age,
  "China_loading_PC1_z"
)

fx_age_pc2 <- extract_svy_term(
  res$nhanes$models$fixed_age,
  "China_loading_PC2_z"
)

make_age_compare_row <- function(
    cohort,
    definition,
    pc,
    primary,
    age
) {
  data.frame(
    Cohort = cohort,
    `Score definition` = definition,
    PC = pc,
    `Primary OR (95% CI)` = fmt_or(
      primary$OR,
      primary$Lower,
      primary$Upper
    ),
    `Age-adjusted OR (95% CI)` = fmt_or(
      age$OR,
      age$Lower,
      age$Upper
    ),
    `Age-adjusted P` = format_p2(
      age$P
    ),
    `Attenuation toward null, %` = sprintf(
      "%.1f",
      attenuation_to_null(
        primary$OR,
        age$OR
      )
    ),
    check.names = FALSE
  )
}

S24 <- dplyr::bind_rows(
  make_age_compare_row(
    "China derivation",
    "China-derived PCA",
    "PC1",
    ch_primary_pc1,
    ch_age_pc1
  ),
  make_age_compare_row(
    "China derivation",
    "China-derived PCA",
    "PC2",
    ch_primary_pc2,
    ch_age_pc2
  ),
  make_age_compare_row(
    "NHANES 2011–2018",
    "NHANES-derived PCA",
    "PC1",
    nh_primary_pc1,
    nh_age_pc1
  ),
  make_age_compare_row(
    "NHANES 2011–2018",
    "NHANES-derived PCA",
    "PC2",
    nh_primary_pc2,
    nh_age_pc2
  ),
  make_age_compare_row(
    "NHANES 2011–2018",
    "Fixed China-loading",
    "PC1",
    fx_primary_pc1,
    fx_age_pc1
  ),
  make_age_compare_row(
    "NHANES 2011–2018",
    "Fixed China-loading",
    "PC2",
    fx_primary_pc2,
    fx_age_pc2
  )
)

# ==============================================================================
# 18. Supplementary Table S25: PC2-by-age interaction
# ==============================================================================

age_int <- sup$age_interactions

S25 <- age_int |>
  dplyr::mutate(
    Cohort = dplyr::case_when(
      Label == "China derivation" ~
        "China derivation",
      TRUE ~
        "NHANES 2011–2018"
    ),
    `Score definition` = dplyr::case_when(
      Label == "China derivation" ~
        "China-derived PCA",
      Label == "NHANES-derived PC2" ~
        "NHANES-derived PCA",
      Label == "Fixed China-loading PC2" ~
        "Fixed China-loading",
      TRUE ~
        Label
    )
  ) |>
  dplyr::transmute(
    Cohort,
    `Score definition`,
    `Interaction OR per 10 years (95% CI)` = fmt_or(
      OR,
      Lower,
      Upper
    ),
    `P value` = format_p2(
      P
    )
  )

# ==============================================================================
# 19. Supplementary Table S26: age-sensitivity analytic-sample audit
# ==============================================================================

china_age <- as.numeric(
  china$Age
)

nh_age <- as.numeric(
  nh$age
)

S26 <- dplyr::bind_rows(
  data.frame(
    Cohort = "China derivation",
    `Score definition` = "China-derived PCA",
    `Input N` = nrow(
      china
    ),
    `Same-sample model N` = stats::nobs(
      res$china$age_sensitivity$fit
    ),
    Excluded = nrow(
      china
    ) -
      stats::nobs(
        res$china$age_sensitivity$fit
      ),
    Events = sum(
      china$severe == 1,
      na.rm = TRUE
    ),
    `Age, mean ± SD (range), years` = sprintf(
      "%.1f ± %.1f (%d–%d)",
      mean(
        china_age,
        na.rm = TRUE
      ),
      stats::sd(
        china_age,
        na.rm = TRUE
      ),
      floor(
        min(
          china_age,
          na.rm = TRUE
        )
      ),
      ceiling(
        max(
          china_age,
          na.rm = TRUE
        )
      )
    ),
    check.names = FALSE
  ),
  data.frame(
    Cohort = "NHANES 2011–2018",
    `Score definition` = "NHANES-derived PCA",
    `Input N` = nrow(
      nh
    ),
    `Same-sample model N` = stats::nobs(
      res$nhanes$models$age
    ),
    Excluded = nrow(
      nh
    ) -
      stats::nobs(
        res$nhanes$models$age
      ),
    Events = sum(
      nh$advanced_ckm == 1,
      na.rm = TRUE
    ),
    `Age, mean ± SD (range), years` = sprintf(
      "%.1f ± %.1f (%d–%d)",
      mean(
        nh_age,
        na.rm = TRUE
      ),
      stats::sd(
        nh_age,
        na.rm = TRUE
      ),
      floor(
        min(
          nh_age,
          na.rm = TRUE
        )
      ),
      ceiling(
        max(
          nh_age,
          na.rm = TRUE
        )
      )
    ),
    check.names = FALSE
  ),
  data.frame(
    Cohort = "NHANES 2011–2018",
    `Score definition` = "Fixed China-loading",
    `Input N` = nrow(
      nh
    ),
    `Same-sample model N` = stats::nobs(
      res$nhanes$models$fixed_age
    ),
    Excluded = nrow(
      nh
    ) -
      stats::nobs(
        res$nhanes$models$fixed_age
      ),
    Events = sum(
      nh$advanced_ckm == 1,
      na.rm = TRUE
    ),
    `Age, mean ± SD (range), years` = sprintf(
      "%.1f ± %.1f (%d–%d)",
      mean(
        nh_age,
        na.rm = TRUE
      ),
      stats::sd(
        nh_age,
        na.rm = TRUE
      ),
      floor(
        min(
          nh_age,
          na.rm = TRUE
        )
      ),
      ceiling(
        max(
          nh_age,
          na.rm = TRUE
        )
      )
    ),
    check.names = FALSE
  )
)

# ==============================================================================
# 20. Collect titles / notes / tables
# ==============================================================================

table_titles <- c(
  S1 = "CKM syndrome staging criteria used in the Chinese derivation and repeated-measures cohorts.",
  S2 = "PC1 and PC2 loadings in the Chinese derivation cohort.",
  S3 = "Variance explained by principal components in the Chinese derivation cohort.",
  S4 = "Crude and multivariable-adjusted logistic regression results.",
  S5 = "Strict nested cross-validation performance of the six candidate algorithms.",
  S6 = "Inner-cross-validation algorithm selection frequency.",
  S7 = "GBM comparator-model performance.",
  S8 = "Continuous NRI, IDI, event NRI, and non-event NRI.",
  S9 = "Calibration metrics for comparator models.",
  S10 = "Baseline characteristics of the exploratory repeated-measures cohort by transition group.",
  S11 = "Within-progressor primary and sensitivity analyses.",
  S12 = "Dynamic-reference comparison of longitudinal changes.",
  S13 = "Characteristics of the NHANES external-replication cohort.",
  S14 = "Alcohol harmonization by NHANES cycle.",
  S15 = "China and NHANES PCA loadings.",
  S16 = "Loading-vector similarity between the Chinese and NHANES cohorts.",
  S17 = "Bootstrap congruence of PCA loading vectors.",
  S18 = "Variance explained by principal components in NHANES.",
  S19 = "Primary survey-weighted external association and Stage 2-4 sensitivity models.",
  S20 = "Alcohol sensitivity analyses for the fixed China-loading PC2 association.",
  S21 = "PC2-by-sex interaction in NHANES.",
  S22 = "Leave-one-cycle-out robustness of the fixed China-loading PC2 association.",
  S23 = "External weighted AUROC and Brier-score point estimates for comparator models.",
  S24 = "Primary versus age-adjusted PC association estimates in the derivation and NHANES cohorts.",
  S25 = "Exploratory PC2-by-age interaction analyses.",
  S26 = "Age-sensitivity analytic-sample audit."
)

table_notes <- c(
  S1 = paste0(
    "Based on the 2023 American Heart Association CKM framework. ",
    "ASCVD, atherosclerotic cardiovascular disease; CKD, chronic kidney disease; ",
    "CKM, cardiovascular-kidney-metabolic; CVD, cardiovascular disease; HF, heart failure."
  ),
  S2 = paste0(
    "Trunk%, LA%, RA%, LL%, and RL% are InBody 770 segmental fat indices ",
    "expressed relative to the manufacturer-defined reference for the segment."
  ),
  S3 = "",
  S4 = paste0(
    "The adjusted model included PC1, PC2, sex, current smoking, and alcohol use simultaneously."
  ),
  S5 = "Metrics were calculated from pooled strict outer-fold predictions.",
  S6 = "Only algorithms selected in at least one outer fold are shown.",
  S7 = "All comparator models used identical outer folds; paired DeLong tests compare +FDP with each reference model.",
  S8 = paste0(
    "Continuous NRI and IDI use 2,000 deterministic stratified bootstrap resamples. ",
    "These are secondary reclassification analyses."
  ),
  S9 = paste0(
    "Calibration metrics use locked pooled out-of-fold predictions. ",
    "Calibration-intercept and slope confidence intervals use 2,000 bootstrap resamples."
  ),
  S10 = paste0(
    "Values are mean (SD), median [IQR], or n (%). Progressors transitioned from mild to severe CKM; ",
    "the stable-severe reference group was severe at both assessments."
  ),
  S11 = "P values are from paired Wilcoxon signed-rank tests; paired t tests are sensitivity analyses.",
  S12 = paste0(
    "The stable-severe group is a pathological dynamic reference group and is not a mild non-progressor control group."
  ),
  S13 = "Survey-weighted estimates use the combined 2011–2018 fasting subsample weight.",
  S14 = "Alcohol use denotes harmonized past-12-month drinking status across NHANES cycles.",
  S15 = paste0(
    "Chinese segmental variables are InBody reference-relative indices, whereas NHANES regional variables are ",
    "DXA-derived fat percentages. Loading comparisons use within-cohort centering and scaling."
  ),
  S16 = "",
  S17 = paste0(
    "Tucker coefficients were calculated after component-direction alignment; ",
    "bootstrap results are based on the locked NHANES resamples."
  ),
  S18 = "",
  S19 = "NHANES odds ratios for PC scores are expressed per 1 sampling-weighted SD.",
  S20 = "Alcohol sensitivity analyses are restricted to the harmonized alcohol-complete sample.",
  S21 = "The interaction OR represents modification of the fixed China-loading PC2 association by sex.",
  S22 = paste0(
    "Each leave-one-cycle-out analysis used the remaining three NHANES cycles and a combined six-year fasting subsample weight."
  ),
  S23 = paste0(
    "External model-performance analyses are secondary descriptive comparisons. ",
    "Weighted AUROC and Brier-score point estimates are reported without formal validation claims."
  ),
  S24 = paste0(
    "Primary and age-adjusted models were fitted to the same age-complete participants. ",
    "Positive attenuation values indicate movement of the absolute log-OR toward the null; ",
    "negative values indicate strengthening away from the null. China estimates are per 1 PC-score unit; ",
    "NHANES estimates are per 1 survey-weighted SD."
  ),
  S25 = paste0(
    "Age was centered at the cohort mean and scaled per 10 years. Interaction analyses were exploratory."
  ),
  S26 = "No participant should be lost solely because of age in the current locked analysis."
)

tables <- list(
  S1 = S1,
  S2 = S2,
  S3 = S3,
  S4 = S4,
  S5 = S5,
  S6 = S6,
  S7 = S7,
  S8 = S8,
  S9 = S9,
  S10 = S10,
  S11 = S11,
  S12 = S12,
  S13 = S13,
  S14 = S14,
  S15 = S15,
  S16 = S16,
  S17 = S17,
  S18 = S18,
  S19 = S19,
  S20 = S20,
  S21 = S21,
  S22 = S22,
  S23 = S23,
  S24 = S24,
  S25 = S25,
  S26 = S26
)

# ==============================================================================
# 21. Save one CSV per table
# ==============================================================================

for (nm in names(
  tables
)) {
  utils::write.csv(
    tables[[nm]],
    file.path(
      CSV_DIR,
      paste0(
        "Supplementary_Table_",
        nm,
        ".csv"
      )
    ),
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}

# ==============================================================================
# 22. Excel workbook with journal-style formatting
# ==============================================================================

wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 10,
  textDecoration = "bold",
  fgFill = "#EDEDED",
  halign = "center",
  valign = "center",
  border = "TopBottom",
  borderColour = "#404040",
  wrapText = TRUE
)

body_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 9,
  valign = "top",
  wrapText = TRUE
)

title_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 11,
  textDecoration = "bold",
  valign = "center",
  wrapText = TRUE
)

note_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 9,
  fontColour = "#505050",
  textDecoration = "italic",
  wrapText = TRUE,
  valign = "top"
)

for (nm in names(
  tables
)) {
  sheet_name <- nm

  openxlsx::addWorksheet(
    wb,
    sheet_name
  )

  title_txt <- paste0(
    "Supplementary Table ",
    nm,
    ". ",
    table_titles[nm]
  )

  openxlsx::writeData(
    wb,
    sheet_name,
    title_txt,
    startRow = 1,
    startCol = 1
  )

  openxlsx::addStyle(
    wb,
    sheet_name,
    title_style,
    rows = 1,
    cols = 1,
    gridExpand = TRUE
  )

  x <- tables[[nm]]

  openxlsx::writeData(
    wb,
    sheet_name,
    x,
    startRow = 3,
    startCol = 1,
    headerStyle = header_style
  )

  if (nrow(
    x
  ) > 0) {
    openxlsx::addStyle(
      wb,
      sheet_name,
      body_style,
      rows = 4:(
        3 +
          nrow(
            x
          )
      ),
      cols = seq_len(
        ncol(
          x
        )
      ),
      gridExpand = TRUE
    )
  }

  note_txt <- table_notes[nm]

  if (
    !is.na(
      note_txt
    ) &&
      nzchar(
        note_txt
      )
  ) {
    note_row <- 5 +
      nrow(
        x
      )

    openxlsx::writeData(
      wb,
      sheet_name,
      paste0(
        "Note: ",
        note_txt
      ),
      startRow = note_row,
      startCol = 1
    )

    openxlsx::addStyle(
      wb,
      sheet_name,
      note_style,
      rows = note_row,
      cols = 1,
      gridExpand = TRUE
    )
  }

  openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = 4
  )

  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_len(
      ncol(
        x
      )
    ),
    widths = "auto"
  )

  # Cap very wide auto-sized text columns.
  if (ncol(
    x
  ) >= 1) {
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = 1,
      widths = 28
    )
  }

  if (
    nm == "S1" &&
      ncol(
        x
      ) >= 2
  ) {
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = 2,
      widths = 90
    )
  }
}

xlsx_path <- file.path(
  OUT_DIR,
  "CKM_Supplementary_Tables_S1-S26.xlsx"
)

openxlsx::saveWorkbook(
  wb,
  xlsx_path,
  overwrite = TRUE
)

# ==============================================================================
# 23. Publication-ready Word supplementary-table document
# ==============================================================================
#
# WORD LAYOUT FIX v6
# ------------------
# This version is designed specifically to prevent horizontal clipping.
#
# Key changes:
#   1) FORCE A4 landscape using officer section properties.
#   2) Verify the written DOCX with officer::docx_dim(); stop if it is portrait.
#   3) Use fixed page-safe column widths. Never use autofit() for Word.
#   4) Use explicit multi-line headers for wide tables.
#   5) Wrap long BODY text with soft line breaks inside cells.
#   6) Use compact Word-only display versions for the widest statistical tables.
#   7) Keep CSV/XLSX outputs unchanged and numerically complete.
#   8) Use a unique output filename so an older portrait file is not mistaken
#      for the new landscape version.
#
# ==============================================================================

docx_written <- FALSE

if (
  requireNamespace("officer", quietly = TRUE) &&
  requireNamespace("flextable", quietly = TRUE)
) {

  # ---------------------------------------------------------------------------
  # 23.1 General Word helpers
  # ---------------------------------------------------------------------------

  format_num_word <- function(x, colname = "") {
    x <- as.numeric(x)

    if (
      grepl(
        "(^|[ _])P($|[ _])|P value|P_Value|Bootstrap_P|DeLong_P",
        colname,
        ignore.case = TRUE
      )
    ) {
      return(format_p2(x))
    }

    if (
      grepl(
        paste0(
          "(^N$|Unweighted N|Input N|model N|Same-sample model N|",
          "Excluded|Events|folds|Selected_N_Folds|^n | n )"
        ),
        colname,
        ignore.case = TRUE
      )
    ) {
      return(
        ifelse(
          is.na(x),
          "",
          format(
            round(x),
            big.mark = ",",
            scientific = FALSE,
            trim = TRUE
          )
        )
      )
    }

    if (
      grepl(
        "Percent|percentage|%",
        colname,
        ignore.case = TRUE
      )
    ) {
      return(
        ifelse(
          is.na(x),
          "",
          sprintf("%.1f", x)
        )
      )
    }

    if (
      grepl(
        "Brier",
        colname,
        ignore.case = TRUE
      )
    ) {
      return(
        ifelse(
          is.na(x),
          "",
          sprintf("%.4f", x)
        )
      )
    }

    ifelse(
      is.na(x),
      "",
      sprintf("%.3f", x)
    )
  }

  clean_word_colnames <- function(nms) {
    out <- nms
    out <- gsub("_", " ", out, fixed = TRUE)
    out <- gsub("95CI", "95% CI", out, fixed = TRUE)
    out <- gsub("OR 95% CI", "OR (95% CI)", out, fixed = TRUE)
    out <- gsub("AUROC 95% CI", "AUROC (95% CI)", out, fixed = TRUE)
    out <- gsub("P Value", "P value", out, fixed = TRUE)
    out <- gsub("Tucker phi", "Tucker φ", out, fixed = TRUE)
    out <- gsub("Spearman rho", "Spearman ρ", out, fixed = TRUE)
    out
  }

  wrap_one_cell <- function(x, width = 34L, machine_breaks = FALSE) {
    if (is.na(x) || !nzchar(as.character(x))) {
      return("")
    }

    z <- as.character(x)

    # Allow technical terms without spaces to break safely in Word.
    if (isTRUE(machine_breaks)) {
      z <- gsub(":", ":\n", z, fixed = TRUE)
      z <- gsub("_", "_\n", z, fixed = TRUE)
    }

    # Long cycle lists wrap cleanly after semicolons.
    z <- gsub("; ", ";\n", z, fixed = TRUE)

    # Preserve explicit breaks already inserted above.
    pieces <- strsplit(z, "\n", fixed = TRUE)[[1]]

    wrapped <- unlist(
      lapply(
        pieces,
        function(piece) {
          if (!nzchar(piece)) {
            return("")
          }
          w <- strwrap(
            piece,
            width = width,
            simplify = FALSE
          )
          if (!length(w)) piece else w
        }
      ),
      use.names = FALSE
    )

    paste(
      wrapped,
      collapse = "\n"
    )
  }

  wrap_vector <- function(
      x,
      width = 34L,
      machine_breaks = FALSE
  ) {
    vapply(
      x,
      wrap_one_cell,
      character(1),
      width = width,
      machine_breaks = machine_breaks
    )
  }

  # ---------------------------------------------------------------------------
  # 23.2 Prepare compact Word-only display tables
  # ---------------------------------------------------------------------------

  prepare_word_table <- function(nm, x) {
    y <- as.data.frame(
      x,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    # S5: combine AUROC point + CI, reducing 8 columns to 6.
    if (
      nm == "S5" &&
      all(
        c(
          "Model",
          "AUC",
          "AUC_Lower",
          "AUC_Upper",
          "AUPRC",
          "Brier",
          "RMSE",
          "MAE"
        ) %in% names(y)
      )
    ) {
      y <- data.frame(
        Model = y$Model,
        `AUROC (95% CI)` = sprintf(
          "%.3f (%.3f–%.3f)",
          y$AUC,
          y$AUC_Lower,
          y$AUC_Upper
        ),
        AUPRC = sprintf("%.3f", y$AUPRC),
        Brier = sprintf("%.4f", y$Brier),
        RMSE = sprintf("%.3f", y$RMSE),
        MAE = sprintf("%.3f", y$MAE),
        check.names = FALSE
      )
    }

    # S8: keep one estimate+CI column instead of separate estimate/lower/upper.
    if (
      nm == "S8" &&
      all(
        c(
          "Comparison",
          "Metric",
          "Estimate_95CI",
          "P_Value"
        ) %in% names(y)
      )
    ) {
      y <- data.frame(
        Comparison = y$Comparison,
        Metric = gsub("_", " ", y$Metric, fixed = TRUE),
        `Estimate (95% CI)` = y$Estimate_95CI,
        `P value` = y$P_Value,
        check.names = FALSE
      )
    }

    # S9: collapse CI columns into the calibration-estimate cells.
    if (
      nm == "S9" &&
      all(
        c(
          "Model",
          "Calibration_Intercept",
          "Calibration_Slope",
          "Recalibration_Intercept",
          "Brier",
          "ICI",
          "E50",
          "E90",
          "CI_Lower_Calibration_Intercept",
          "CI_Upper_Calibration_Intercept",
          "CI_Lower_Calibration_Slope",
          "CI_Upper_Calibration_Slope"
        ) %in% names(y)
      )
    ) {
      y <- data.frame(
        Model = y$Model,
        `Calibration intercept (95% CI)` = sprintf(
          "%.3f (%.3f–%.3f)",
          y$Calibration_Intercept,
          y$CI_Lower_Calibration_Intercept,
          y$CI_Upper_Calibration_Intercept
        ),
        `Calibration slope (95% CI)` = sprintf(
          "%.3f (%.3f–%.3f)",
          y$Calibration_Slope,
          y$CI_Lower_Calibration_Slope,
          y$CI_Upper_Calibration_Slope
        ),
        `Recalibration intercept` = sprintf(
          "%.3f",
          y$Recalibration_Intercept
        ),
        Brier = sprintf("%.4f", y$Brier),
        ICI = sprintf("%.3f", y$ICI),
        E50 = sprintf("%.3f", y$E50),
        E90 = sprintf("%.3f", y$E90),
        check.names = FALSE
      )
    }

    # S17: collapse bootstrap limits into one CI column.
    if (
      nm == "S17" &&
      all(
        c(
          "Component",
          "Point_Tucker_phi",
          "Bootstrap_mean",
          "Lower_95",
          "Upper_95",
          "Proportion_phi_ge_0_90",
          "Proportion_phi_ge_0_95"
        ) %in% names(y)
      )
    ) {
      y <- data.frame(
        Component = y$Component,
        `Tucker φ` = sprintf(
          "%.3f",
          y$Point_Tucker_phi
        ),
        `Bootstrap mean` = sprintf(
          "%.3f",
          y$Bootstrap_mean
        ),
        `95% CI` = sprintf(
          "%.3f–%.3f",
          y$Lower_95,
          y$Upper_95
        ),
        `φ ≥0.90, %` = sprintf(
          "%.1f",
          100 * y$Proportion_phi_ge_0_90
        ),
        `φ ≥0.95, %` = sprintf(
          "%.1f",
          100 * y$Proportion_phi_ge_0_95
        ),
        check.names = FALSE
      )
    }

    # Shorter S14 headers.
    if (nm == "S14") {
      rename14 <- c(
        "cycle" = "Cycle",
        "n external" = "External N",
        "n alcohol observed" = "Alcohol observed, n",
        "alcohol complete percent" = "Alcohol complete, %",
        "n current drinker" = "Current drinker, n",
        "n no past12m drinking" = "No past-12-month drinking, n",
        "n alcohol missing" = "Alcohol missing, n",
        "advanced events total" = "Advanced CKM events, n",
        "advanced events alcohol complete" =
          "Advanced events with alcohol data, n"
      )

      hit <- intersect(
        names(rename14),
        names(y)
      )

      names(y)[
        match(
          hit,
          names(y)
        )
      ] <- unname(
        rename14[hit]
      )
    }

    if (nm == "S14") {
      # Count columns are integers; keep the percentage column decimal-formatted.
      s14_count_cols <- intersect(
        c(
          "External N",
          "Alcohol observed, n",
          "Current drinker, n",
          "No past-12-month drinking, n",
          "Alcohol missing, n",
          "Advanced CKM events, n",
          "Advanced events with alcohol data, n"
        ),
        names(y)
      )
      for (cc in s14_count_cols) {
        y[[cc]] <- ifelse(
          is.na(y[[cc]]),
          "",
          format(round(as.numeric(y[[cc]])), big.mark = ",", scientific = FALSE, trim = TRUE)
        )
      }
    }

    if (nm == "S24") {
      names(y)[
        names(y) ==
          "Attenuation toward null, %"
      ] <- "Attenuation, %"
    }

    # Word-only numeric precision.
    for (j in seq_along(y)) {
      if (is.numeric(y[[j]])) {
        y[[j]] <- format_num_word(
          y[[j]],
          names(y)[j]
        )
      }
    }

    names(y) <- clean_word_colnames(
      names(y)
    )

    # -------------------------------------------------------------------------
    # Body-cell wrapping. This is intentionally presentation-only.
    # -------------------------------------------------------------------------

    if ("Definition and stage-defining criteria" %in% names(y)) {
      y[["Definition and stage-defining criteria"]] <- wrap_vector(
        y[["Definition and stage-defining criteria"]],
        width = 78L
      )
    }

    if ("CKM stage" %in% names(y)) {
      y[["CKM stage"]] <- wrap_vector(
        y[["CKM stage"]],
        width = 27L
      )
    }

    if ("Analysis" %in% names(y)) {
      y[["Analysis"]] <- wrap_vector(
        y[["Analysis"]],
        width = if (nm %in% c("S19", "S20")) 48L else 30L
      )
    }

    if ("Included cycles" %in% names(y)) {
      y[["Included cycles"]] <- wrap_vector(
        y[["Included cycles"]],
        width = 24L
      )
    }

    if ("Term" %in% names(y)) {
      y[["Term"]] <- wrap_vector(
        y[["Term"]],
        width = 24L,
        machine_breaks = TRUE
      )
    }

    if ("Cohort" %in% names(y)) {
      y[["Cohort"]] <- wrap_vector(
        y[["Cohort"]],
        width = 18L
      )
    }

    if ("Score definition" %in% names(y)) {
      y[["Score definition"]] <- wrap_vector(
        y[["Score definition"]],
        width = 18L
      )
    }

    if ("Characteristic" %in% names(y)) {
      y[["Characteristic"]] <- wrap_vector(
        y[["Characteristic"]],
        width = 30L
      )
    }

    # Generic safeguard for any remaining very long character cell.
    for (j in seq_along(y)) {
      if (is.character(y[[j]])) {
        cell_len <- nchar(
          y[[j]],
          type = "width"
        )
        if (
          any(
            cell_len > 55,
            na.rm = TRUE
          )
        ) {
          y[[j]] <- wrap_vector(
            y[[j]],
            width = 42L
          )
        }
      }
    }

    y
  }

  # ---------------------------------------------------------------------------
  # 23.3 Multi-line header labels
  # ---------------------------------------------------------------------------

  word_header_labels <- function(nm, dat) {
    labs <- setNames(
      names(dat),
      names(dat)
    )

    maps <- list(
      S3 = c(
        "Principal Component" =
          "Principal\ncomponent",
        "Variance Explained" =
          "Variance\nexplained",
        "Cumulative Variance" =
          "Cumulative\nvariance",
        "Variance Explained Percent" =
          "Variance explained\n(%)",
        "Cumulative Variance Percent" =
          "Cumulative variance\n(%)"
      ),
      S4 = c(
        "Crude OR (95% CI)" =
          "Crude OR\n(95% CI)",
        "Crude P" =
          "Crude\nP value",
        "Adjusted OR (95% CI)" =
          "Adjusted OR\n(95% CI)",
        "Adjusted P" =
          "Adjusted\nP value"
      ),
      S5 = c(
        "AUROC (95% CI)" =
          "AUROC\n(95% CI)"
      ),
      S6 = c(
        "Selected N Folds" =
          "Selected\nfolds",
        "Selection Percent" =
          "Selection\n(%)"
      ),
      S7 = c(
        "AUROC (95% CI)" =
          "AUROC\n(95% CI)",
        "Delta AUROC vs FDP" =
          "ΔAUROC\nvs FDP",
        "DeLong P" =
          "DeLong\nP value",
        "Brier Score" =
          "Brier\nscore"
      ),
      S8 = c(
        "Estimate (95% CI)" =
          "Estimate\n(95% CI)",
        "P value" =
          "P\nvalue"
      ),
      S9 = c(
        "Calibration intercept (95% CI)" =
          "Calibration intercept\n(95% CI)",
        "Calibration slope (95% CI)" =
          "Calibration slope\n(95% CI)",
        "Recalibration intercept" =
          "Recalibration\nintercept"
      ),
      S10 = c(
        "Progressors (n=37)" =
          "Progressors\n(n=37)",
        "Stable severe (n=63)" =
          "Stable severe\n(n=63)",
        "P value" =
          "P\nvalue"
      ),
      S11 = c(
        "Baseline Median IQR" =
          "Baseline\nmedian (IQR)",
        "Followup Median IQR" =
          "Follow-up\nmedian (IQR)",
        "Change Median IQR" =
          "Change\nmedian (IQR)",
        "Wilcoxon Signed Rank P" =
          "Wilcoxon signed-rank\nP value",
        "Paired t Sensitivity P" =
          "Paired t-test\nP value"
      ),
      S12 = c(
        "Progressors Median IQR" =
          "Progressors\nmedian (IQR)",
        "Stable Severe Median IQR" =
          "Stable severe\nmedian (IQR)",
        "Wilcoxon Rank Sum P" =
          "Wilcoxon rank-sum\nP value"
      ),
      S13 = c(
        "Unweighted N" =
          "Unweighted\nN",
        "Unweighted summary" =
          "Unweighted\nsummary",
        "Survey-weighted estimate (95% CI)" =
          "Survey-weighted estimate\n(95% CI)"
      ),
      S14 = c(
        "External N" =
          "External\nN",
        "Alcohol observed, n" =
          "Alcohol\nobserved, n",
        "Alcohol complete, %" =
          "Alcohol\ncomplete, %",
        "Current drinker, n" =
          "Current\ndrinker, n",
        "No past-12-month drinking, n" =
          "No past-12-month\ndrinking, n",
        "Alcohol missing, n" =
          "Alcohol\nmissing, n",
        "Advanced CKM events, n" =
          "Advanced CKM\nevents, n",
        "Advanced events with alcohol data, n" =
          "Advanced events\nwith alcohol data, n"
      ),
      S15 = c(
        "China PC1" =
          "China\nPC1",
        "China PC2" =
          "China\nPC2",
        "NHANES PC1" =
          "NHANES\nPC1",
        "NHANES PC2" =
          "NHANES\nPC2"
      ),
      S16 = c(
        "Pearson r" =
          "Pearson\nr",
        "Spearman ρ" =
          "Spearman\nρ",
        "Tucker φ" =
          "Tucker\nφ",
        "RMS loading difference" =
          "RMS loading\ndifference",
        "NHANES matched component" =
          "NHANES matched\ncomponent"
      ),
      S17 = c(
        "Bootstrap mean" =
          "Bootstrap\nmean",
        "φ ≥0.90, %" =
          "φ ≥0.90\n(%)",
        "φ ≥0.95, %" =
          "φ ≥0.95\n(%)"
      ),
      S18 = c(
        "variance explained" =
          "Variance\nexplained",
        "cumulative variance" =
          "Cumulative\nvariance"
      ),
      S19 = c(
        "OR (95% CI)" =
          "OR\n(95% CI)",
        "P value" =
          "P\nvalue"
      ),
      S20 = c(
        "OR (95% CI)" =
          "OR\n(95% CI)",
        "P value" =
          "P\nvalue"
      ),
      S21 = c(
        "OR (95% CI)" =
          "OR\n(95% CI)",
        "P value" =
          "P\nvalue"
      ),
      S22 = c(
        "Advanced CKM events" =
          "Advanced CKM\nevents",
        "OR (95% CI)" =
          "OR\n(95% CI)",
        "P value" =
          "P\nvalue",
        "Included cycles" =
          "Included\ncycles"
      ),
      S23 = c(
        "Weighted AUROC" =
          "Weighted\nAUROC",
        "Weighted Brier score" =
          "Weighted Brier\nscore"
      ),
      S24 = c(
        "Score definition" =
          "Score\ndefinition",
        "Primary OR (95% CI)" =
          "Primary OR\n(95% CI)",
        "Age-adjusted OR (95% CI)" =
          "Age-adjusted OR\n(95% CI)",
        "Age-adjusted P" =
          "Age-adjusted\nP value",
        "Attenuation, %" =
          "Attenuation\n(%)"
      ),
      S25 = c(
        "Score definition" =
          "Score\ndefinition",
        "Interaction OR per 10 years (95% CI)" =
          "Interaction OR per 10 years\n(95% CI)",
        "P value" =
          "P\nvalue"
      ),
      S26 = c(
        "Score definition" =
          "Score\ndefinition",
        "Input N" =
          "Input\nN",
        "Same-sample model N" =
          "Same-sample\nmodel N",
        "Age, mean ± SD (range), years" =
          "Age, mean ± SD\n(range), years"
      )
    )

    mp <- maps[[nm]]

    if (!is.null(mp)) {
      hit <- intersect(
        names(mp),
        names(labs)
      )
      labs[hit] <- unname(
        mp[hit]
      )
    }

    labs
  }

  # ---------------------------------------------------------------------------
  # 23.4 Page-safe widths and font sizes
  # ---------------------------------------------------------------------------

  word_col_widths <- function(
      nm,
      dat,
      total_width = 10.20
  ) {
    nc <- ncol(dat)

    overrides <- list(
      S1  = c(1.85, 8.35),
      S3  = c(1.45, 1.80, 1.90, 2.30, 2.55),
      S4  = c(1.70, 2.05, 1.10, 2.05, 1.10),
      S5  = c(1.10, 2.35, 1.20, 1.20, 1.20, 1.20),
      S6  = c(2.20, 1.65, 1.65),
      S7  = c(1.35, 2.25, 1.55, 1.10, 1.30),
      S8  = c(2.00, 1.85, 3.95, 1.05),
      S9  = c(1.00, 2.30, 2.30, 1.55, 0.90, 0.70, 0.70, 0.70),
      S10 = c(2.30, 1.70, 2.00, 2.15, 0.95),
      S11 = c(0.85, 2.00, 2.00, 2.00, 1.60, 1.55),
      S12 = c(1.00, 2.80, 2.80, 1.95),
      S13 = c(2.50, 1.20, 2.45, 3.40),
      S14 = c(0.90, 0.85, 1.05, 1.05, 1.00, 1.25, 0.95, 1.10, 1.20),
      S15 = c(1.65, 1.40, 1.40, 1.40, 1.40),
      S16 = c(1.10, 1.10, 1.10, 1.10, 1.50, 2.05),
      S17 = c(1.15, 1.25, 1.45, 1.55, 1.35, 1.35),
      S18 = c(1.40, 1.50, 1.90, 2.00),
      S19 = c(5.00, 0.70, 1.95, 0.95),
      S20 = c(6.15, 2.00, 0.95),
      S21 = c(5.45, 2.00, 0.95),
      S22 = c(1.50, 0.70, 1.35, 1.40, 0.85, 3.45),
      S23 = c(2.00, 1.95, 2.05),
      S24 = c(1.30, 1.65, 0.50, 1.70, 1.80, 1.05, 1.15),
      S25 = c(1.65, 2.00, 3.50, 0.95),
      S26 = c(1.35, 1.60, 0.70, 1.25, 0.65, 0.65, 3.10)
    )

    if (
      !is.null(overrides[[nm]]) &&
      length(overrides[[nm]]) == nc
    ) {
      w <- overrides[[nm]]
      return(
        total_width *
          w /
          sum(w)
      )
    }

    # Fallback based on displayed cell lengths.
    score <- vapply(
      seq_len(nc),
      function(j) {
        vals <- as.character(
          dat[[j]]
        )
        vals[
          is.na(vals)
        ] <- ""

        lens <- nchar(
          vals,
          type = "width"
        )

        h <- nchar(
          names(dat)[j],
          type = "width"
        )

        p90 <- if (length(lens)) {
          as.numeric(
            stats::quantile(
              lens,
              .90,
              na.rm = TRUE,
              names = FALSE
            )
          )
        } else {
          1
        }

        sqrt(
          max(
            4,
            min(
              55,
              max(
                h,
                p90
              )
            )
          )
        )
      },
      numeric(1)
    )

    score[1] <- max(
      score[1],
      2.0
    )

    widths <- total_width *
      score /
      sum(score)

    min_w <- if (nc >= 9) {
      0.58
    } else if (nc >= 7) {
      0.68
    } else {
      0.82
    }

    widths <- pmax(
      widths,
      min_w
    )

    total_width *
      widths /
      sum(widths)
  }

  word_font_size <- function(nm, dat) {
    nc <- ncol(dat)

    if (nm %in% c("S9", "S14")) {
      return(6.5)
    }

    if (
      nm %in%
      c(
        "S5",
        "S11",
        "S15",
        "S16",
        "S17",
        "S22",
        "S24",
        "S26"
      )
    ) {
      return(6.9)
    }

    if (
      nm %in%
      c(
        "S3",
        "S4",
        "S7",
        "S10",
        "S13",
        "S19",
        "S20",
        "S21",
        "S25"
      )
    ) {
      return(7.3)
    }

    if (nc >= 9) return(6.5)
    if (nc >= 7) return(6.9)
    if (nc >= 5) return(7.3)

    8.0
  }

  add_title_par <- function(doc, text) {
    officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          text,
          officer::fp_text(
            font.family = "Arial",
            font.size = 10.0,
            bold = TRUE
          )
        ),
        fp_p = officer::fp_par(
          text.align = "left",
          padding.bottom = 3,
          keep_with_next = TRUE
        )
      )
    )
  }

  add_note_par <- function(doc, text) {
    officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          text,
          officer::fp_text(
            font.family = "Arial",
            font.size = 7.8,
            italic = TRUE
          )
        ),
        fp_p = officer::fp_par(
          text.align = "left",
          padding.top = 3
        )
      )
    )
  }

  # ---------------------------------------------------------------------------
  # 23.5 Create the Word document in forced landscape
  # ---------------------------------------------------------------------------

  # Official officer/flextable landscape pattern:
  # width/height are supplied in portrait A4 order and orient="landscape"
  # swaps them to landscape.
  landscape_section <- officer::prop_section(
    page_size = officer::page_size(
      orient = "landscape",
      width = 8.27,
      height = 11.69
    ),
    page_margins = officer::page_mar(
      top = 0.40,
      bottom = 0.40,
      left = 0.40,
      right = 0.40,
      header = 0.18,
      footer = 0.18
    ),
    type = "continuous"
  )

  doc <- officer::read_docx()

  # Set before content and again at the end.
  doc <- officer::body_set_default_section(
    doc,
    value = landscape_section
  )

  dims_before <- officer::docx_dim(
    doc
  )

  if (
    !isTRUE(
      dims_before$landscape
    )
  ) {
    stop(
      paste0(
        "Could not switch the Word document to landscape before writing tables. ",
        "officer::docx_dim() reports portrait."
      ),
      call. = FALSE
    )
  }

  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        "Supplementary Tables",
        officer::fp_text(
          font.family = "Arial",
          font.size = 12.5,
          bold = TRUE
        )
      ),
      fp_p = officer::fp_par(
        text.align = "center",
        padding.bottom = 6
      )
    )
  )

  table_names <- names(
    tables
  )

  word_layout_audit <- vector(
    "list",
    length(
      table_names
    )
  )

  for (ii in seq_along(table_names)) {
    nm <- table_names[ii]

    title_txt <- paste0(
      "Supplementary Table ",
      nm,
      ". ",
      unname(
        table_titles[nm]
      )
    )

    doc <- add_title_par(
      doc,
      title_txt
    )

    word_dat <- prepare_word_table(
      nm,
      tables[[nm]]
    )

    ft <- flextable::flextable(
      word_dat
    )

    hdr <- word_header_labels(
      nm,
      word_dat
    )

    # Newline characters are supported by flextable cells and render as
    # soft line breaks in Word.
    ft <- flextable::set_header_labels(
      ft,
      values = as.list(
        hdr
      )
    )

    ft <- flextable::theme_booktabs(
      ft
    )

    ft <- flextable::font(
      ft,
      fontname = "Arial",
      part = "all"
    )

    fs <- word_font_size(
      nm,
      word_dat
    )

    ft <- flextable::fontsize(
      ft,
      size = fs,
      part = "body"
    )

    ft <- flextable::fontsize(
      ft,
      size = fs + 0.2,
      part = "header"
    )

    ft <- flextable::bold(
      ft,
      part = "header"
    )

    ft <- flextable::valign(
      ft,
      valign = "center",
      part = "header"
    )

    ft <- flextable::valign(
      ft,
      valign = "top",
      part = "body"
    )

    ft <- flextable::align(
      ft,
      j = 1,
      align = "left",
      part = "all"
    )

    if (
      ncol(
        word_dat
      ) >= 2
    ) {
      ft <- flextable::align(
        ft,
        j = 2:ncol(
          word_dat
        ),
        align = "center",
        part = "all"
      )
    }

    ft <- flextable::padding(
      ft,
      padding.top = 1.6,
      padding.bottom = 1.6,
      padding.left = 1.8,
      padding.right = 1.8,
      part = "all"
    )

    # Critical: fixed layout + explicit widths, no autofit().
    # Compatibility fix for older flextable versions:
    # opts_word() in some releases accepts only `split` and `keep_with_next`.
    # `repeat_headers` is therefore NOT passed here.
    ft <- flextable::set_table_properties(
      ft,
      layout = "fixed",
      align = "center",
      opts_word = list(
        split = TRUE,
        keep_with_next = FALSE
      )
    )

    col_widths <- word_col_widths(
      nm,
      word_dat,
      total_width = 10.20
    )

    for (jj in seq_along(col_widths)) {
      ft <- flextable::width(
        ft,
        j = jj,
        width = col_widths[jj]
      )
    }

    # Repeat headers on multi-page tables when the installed flextable version
    # supports paginate().  The formal arguments changed across versions, so
    # construct the call dynamically instead of assuming `init`/`hdr_ftr`.
    if (
      "paginate" %in% getNamespaceExports("flextable")
    ) {
      pg_fun <- getExportedValue("flextable", "paginate")
      pg_formals <- names(formals(pg_fun))

      pg_args <- list(x = ft)

      if ("init" %in% pg_formals) {
        pg_args$init <- TRUE
      }

      if ("hdr_ftr" %in% pg_formals) {
        pg_args$hdr_ftr <- TRUE
      }

      if ("group" %in% pg_formals && !"init" %in% pg_formals) {
        # Leave model-specific grouping at the package default.
        pg_args$group <- NULL
      }

      ft <- tryCatch(
        do.call(pg_fun, pg_args),
        error = function(e) {
          warning(
            paste0(
              "flextable::paginate() is available but could not be applied ",
              "with this installed version; continuing without explicit ",
              "header pagination. Details: ",
              conditionMessage(e)
            ),
            call. = FALSE
          )
          ft
        }
      )
    }

    doc <- flextable::body_add_flextable(
      doc,
      ft
    )

    note_txt <- unname(
      table_notes[nm]
    )

    if (
      !is.na(
        note_txt
      ) &&
      nzchar(
        note_txt
      )
    ) {
      doc <- add_note_par(
        doc,
        paste0(
          "Note: ",
          note_txt
        )
      )
    }

    word_layout_audit[[ii]] <- data.frame(
      Table = nm,
      N_columns = ncol(
        word_dat
      ),
      Font_size_pt = fs,
      Planned_width_in = sum(
        col_widths
      ),
      stringsAsFactors = FALSE
    )

    if (
      ii <
      length(
        table_names
      )
    ) {
      doc <- officer::body_add_break(
        doc
      )
    }
  }

  # Re-assert the default section after all page breaks/tables.
  doc <- officer::body_set_default_section(
    doc,
    value = landscape_section
  )

  dims_final_object <- officer::docx_dim(
    doc
  )

  if (
    !isTRUE(
      dims_final_object$landscape
    )
  ) {
    stop(
      "Final rdocx object is not landscape; Word file was not written.",
      call. = FALSE
    )
  }

  docx_path <- file.path(
    OUT_DIR,
    "CKM_Supplementary_Tables_S1-S26_v7_LANDSCAPE_WRAPPED.docx"
  )

  print(
    doc,
    target = docx_path
  )

  # ---------------------------------------------------------------------------
  # 23.6 Read the actual written DOCX back and verify page geometry
  # ---------------------------------------------------------------------------

  written_doc <- officer::read_docx(
    path = docx_path
  )

  written_dim <- officer::docx_dim(
    written_doc
  )

  if (
    !isTRUE(
      written_dim$landscape
    ) ||
    written_dim$page[["width"]] <=
      written_dim$page[["height"]]
  ) {
    stop(
      paste0(
        "DOCX was written, but verification shows it is not landscape. ",
        "Do not use this output. Page width=",
        round(
          written_dim$page[["width"]],
          2
        ),
        ", height=",
        round(
          written_dim$page[["height"]],
          2
        ),
        "."
      ),
      call. = FALSE
    )
  }

  layout_audit <- dplyr::bind_rows(
    word_layout_audit
  )

  layout_audit$DOCX_landscape <- isTRUE(
    written_dim$landscape
  )
  layout_audit$Page_width_in <- written_dim$page[["width"]]
  layout_audit$Page_height_in <- written_dim$page[["height"]]
  layout_audit$Left_margin_in <- written_dim$margins[["left"]]
  layout_audit$Right_margin_in <- written_dim$margins[["right"]]

  utils::write.csv(
    layout_audit,
    file.path(
      OUT_DIR,
      "Word_layout_audit_v7.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  message("")
  message("Word layout verification:")
  message(
    "  Landscape: ",
    written_dim$landscape
  )
  message(
    "  Page: ",
    sprintf(
      "%.2f x %.2f inches",
      written_dim$page[["width"]],
      written_dim$page[["height"]]
    )
  )
  message(
    "  Margins L/R: ",
    sprintf(
      "%.2f / %.2f inches",
      written_dim$margins[["left"]],
      written_dim$margins[["right"]]
    )
  )
  message(
    "  Verified file: ",
    docx_path
  )

  docx_written <- TRUE
}

# ==============================================================================
# 24. Audit manifest
# ==============================================================================

manifest <- data.frame(
  Table = names(
    tables
  ),
  Title = unname(
    table_titles[
      names(
        tables
      )
    ]
  ),
  Rows = vapply(
    tables,
    nrow,
    integer(1)
  ),
  Columns = vapply(
    tables,
    ncol,
    integer(1)
  ),
  CSV = file.path(
    CSV_DIR,
    paste0(
      "Supplementary_Table_",
      names(
        tables
      ),
      ".csv"
    )
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  manifest,
  file.path(
    OUT_DIR,
    "Supplementary_Table_manifest.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ==============================================================================
# 25. Final checks and console output
# ==============================================================================

if (
  length(
    tables
  ) != 26L
) {
  stop(
    "Expected 26 supplementary tables but generated ",
    length(
      tables
    ),
    ".",
    call. = FALSE
  )
}

message("")
message("============================================================")
message("SUPPLEMENTARY TABLE GENERATION COMPLETE")
message("============================================================")
message("Output directory:")
message(OUT_DIR)
message("")
message("Excel workbook:")
message(xlsx_path)
message("")
message("Individual CSV files:")
message(CSV_DIR)
message("")
if (docx_written) {
  message("Word document:")
  message(docx_path)
} else {
  message(
    "Word document was not created because officer/flextable ",
    "are not installed. Excel and CSV outputs were created normally."
  )
}
message("")
message("Generated tables: S1-S26")
message("============================================================")
