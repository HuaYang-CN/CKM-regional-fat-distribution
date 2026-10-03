# ==============================================================================
# 00_CKM_refactored_config.R
# Refactored configuration for the regional fat-distribution / CKM project
# ==============================================================================
# Edit ONLY the four input-path settings below if your folders change.
# Analysis and plotting scripts source this file and never search legacy folders.
# This deliberate design prevents an older file from being selected silently.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999)
options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)

# ---- User paths ---------------------------------------------------------------
# Run from the repository root. Override only these paths when authorized.
REPO_ROOT <- normalizePath(Sys.getenv("CKM_REPO_ROOT", unset = getwd()), winslash = "/", mustWork = TRUE)
OUTPUT_ROOT <- Sys.getenv("CKM_OUTPUT_ROOT", unset = file.path(REPO_ROOT, "results"))
CHINA_CROSS_FILE <- Sys.getenv("CKM_CHINA_CROSS_FILE", unset = file.path(REPO_ROOT, "restricted_data", "china", "cross_filtered.xlsx"))
CHINA_LONG_FILE <- Sys.getenv("CKM_CHINA_LONG_FILE", unset = file.path(REPO_ROOT, "restricted_data", "china", "suifang.xlsx"))
NHANES_EXTERNAL_READY_FILE <- Sys.getenv("CKM_NHANES_READY_FILE", unset = file.path(REPO_ROOT, "data", "public", "nhanes", "data_processed", "NHANES_2011_2018_external_validation_ready_REVISED.rds"))
NHANES_RAW_DIR <- Sys.getenv("CKM_NHANES_RAW_DIR", unset = file.path(REPO_ROOT, "data", "public", "nhanes", "data_raw"))

# IMPORTANT: the refactored analysis begins from the locked staged NHANES RDS.
# For end-to-end reproducibility, archive the upstream script that creates
# NHANES_2011_2018_external_validation_ready_REVISED.rds together with this
# pipeline, and retain the input MD5 manifest written by Stage 01.

# ---- Refactored output tree ---------------------------------------------------
# Save every analysis/table/figure/audit output inside CKM第五次分析.
REFAC_ROOT <- OUTPUT_ROOT
DIR_ANALYSIS <- file.path(REFAC_ROOT, "01_locked_analysis_objects")
DIR_TIDY     <- file.path(REFAC_ROOT, "02_tidy_results")
DIR_FIG      <- file.path(REFAC_ROOT, "03_publication_figures")
DIR_TABLE    <- file.path(REFAC_ROOT, "04_publication_tables")
DIR_AUDIT    <- file.path(REFAC_ROOT, "05_audit")

invisible(lapply(
  c(REFAC_ROOT, DIR_ANALYSIS, DIR_TIDY, DIR_FIG, DIR_TABLE, DIR_AUDIT),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

# ---- Locked variables ---------------------------------------------------------
CHINA_FAT_VARS <- c("LL%", "LA%", "RA%", "RL%", "Trunk%", "BMI", "VFA")
CHINA_BASELINE_VARS <- c("Sex", "Smoke", "Alcohol_Use")
CHINA_AGE_VAR <- "Age"

# Canonical cross-platform order used for loading comparison/transfer.
CANONICAL_VARS <- c("BMI", "VAT", "Trunk", "Left arm", "Right arm", "Left leg", "Right leg")
CHINA_TO_CANONICAL <- c(
  "BMI" = "BMI", "VFA" = "VAT", "Trunk%" = "Trunk",
  "LA%" = "Left arm", "RA%" = "Right arm",
  "LL%" = "Left leg", "RL%" = "Right leg"
)
NHANES_TO_CANONICAL <- c(
  "BMI" = "BMI", "VAT_area" = "VAT", "Trunk_pct" = "Trunk",
  "LA_pct" = "Left arm", "RA_pct" = "Right arm",
  "LL_pct" = "Left leg", "RL_pct" = "Right leg"
)
NHANES_PCA_VARS <- names(NHANES_TO_CANONICAL)

# Explicit factor coding; edit only if your raw China file uses a different code.
SEX_FEMALE_VALUES <- c("female", "f", "0", "女")
SEX_MALE_VALUES   <- c("male", "m", "1", "男")
NO_VALUES  <- c("no", "n", "0", "false", "否")
YES_VALUES <- c("yes", "y", "1", "true", "是")

# ---- Locked sample expectations ----------------------------------------------
EXPECTED_DERIVATION_N <- 955L
EXPECTED_LONGITUDINAL_N <- 100L
EXPECTED_PROGRESSOR_N <- 37L
EXPECTED_STABLE_SEVERE_N <- 63L
EXPECTED_NHANES_N <- 2254L
EXPECTED_NHANES_ADVANCED <- 157L
STRICT_COUNTS <- TRUE

# ---- Reproducibility -----------------------------------------------------------
SEED_MAIN <- 20260825L
SEED_BOOT <- 20260826L
SEED_EXTERNAL <- 20260827L
SEED_SHAP <- 20260828L
N_PCA_BOOT <- 1000L
N_METRIC_BOOT <- 2000L

# ---- Optional secondary modules -----------------------------------------------
RUN_INTERNAL_ML <- TRUE
RUN_NRI_IDI <- TRUE
RUN_SHAP <- TRUE
RUN_NHANES_ALCOHOL <- TRUE
RUN_TWO_SAMPLE_PCA_BOOTSTRAP <- TRUE

# Candidate algorithms for secondary internal ML comparison.
ML_ALGORITHMS <- c("LR", "RF", "GBM", "SVM", "ElasticNet", "KNN")
ML_TUNE_LENGTH <- 5L
OUTER_FOLDS <- 10L
INNER_FOLDS <- 5L

# ---- Plot style ----------------------------------------------------------------
PLOT_FONT <- "Arial"
COL_BLUE  <- "#2F5597"
COL_RED   <- "#B33A3A"
COL_TEAL  <- "#2A7F9E"
COL_GOLD  <- "#C58A2A"
COL_GRAY  <- "#6B7280"
COL_LIGHT_GRAY <- "#D1D5DB"
COL_BLACK <- "#1F2937"

# ==============================================================================
# Common helpers
# ==============================================================================
assert_file <- function(path, label = "Input file") {
  if (!file.exists(path)) stop(label, " not found:\n", path, call. = FALSE)
  invisible(path)
}

assert_columns <- function(dat, vars, label = "data") {
  miss <- setdiff(vars, names(dat))
  if (length(miss)) stop(label, " missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

assert_count <- function(observed, expected, label) {
  if (identical(as.integer(observed), as.integer(expected))) return(invisible(TRUE))
  msg <- sprintf("%s count differs from lock: expected %d, observed %d", label, expected, observed)
  if (isTRUE(STRICT_COUNTS)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
}

normalize_text <- function(x) tolower(trimws(as.character(x)))

normalize_ckm <- function(x) {
  z <- normalize_text(x)
  out <- rep(NA_character_, length(z))
  out[z %in% c("0", "0.0", "mild")] <- "Mild"
  out[z %in% c("1", "1.0", "severe")] <- "Severe"
  factor(out, levels = c("Mild", "Severe"))
}

normalize_binary_factor <- function(x, no_values = NO_VALUES, yes_values = YES_VALUES,
                                    labels = c("No", "Yes"), variable = "binary variable") {
  z <- normalize_text(x)
  out <- rep(NA_character_, length(z))
  out[z %in% no_values] <- labels[1]
  out[z %in% yes_values] <- labels[2]
  bad <- unique(z[is.na(out) & !is.na(z) & nzchar(z)])
  if (length(bad)) stop(variable, " has unrecognized values: ", paste(bad, collapse = ", "), call. = FALSE)
  factor(out, levels = labels)
}

normalize_sex <- function(x) {
  z <- normalize_text(x)
  out <- rep(NA_character_, length(z))
  out[z %in% SEX_FEMALE_VALUES] <- "Female"
  out[z %in% SEX_MALE_VALUES] <- "Male"
  bad <- unique(z[is.na(out) & !is.na(z) & nzchar(z)])
  if (length(bad)) stop("Sex has unrecognized values: ", paste(bad, collapse = ", "), call. = FALSE)
  factor(out, levels = c("Female", "Male"))
}

format_p <- function(p) {
  ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

weighted_mean_sd <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  mu <- sum(w * x) / sum(w)
  s <- sqrt(sum(w * (x - mu)^2) / sum(w))
  c(mean = mu, sd = s)
}

weighted_auc <- function(y, pred, w) {
  ok <- is.finite(y) & is.finite(pred) & is.finite(w) & w > 0 & y %in% c(0, 1)
  y <- y[ok]; pred <- pred[ok]; w <- w[ok]
  if (!length(y) || sum(w[y == 1]) <= 0 || sum(w[y == 0]) <= 0) return(NA_real_)
  d <- data.frame(y = y, pred = pred, w = w)
  sp <- split(d, d$pred)
  tab <- do.call(rbind, lapply(sp, function(z) c(
    pred = z$pred[1], case_w = sum(z$w[z$y == 1]), control_w = sum(z$w[z$y == 0])
  )))
  tab <- as.data.frame(tab)
  tab <- tab[order(tab$pred), , drop = FALSE]
  controls_below <- c(0, head(cumsum(tab$control_w), -1))
  num <- sum(tab$case_w * (controls_below + 0.5 * tab$control_w))
  den <- sum(tab$case_w) * sum(tab$control_w)
  num / den
}

weighted_brier <- function(y, pred, w) {
  ok <- is.finite(y) & is.finite(pred) & is.finite(w) & w > 0
  sum(w[ok] * (y[ok] - pred[ok])^2) / sum(w[ok])
}

tucker_phi <- function(x, y) {
  x <- as.numeric(x); y <- as.numeric(y)
  sum(x * y) / sqrt(sum(x^2) * sum(y^2))
}

# One orientation rule is used in China, NHANES, and all bootstrap replicates.
# PC1: mean loading positive. PC2: maximize lower-limb relative to visceral loading.
orient_pca <- function(fit, vat_name, leg_names) {
  if (mean(fit$rotation[, "PC1"], na.rm = TRUE) < 0) {
    fit$rotation[, "PC1"] <- -fit$rotation[, "PC1"]
    fit$x[, "PC1"] <- -fit$x[, "PC1"]
  }
  direction <- mean(fit$rotation[leg_names, "PC2"], na.rm = TRUE) - fit$rotation[vat_name, "PC2"]
  if (is.finite(direction) && direction < 0) {
    fit$rotation[, "PC2"] <- -fit$rotation[, "PC2"]
    fit$x[, "PC2"] <- -fit$x[, "PC2"]
  }
  fit
}

map_loading_to_canonical <- function(rotation, mapping, component) {
  x <- rotation[names(mapping), component]
  names(x) <- unname(mapping)
  x[CANONICAL_VARS]
}

extract_glm_or <- function(fit, term) {
  b <- stats::coef(fit)[term]
  se <- sqrt(stats::vcov(fit)[term, term])
  z <- stats::qnorm(0.975)
  data.frame(
    term = term, OR = exp(b), Lower = exp(b - z * se), Upper = exp(b + z * se),
    P = summary(fit)$coefficients[term, 4], row.names = NULL
  )
}

extract_svy_or <- function(fit, design, term) {
  cm <- stats::coef(summary(fit))
  if (!term %in% rownames(cm)) return(data.frame(term = term, OR = NA, Lower = NA, Upper = NA, P = NA))
  df <- survey::degf(design)
  crit <- if (is.finite(df) && df > 0) stats::qt(0.975, df) else stats::qnorm(0.975)
  b <- cm[term, 1]; se <- cm[term, 2]
  data.frame(term = term, OR = exp(b), Lower = exp(b - crit * se), Upper = exp(b + crit * se), P = cm[term, 4])
}

safe_md5 <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) return(data.frame())
  data.frame(File = basename(paths), Path = normalizePath(paths, winslash = "/", mustWork = FALSE),
             MD5 = unname(tools::md5sum(paths)), stringsAsFactors = FALSE)
}
