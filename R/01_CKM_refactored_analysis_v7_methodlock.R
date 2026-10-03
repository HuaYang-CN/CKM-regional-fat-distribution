# ==============================================================================
# v3 survey fix: replaced survey::subset(...) with the base subset() generic
# so R can dispatch to survey's subset.survey.design S3 method.
# 01_CKM_refactored_analysis.R
# Statistical analysis only. NO publication figure/table construction.
# v4 ML fix: use dplyr::bind_rows() for heterogeneous caret bestTune columns;
# preserves model-specific tuning parameters and fills non-applicable fields with NA.
# v5 calibration fix: robust, un-named calibration coefficients and one-pass
# comparator metric extraction; fixes 'row names contain missing values'.
# v7 method lock: DCA thresholds harmonized to 5%-80%; metric bootstrap set by config;
# NRI/IDI bootstrap stratified by outcome; external/age ORs stored from one CI extractor.
# v6 full-supplement support:
#   * saves figure-ready secondary summaries for Supplementary Figures S1-S12;
#   * adds NHANES-derived PC2-by-age interaction needed for age-sensitivity panel;
#   * expands alcohol sensitivity to the full five-row comparison;
#   * saves PCA bootstrap loading distributions and external weighted ROC data.
# ==============================================================================
# Key corrections relative to the previous multi-script workflow:
#   * one explicit path/config source; no recursive legacy-file discovery;
#   * one locked China PCA object reused for longitudinal projection and transfer;
#   * China loadings are read from the locked PCA object, never hard-coded;
#   * one PCA orientation rule across China/NHANES/bootstrap;
#   * factor reference levels are explicit;
#   * Figure-2 named-vector extraction bug is eliminated by model-object storage;
#   * SHAP reads the current locked cohort rather than an older Stage-03 file;
#   * distance/kernel ML models receive normalized numeric predictors after PCA;
#   * all primary and sensitivity objects are saved in one master RDS.
# ==============================================================================

# ---- Robustly locate the configuration file ----------------------------------
# Works when the script is run with source(), the RStudio Source button, or Rscript.
get_current_script_dir <- function() {
  # source()/RStudio: search call frames for an `ofile` path.
  frames <- sys.frames()
  ofiles <- vapply(frames, function(fr) {
    if (!is.null(fr$ofile) && length(fr$ofile) >= 1L) as.character(fr$ofile[[1]]) else NA_character_
  }, character(1))
  ofiles <- ofiles[!is.na(ofiles) & nzchar(ofiles)]
  if (length(ofiles)) {
    return(dirname(normalizePath(tail(ofiles, 1L), winslash = "/", mustWork = FALSE)))
  }

  # Rscript --file=/path/to/script.R
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    script_path <- sub("^--file=", "", file_arg[[1]])
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)))
  }

  # Interactive fallback.
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

SCRIPT_DIR <- get_current_script_dir()
CONFIG_NAME <- "00_CKM_refactored_config.R"
CONFIG_CANDIDATES <- unique(c(
  file.path(SCRIPT_DIR, CONFIG_NAME),
  file.path(getwd(), CONFIG_NAME),
  file.path(Sys.getenv("CKM_REPO_ROOT", unset = getwd()), "R", CONFIG_NAME)
))
existing_config <- CONFIG_CANDIDATES[file.exists(CONFIG_CANDIDATES)]
if (!length(existing_config)) {
  stop(
    paste0(
      "Could not find ", CONFIG_NAME, ".\nChecked:\n  ",
      paste(CONFIG_CANDIDATES, collapse = "\n  "),
      "\n\nPlace 00_CKM_refactored_config.R in the same folder as this script, ",
      "or edit CONFIG_CANDIDATES at the top of the script."
    ),
    call. = FALSE
  )
}
CONFIG_FILE <- normalizePath(existing_config[[1]], winslash = "/", mustWork = TRUE)
message("Using config: ", CONFIG_FILE)
source(CONFIG_FILE, encoding = "UTF-8")

required_packages <- c(
  "readxl", "dplyr", "tidyr", "tibble", "rms", "survey", "haven",
  "caret", "recipes", "tidyselect", "pROC", "PRROC", "broom"
)
if (isTRUE(RUN_INTERNAL_ML)) {
  required_packages <- unique(c(required_packages, "randomForest", "gbm", "kernlab", "glmnet", "kknn"))
}
if (isTRUE(RUN_SHAP)) required_packages <- unique(c(required_packages, "fastshap"))
missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Install required packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))", call. = FALSE)
}

set.seed(SEED_MAIN)
assert_file(CHINA_CROSS_FILE, "China cross-sectional file")
assert_file(CHINA_LONG_FILE, "China repeated-measures file")
assert_file(NHANES_EXTERNAL_READY_FILE, "Locked NHANES external-ready RDS")

input_manifest <- safe_md5(c(CHINA_CROSS_FILE, CHINA_LONG_FILE, NHANES_EXTERNAL_READY_FILE))
utils::write.csv(input_manifest, file.path(DIR_AUDIT, "input_file_manifest_md5.csv"), row.names = FALSE)

# ==============================================================================
# Helpers local to this analysis script
# ==============================================================================

as_numeric_strict <- function(x, name) {
  y <- suppressWarnings(as.numeric(x))
  new_na <- is.na(y) & !is.na(x) & nzchar(trimws(as.character(x)))
  if (any(new_na)) stop(name, " contains non-numeric values after conversion.", call. = FALSE)
  y
}

choose_column <- function(dat, candidates, label) {
  nms <- names(dat)
  hit <- candidates[candidates %in% nms]
  if (length(hit)) return(hit[1])
  key <- function(x) gsub("[^[:alnum:]一-龥]+", "", tolower(trimws(x)))
  nk <- key(nms); ck <- key(candidates)
  for (k in ck) {
    idx <- which(nk == k)
    if (length(idx)) return(nms[idx[1]])
  }
  stop("Could not identify ", label, ". Available columns: ", paste(nms, collapse = ", "), call. = FALSE)
}

normalise_time01 <- function(x) {
  z <- normalize_text(x)
  out <- rep(NA_integer_, length(z))
  out[z %in% c("0", "0.0", "baseline", "base", "t0", "visit 0", "visit0", "基线", "初始")] <- 0L
  out[z %in% c("1", "1.0", "followup", "follow-up", "follow up", "t1", "visit 1", "visit1", "随访", "复查")] <- 1L
  bad <- unique(z[is.na(out) & !is.na(z) & nzchar(z)])
  if (length(bad)) stop("Unrecognized longitudinal time value(s): ", paste(bad, collapse = ", "), call. = FALSE)
  out
}

match_boot_components <- function(reference_rotation, fit_rotation, candidates = 1:4) {
  candidates <- intersect(candidates, seq_len(ncol(fit_rotation)))
  # Global one-to-one assignment for PC1/PC2; avoids greedy matching bias.
  pairs <- expand.grid(j1 = candidates, j2 = candidates)
  pairs <- pairs[pairs$j1 != pairs$j2, , drop = FALSE]
  score <- vapply(seq_len(nrow(pairs)), function(i) {
    abs(tucker_phi(reference_rotation[, "PC1"], fit_rotation[, pairs$j1[i]])) +
      abs(tucker_phi(reference_rotation[, "PC2"], fit_rotation[, pairs$j2[i]]))
  }, numeric(1))
  best <- pairs[which.max(score), , drop = FALSE]
  v1 <- fit_rotation[, best$j1]
  v2 <- fit_rotation[, best$j2]
  if (tucker_phi(reference_rotation[, "PC1"], v1) < 0) v1 <- -v1
  if (tucker_phi(reference_rotation[, "PC2"], v2) < 0) v2 <- -v2
  list(PC1 = v1, PC2 = v2, matched = c(PC1 = best$j1, PC2 = best$j2))
}

continuous_nri_idi <- function(y, p_ref, p_new) {
  ok <- is.finite(y) & is.finite(p_ref) & is.finite(p_new) & y %in% c(0, 1)
  y <- y[ok]; p_ref <- p_ref[ok]; p_new <- p_new[ok]
  d <- p_new - p_ref
  event <- y == 1; nonevent <- y == 0
  nri_event <- mean(d[event] > 0) - mean(d[event] < 0)
  nri_nonevent <- mean(d[nonevent] < 0) - mean(d[nonevent] > 0)
  disc_new <- mean(p_new[event]) - mean(p_new[nonevent])
  disc_ref <- mean(p_ref[event]) - mean(p_ref[nonevent])
  c(NRI_event = nri_event, NRI_nonevent = nri_nonevent,
    Continuous_NRI = nri_event + nri_nonevent, IDI = disc_new - disc_ref)
}

calc_calibration <- function(y, p) {
  # Robust calibration summary for binary outcomes.
  # Important: unname() is used on glm coefficients. Without it,
  # c(intercept = coef(fit)[1]) can create names such as
  # "intercept.(Intercept)", so later x["intercept"] returns a
  # named NA and data.frame() may fail with
  # "row names contain missing values".
  ok <- is.finite(y) & is.finite(p) & y %in% c(0, 1)
  y <- y[ok]
  p <- p[ok]

  if (length(y) < 10L || length(unique(y)) < 2L || length(p) != length(y)) {
    return(c(intercept = NA_real_, slope = NA_real_, brier = NA_real_))
  }

  p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
  lp <- qlogis(p)

  f_i <- tryCatch(
    suppressWarnings(stats::glm(y ~ offset(lp), family = stats::binomial())),
    error = function(e) NULL
  )
  f_s <- tryCatch(
    suppressWarnings(stats::glm(y ~ lp, family = stats::binomial())),
    error = function(e) NULL
  )

  intercept <- if (!is.null(f_i) && length(stats::coef(f_i)) >= 1L) {
    unname(stats::coef(f_i)[1])
  } else {
    NA_real_
  }

  slope <- if (!is.null(f_s) && "lp" %in% names(stats::coef(f_s))) {
    unname(stats::coef(f_s)[["lp"]])
  } else {
    NA_real_
  }

  c(
    intercept = as.numeric(intercept),
    slope = as.numeric(slope),
    brier = mean((y - p)^2)
  )
}

net_benefit <- function(y, p, thresholds = seq(0.05, 0.80, by = 0.01)) {
  n <- length(y)
  do.call(rbind, lapply(thresholds, function(pt) {
    pred_pos <- p >= pt
    tp <- sum(pred_pos & y == 1); fp <- sum(pred_pos & y == 0)
    data.frame(threshold = pt, net_benefit = tp / n - fp / n * pt / (1 - pt))
  }))
}

# ==============================================================================
# 1. Chinese derivation cohort and LOCKED PCA
# ==============================================================================

china_raw <- readxl::read_excel(CHINA_CROSS_FILE, .name_repair = "minimal")
assert_columns(china_raw, c("group", CHINA_FAT_VARS, CHINA_BASELINE_VARS), "China cross-sectional data")
china_raw <- as.data.frame(china_raw, check.names = FALSE)
china_raw$row_id <- seq_len(nrow(china_raw))
china_raw$group_factor <- normalize_ckm(china_raw$group)
for (v in CHINA_FAT_VARS) china_raw[[v]] <- as_numeric_strict(china_raw[[v]], v)

pca_ok <- !is.na(china_raw$group_factor) & stats::complete.cases(china_raw[, CHINA_FAT_VARS, drop = FALSE])
china <- china_raw[pca_ok, , drop = FALSE]
assert_count(nrow(china), EXPECTED_DERIVATION_N, "Chinese derivation cohort")

fat_mat <- china[, CHINA_FAT_VARS, drop = FALSE]
china_corr <- stats::cor(fat_mat, use = "complete.obs")
china_vif <- do.call(rbind, lapply(CHINA_FAT_VARS, function(v) {
  others <- setdiff(CHINA_FAT_VARS, v)
  tmp <- fat_mat[, c(v, others), drop = FALSE]; names(tmp)[1] <- "y"
  r2 <- summary(stats::lm(y ~ ., data = tmp))$r.squared
  data.frame(Variable = v, VIF = if (r2 < 1) 1 / (1 - r2) else Inf)
}))

china_pca <- stats::prcomp(fat_mat, center = TRUE, scale. = TRUE)
china_pca <- orient_pca(china_pca, vat_name = "VFA", leg_names = c("LL%", "RL%"))
if (!(china_pca$rotation["VFA", "PC2"] < 0 && mean(china_pca$rotation[c("LL%", "RL%"), "PC2"]) > 0)) {
  stop("China PC2 orientation check failed: expected VFA < 0 and mean leg loading > 0.", call. = FALSE)
}
china$PC1_Score <- china_pca$x[, "PC1"]
china$PC2_Score <- china_pca$x[, "PC2"]
china_variance <- china_pca$sdev^2 / sum(china_pca$sdev^2)

china_pca_lock <- list(
  fit = china_pca,
  center = china_pca$center,
  scale = china_pca$scale,
  rotation = china_pca$rotation[, 1:2, drop = FALSE],
  variance = china_variance,
  variable_order = CHINA_FAT_VARS,
  source_md5 = unname(tools::md5sum(CHINA_CROSS_FILE))
)

# ==============================================================================
# 2. Chinese cross-sectional association + RCS + age sensitivity
# ==============================================================================

china$severe <- as.integer(china$group_factor == "Severe")
china$Sex_f <- normalize_sex(china$Sex)
china$Smoke_f <- normalize_binary_factor(china$Smoke, variable = "Smoke")
china$Alcohol_f <- normalize_binary_factor(china$Alcohol_Use, variable = "Alcohol_Use")
assoc_vars <- c("severe", "PC1_Score", "PC2_Score", "Sex_f", "Smoke_f", "Alcohol_f")
assoc <- china[stats::complete.cases(china[, assoc_vars, drop = FALSE]), , drop = FALSE]
assert_count(nrow(assoc), EXPECTED_DERIVATION_N, "Primary China association sample")

fit_pc1_crude <- stats::glm(severe ~ PC1_Score, family = stats::binomial(), data = assoc)
fit_pc2_crude <- stats::glm(severe ~ PC2_Score, family = stats::binomial(), data = assoc)
fit_primary <- stats::glm(
  severe ~ PC1_Score + PC2_Score + Sex_f + Smoke_f + Alcohol_f,
  family = stats::binomial(), data = assoc
)
china_or <- rbind(
  transform(extract_glm_or(fit_pc1_crude, "PC1_Score"), Exposure = "PC1", Model = "Crude"),
  transform(extract_glm_or(fit_primary, "PC1_Score"), Exposure = "PC1", Model = "Adjusted"),
  transform(extract_glm_or(fit_pc2_crude, "PC2_Score"), Exposure = "PC2", Model = "Crude"),
  transform(extract_glm_or(fit_primary, "PC2_Score"), Exposure = "PC2", Model = "Adjusted")
)

# Restricted cubic splines: 3 knots at 10th, 50th, 90th percentiles.
dd <- rms::datadist(assoc); old_dd <- options(datadist = "dd")
on.exit(options(old_dd), add = TRUE)
k1 <- as.numeric(stats::quantile(assoc$PC1_Score, c(.10, .50, .90), names = FALSE))
k2 <- as.numeric(stats::quantile(assoc$PC2_Score, c(.10, .50, .90), names = FALSE))
fit_rcs_pc1 <- rms::lrm(severe ~ rms::rcs(PC1_Score, k1) + PC2_Score + Sex_f + Smoke_f + Alcohol_f,
                        data = assoc, x = TRUE, y = TRUE)
fit_rcs_pc2 <- rms::lrm(severe ~ rms::rcs(PC2_Score, k2) + PC1_Score + Sex_f + Smoke_f + Alcohol_f,
                        data = assoc, x = TRUE, y = TRUE)
rcs_curve_pc1 <- as.data.frame(rms::Predict(
  fit_rcs_pc1,
  PC1_Score = seq(stats::quantile(assoc$PC1_Score, .025), stats::quantile(assoc$PC1_Score, .975), length.out = 250),
  ref.zero = TRUE, fun = exp, conf.int = .95
))
rcs_curve_pc2 <- as.data.frame(rms::Predict(
  fit_rcs_pc2,
  PC2_Score = seq(stats::quantile(assoc$PC2_Score, .025), stats::quantile(assoc$PC2_Score, .975), length.out = 250),
  ref.zero = TRUE, fun = exp, conf.int = .95
))
extract_rcs_p <- function(fit, varname) {
  a <- as.data.frame(stats::anova(fit)); a$Term <- trimws(rownames(a))
  pcol <- if ("P" %in% names(a)) "P" else grep("^P$|Pr", names(a), value = TRUE, ignore.case = TRUE)[1]
  idx <- which(a$Term == varname)
  nidx <- which(grepl("Nonlinear", a$Term, ignore.case = TRUE) & seq_len(nrow(a)) > ifelse(length(idx), idx[1], Inf))
  c(overall = if (length(idx)) as.numeric(a[idx[1], pcol]) else NA_real_,
    nonlinear = if (length(nidx)) as.numeric(a[nidx[1], pcol]) else NA_real_)
}
rcs_p <- rbind(PC1 = extract_rcs_p(fit_rcs_pc1, "PC1_Score"),
               PC2 = extract_rcs_p(fit_rcs_pc2, "PC2_Score"))

age_sensitivity_china <- NULL
if (CHINA_AGE_VAR %in% names(assoc)) {
  assoc$Age10 <- (as_numeric_strict(assoc[[CHINA_AGE_VAR]], CHINA_AGE_VAR) -
                    mean(as_numeric_strict(assoc[[CHINA_AGE_VAR]], CHINA_AGE_VAR), na.rm = TRUE)) / 10
  fit_age <- stats::glm(severe ~ PC1_Score + PC2_Score + Sex_f + Smoke_f + Alcohol_f + Age10,
                        family = stats::binomial(), data = assoc)
  fit_age_int <- stats::glm(severe ~ PC1_Score + PC2_Score * Age10 + Sex_f + Smoke_f + Alcohol_f,
                            family = stats::binomial(), data = assoc)
  age_sensitivity_china <- list(
    fit = fit_age, interaction_fit = fit_age_int,
    PC1 = extract_glm_or(fit_age, "PC1_Score"), PC2 = extract_glm_or(fit_age, "PC2_Score")
  )
}

# ==============================================================================
# 3. Repeated-measures analysis using THE SAME locked PCA object
# ==============================================================================

long_raw <- readxl::read_excel(CHINA_LONG_FILE, .name_repair = "minimal")
long_raw <- as.data.frame(long_raw, check.names = FALSE)
id_col <- choose_column(long_raw, c("ID", "id", "subject_id", "Subject_ID", "patient_id", "Patient_ID", "编号", "患者编号", "病例号"), "ID")
time_col <- choose_column(long_raw, c("time", "Time", "timepoint", "Timepoint", "visit", "Visit", "时间点", "随访时间点", "时间"), "time")
group_col <- choose_column(long_raw, c("group", "Group", "CKM_group", "ckm_group", "severity", "Severity", "分组", "严重程度"), "CKM group")
assert_columns(long_raw, CHINA_FAT_VARS, "China repeated-measures data")
long <- long_raw
names(long)[match(c(id_col, time_col, group_col), names(long))] <- c("ID", "time", "group")
long$time <- normalise_time01(long$time)
long$group_factor <- normalize_ckm(long$group)
long$group_bin <- as.integer(long$group_factor == "Severe")
for (v in CHINA_FAT_VARS) long[[v]] <- as_numeric_strict(long[[v]], paste0("longitudinal ", v))
if (any(!stats::complete.cases(long[, CHINA_FAT_VARS, drop = FALSE]))) stop("Longitudinal PCA variables contain missing values.")

id_check <- long |>
  dplyr::group_by(ID) |>
  dplyr::summarise(n = dplyr::n(), n0 = sum(time == 0), n1 = sum(time == 1), .groups = "drop")
if (any(id_check$n != 2 | id_check$n0 != 1 | id_check$n1 != 1)) stop("Each longitudinal ID must have one baseline and one follow-up row.")
assert_count(nrow(id_check), EXPECTED_LONGITUDINAL_N, "Repeated-measures cohort")

# predict.prcomp uses the already oriented locked rotation.
long_scores <- stats::predict(china_pca_lock$fit, newdata = long[, CHINA_FAT_VARS, drop = FALSE])
long$PC1_Score <- long_scores[, "PC1"]
long$PC2_Score <- long_scores[, "PC2"]

wide <- long |>
  dplyr::select(ID, time, group_bin, PC1_Score, PC2_Score, dplyr::all_of(CHINA_FAT_VARS)) |>
  tidyr::pivot_wider(names_from = time, values_from = c(group_bin, PC1_Score, PC2_Score, dplyr::all_of(CHINA_FAT_VARS)), names_sep = "_")
wide$Progression_Status <- dplyr::case_when(
  wide$group_bin_0 == 0 & wide$group_bin_1 == 1 ~ "Progressor",
  wide$group_bin_0 == 1 & wide$group_bin_1 == 1 ~ "Stable Severe",
  wide$group_bin_0 == 0 & wide$group_bin_1 == 0 ~ "Stable Mild",
  wide$group_bin_0 == 1 & wide$group_bin_1 == 0 ~ "Severe to Mild",
  TRUE ~ "Other"
)
wide$Delta_PC1 <- wide$PC1_Score_1 - wide$PC1_Score_0
wide$Delta_PC2 <- wide$PC2_Score_1 - wide$PC2_Score_0
progressors <- wide[wide$Progression_Status == "Progressor", , drop = FALSE]
stable_severe <- wide[wide$Progression_Status == "Stable Severe", , drop = FALSE]
assert_count(nrow(progressors), EXPECTED_PROGRESSOR_N, "Mild-to-severe progressors")
assert_count(nrow(stable_severe), EXPECTED_STABLE_SEVERE_N, "Stable-severe reference")

long_tests <- data.frame(
  Outcome = c("PC1 within progressors", "PC2 within progressors", "Delta PC1: progressor vs stable severe", "Delta PC2: progressor vs stable severe"),
  Test = c("Wilcoxon signed-rank", "Wilcoxon signed-rank", "Wilcoxon rank-sum", "Wilcoxon rank-sum"),
  P = c(
    stats::wilcox.test(progressors$Delta_PC1, mu = 0, exact = FALSE)$p.value,
    stats::wilcox.test(progressors$Delta_PC2, mu = 0, exact = FALSE)$p.value,
    stats::wilcox.test(progressors$Delta_PC1, stable_severe$Delta_PC1, exact = FALSE)$p.value,
    stats::wilcox.test(progressors$Delta_PC2, stable_severe$Delta_PC2, exact = FALSE)$p.value
  ), stringsAsFactors = FALSE
)
long_t_sensitivity <- data.frame(
  Outcome = c("PC1 within progressors", "PC2 within progressors"),
  P = c(stats::t.test(progressors$Delta_PC1, mu = 0)$p.value,
        stats::t.test(progressors$Delta_PC2, mu = 0)$p.value)
)
long_summary <- list(
  paired = wide, progressors = progressors, stable_severe = stable_severe,
  tests = long_tests, t_sensitivity = long_t_sensitivity,
  pc2_decrease_percent = 100 * mean(progressors$Delta_PC2 < 0)
)

# ==============================================================================
# 4. NHANES external replication and fixed-loading transportability
# ==============================================================================

nh <- readRDS(NHANES_EXTERNAL_READY_FILE)
if (!is.data.frame(nh)) stop("NHANES external-ready RDS must contain a data.frame.")
nh_required <- c("SEQN", "cycle", "sex", "current_smoker", "age", "WTSAF8YR", "WTSAF2YR",
                 "STRATA_8YR", "PSU_8YR", "ckm_stage_primary_revised", "advanced_ckm_primary_revised", NHANES_PCA_VARS)
assert_columns(nh, nh_required, "NHANES external-ready RDS")
assert_count(nrow(nh), EXPECTED_NHANES_N, "NHANES external cohort")
assert_count(sum(nh$advanced_ckm_primary_revised == 1, na.rm = TRUE), EXPECTED_NHANES_ADVANCED, "NHANES advanced CKM events")
if (any(!stats::complete.cases(nh[, NHANES_PCA_VARS, drop = FALSE]))) stop("NHANES PCA variables contain missing values.")

# Preserve existing textual coding when possible; fail rather than guess unknown values.
nh$sex_f <- normalize_sex(nh$sex)
nh$smoke_f <- normalize_binary_factor(nh$current_smoker, variable = "NHANES current_smoker")
nh$advanced_ckm <- as.integer(nh$advanced_ckm_primary_revised)
nh$stage <- as.integer(nh$ckm_stage_primary_revised)

nh_design_all <- survey::svydesign(ids = ~PSU_8YR, strata = ~STRATA_8YR, weights = ~WTSAF8YR,
                                   nest = TRUE, data = nh)

nh_pca_input <- nh[, NHANES_PCA_VARS, drop = FALSE]
nh_pca <- stats::prcomp(nh_pca_input, center = TRUE, scale. = TRUE)
nh_pca <- orient_pca(nh_pca, vat_name = "VAT_area", leg_names = c("LL_pct", "RL_pct"))
nh$PC1 <- nh_pca$x[, "PC1"]; nh$PC2 <- nh_pca$x[, "PC2"]
for (nm in c("PC1", "PC2", "BMI", "VAT_area")) {
  ms <- weighted_mean_sd(nh[[nm]], nh$WTSAF8YR)
  outnm <- switch(nm, PC1 = "PC1_z", PC2 = "PC2_z", BMI = "BMI_z", VAT_area = "VAT_z")
  nh[[outnm]] <- (nh[[nm]] - ms["mean"]) / ms["sd"]
}

# Survey-weighted PCA sensitivity.
nh_weighted_pca <- survey::svyprcomp(
  stats::as.formula(paste("~", paste(NHANES_PCA_VARS, collapse = "+"))),
  design = nh_design_all, center = TRUE, scale. = TRUE, scores = FALSE
)
weighted_rotation <- nh_weighted_pca$rotation
# Orient using the same rule without relying on a prcomp score matrix.
if (mean(weighted_rotation[, "PC1"]) < 0) weighted_rotation[, "PC1"] <- -weighted_rotation[, "PC1"]
if ((mean(weighted_rotation[c("LL_pct", "RL_pct"), "PC2"]) - weighted_rotation["VAT_area", "PC2"]) < 0) {
  weighted_rotation[, "PC2"] <- -weighted_rotation[, "PC2"]
}

# Cross-platform loading comparison from locked China PCA object, not hard-coded values.
china_pc1_can <- map_loading_to_canonical(china_pca_lock$rotation, CHINA_TO_CANONICAL, "PC1")
china_pc2_can <- map_loading_to_canonical(china_pca_lock$rotation, CHINA_TO_CANONICAL, "PC2")
nh_pc1_can <- map_loading_to_canonical(nh_pca$rotation, NHANES_TO_CANONICAL, "PC1")
nh_pc2_can <- map_loading_to_canonical(nh_pca$rotation, NHANES_TO_CANONICAL, "PC2")
loading_comparison <- data.frame(
  Variable = CANONICAL_VARS,
  China_PC1 = unname(china_pc1_can), NHANES_PC1 = unname(nh_pc1_can),
  China_PC2 = unname(china_pc2_can), NHANES_PC2 = unname(nh_pc2_can)
)
loading_similarity <- data.frame(
  Component = c("PC1", "PC2"),
  Tucker_phi = c(tucker_phi(china_pc1_can, nh_pc1_can), tucker_phi(china_pc2_can, nh_pc2_can)),
  Pearson_r = c(stats::cor(china_pc1_can, nh_pc1_can), stats::cor(china_pc2_can, nh_pc2_can))
)

# Fixed China-loading transfer. NHANES raw variables are standardized within NHANES
# using ordinary center/scale, matching the original PCA definition.
nh_z <- scale(as.matrix(nh[, NHANES_PCA_VARS, drop = FALSE]), center = TRUE, scale = TRUE)
# Build China loading vectors in NHANES variable order via canonical names.
china_pc1_for_nh <- china_pc1_can[unname(NHANES_TO_CANONICAL)]
china_pc2_for_nh <- china_pc2_can[unname(NHANES_TO_CANONICAL)]
transfer_raw <- cbind(
  PC1 = as.numeric(nh_z %*% china_pc1_for_nh),
  PC2 = as.numeric(nh_z %*% china_pc2_for_nh)
)
nh$China_loading_PC1 <- transfer_raw[, "PC1"]
nh$China_loading_PC2 <- transfer_raw[, "PC2"]
for (nm in c("China_loading_PC1", "China_loading_PC2")) {
  ms <- weighted_mean_sd(nh[[nm]], nh$WTSAF8YR)
  nh[[paste0(nm, "_z")]] <- (nh[[nm]] - ms["mean"]) / ms["sd"]
}
score_concordance <- data.frame(
  Component = c("PC1", "PC2"),
  Pearson_r = c(stats::cor(nh$PC1_z, nh$China_loading_PC1_z),
                stats::cor(nh$PC2_z, nh$China_loading_PC2_z))
)

# Conditional NHANES bootstrap for loading stability / Tucker congruence.
set.seed(SEED_BOOT)
ref_nh <- nh_pca$rotation[, c("PC1", "PC2"), drop = FALSE]
boot_phi <- matrix(NA_real_, nrow = N_PCA_BOOT, ncol = 2, dimnames = list(NULL, c("PC1", "PC2")))
boot_pc1 <- matrix(NA_real_, nrow = N_PCA_BOOT, ncol = length(NHANES_PCA_VARS), dimnames = list(NULL, NHANES_PCA_VARS))
boot_pc2 <- matrix(NA_real_, nrow = N_PCA_BOOT, ncol = length(NHANES_PCA_VARS), dimnames = list(NULL, NHANES_PCA_VARS))
for (b in seq_len(N_PCA_BOOT)) {
  idx <- sample.int(nrow(nh_pca_input), nrow(nh_pca_input), replace = TRUE)
  fb <- stats::prcomp(nh_pca_input[idx, , drop = FALSE], center = TRUE, scale. = TRUE)
  mt <- match_boot_components(ref_nh, fb$rotation)
  boot_pc1[b, ] <- mt$PC1
  boot_pc2[b, ] <- mt$PC2
  boot_phi[b, "PC1"] <- tucker_phi(china_pc1_can, map_loading_to_canonical(cbind(PC1 = mt$PC1, PC2 = mt$PC2), NHANES_TO_CANONICAL, "PC1"))
  boot_phi[b, "PC2"] <- tucker_phi(china_pc2_can, map_loading_to_canonical(cbind(PC1 = mt$PC1, PC2 = mt$PC2), NHANES_TO_CANONICAL, "PC2"))
}
bootstrap_phi_summary <- data.frame(
  Component = c("PC1", "PC2"),
  Point = loading_similarity$Tucker_phi,
  Lower = apply(boot_phi, 2, stats::quantile, probs = .025, na.rm = TRUE),
  Upper = apply(boot_phi, 2, stats::quantile, probs = .975, na.rm = TRUE)
)

# Optional two-sample bootstrap, accounting for uncertainty in BOTH cohorts.
two_sample_phi <- NULL
if (isTRUE(RUN_TWO_SAMPLE_PCA_BOOTSTRAP)) {
  set.seed(SEED_BOOT + 1L)
  ref_ch <- china_pca_lock$rotation[, c("PC1", "PC2"), drop = FALSE]
  two_sample_phi <- matrix(NA_real_, nrow = N_PCA_BOOT, ncol = 2, dimnames = list(NULL, c("PC1", "PC2")))
  for (b in seq_len(N_PCA_BOOT)) {
    ci <- sample.int(nrow(fat_mat), nrow(fat_mat), replace = TRUE)
    ni <- sample.int(nrow(nh_pca_input), nrow(nh_pca_input), replace = TRUE)
    fc <- stats::prcomp(fat_mat[ci, , drop = FALSE], center = TRUE, scale. = TRUE)
    fn <- stats::prcomp(nh_pca_input[ni, , drop = FALSE], center = TRUE, scale. = TRUE)
    mc <- match_boot_components(ref_ch, fc$rotation)
    mn <- match_boot_components(ref_nh, fn$rotation)
    cmat <- cbind(PC1 = mc$PC1, PC2 = mc$PC2)
    nmat <- cbind(PC1 = mn$PC1, PC2 = mn$PC2)
    two_sample_phi[b, "PC1"] <- tucker_phi(
      map_loading_to_canonical(cmat, CHINA_TO_CANONICAL, "PC1"),
      map_loading_to_canonical(nmat, NHANES_TO_CANONICAL, "PC1")
    )
    two_sample_phi[b, "PC2"] <- tucker_phi(
      map_loading_to_canonical(cmat, CHINA_TO_CANONICAL, "PC2"),
      map_loading_to_canonical(nmat, NHANES_TO_CANONICAL, "PC2")
    )
  }
}

nh$external_model_complete <- stats::complete.cases(
  nh[, c("advanced_ckm", "PC1_z", "PC2_z", "China_loading_PC1_z",
         "China_loading_PC2_z", "sex_f", "smoke_f"), drop = FALSE]
)
# Rebuild after adding the explicit analysis-domain indicator.
nh_design_all <- survey::svydesign(ids = ~PSU_8YR, strata = ~STRATA_8YR, weights = ~WTSAF8YR,
                                   nest = TRUE, data = nh)
# `subset()` is a base S3 generic. The survey package supplies the
# subset.survey.design method, but `subset` itself is not exported from
# namespace:survey. Calling the generic directly allows proper S3 dispatch.
design_model <- subset(nh_design_all, external_model_complete)
fit_nh_pca <- survey::svyglm(advanced_ckm ~ PC1_z + PC2_z + sex_f + smoke_f,
                             design = design_model, family = stats::quasibinomial())
fit_fixed <- survey::svyglm(advanced_ckm ~ China_loading_PC1_z + China_loading_PC2_z + sex_f + smoke_f,
                            design = design_model, family = stats::quasibinomial())
design_234 <- subset(design_model, stage >= 2)
fit_fixed_234 <- survey::svyglm(advanced_ckm ~ China_loading_PC1_z + China_loading_PC2_z + sex_f + smoke_f,
                                design = design_234, family = stats::quasibinomial())
external_or <- rbind(
  transform(extract_svy_or(fit_nh_pca, design_model, "PC1_z"), Analysis = "NHANES-derived PC1"),
  transform(extract_svy_or(fit_nh_pca, design_model, "PC2_z"), Analysis = "NHANES-derived PC2"),
  transform(extract_svy_or(fit_fixed, design_model, "China_loading_PC1_z"), Analysis = "Fixed China-loading PC1"),
  transform(extract_svy_or(fit_fixed, design_model, "China_loading_PC2_z"), Analysis = "Fixed China-loading PC2"),
  transform(extract_svy_or(fit_fixed_234, design_234, "China_loading_PC1_z"), Analysis = "Fixed China-loading PC1, Stage 2 vs 3-4"),
  transform(extract_svy_or(fit_fixed_234, design_234, "China_loading_PC2_z"), Analysis = "Fixed China-loading PC2, Stage 2 vs 3-4")
)

# NHANES age sensitivity and interaction.
nh$Age10 <- (nh$age - mean(nh$age, na.rm = TRUE)) / 10
nh_design_age <- survey::svydesign(ids = ~PSU_8YR, strata = ~STRATA_8YR, weights = ~WTSAF8YR, nest = TRUE, data = nh)
fit_nh_age <- survey::svyglm(advanced_ckm ~ PC1_z + PC2_z + sex_f + smoke_f + Age10,
                             design = nh_design_age, family = stats::quasibinomial())
fit_nh_age_int <- survey::svyglm(advanced_ckm ~ PC1_z + PC2_z * Age10 + sex_f + smoke_f,
                                 design = nh_design_age, family = stats::quasibinomial())
fit_fixed_age <- survey::svyglm(advanced_ckm ~ China_loading_PC1_z + China_loading_PC2_z + sex_f + smoke_f + Age10,
                                design = nh_design_age, family = stats::quasibinomial())
fit_fixed_age_int <- survey::svyglm(advanced_ckm ~ China_loading_PC1_z + China_loading_PC2_z * Age10 + sex_f + smoke_f,
                                    design = nh_design_age, family = stats::quasibinomial())
age_sensitivity_nh <- rbind(
  transform(extract_svy_or(fit_nh_age, nh_design_age, "PC1_z"), Analysis = "NHANES-derived PC1 + age"),
  transform(extract_svy_or(fit_nh_age, nh_design_age, "PC2_z"), Analysis = "NHANES-derived PC2 + age"),
  transform(extract_svy_or(fit_fixed_age, nh_design_age, "China_loading_PC1_z"), Analysis = "Fixed China-loading PC1 + age"),
  transform(extract_svy_or(fit_fixed_age, nh_design_age, "China_loading_PC2_z"), Analysis = "Fixed China-loading PC2 + age")
)

# Sex interaction.
fit_fixed_sex_int <- survey::svyglm(
  advanced_ckm ~ China_loading_PC1_z + China_loading_PC2_z * sex_f + smoke_f,
  design = nh_design_age, family = stats::quasibinomial()
)

# Leave-one-cycle-out, with correct 2-year fasting-weight rescaling.
cycle_values <- sort(unique(as.character(nh$cycle)))
loco <- do.call(rbind, lapply(cycle_values, function(excl) {
  d <- nh[as.character(nh$cycle) != excl, , drop = FALSE]
  d$LOCO_WT <- d$WTSAF2YR / length(unique(as.character(d$cycle)))
  des <- survey::svydesign(ids = ~PSU_8YR, strata = ~STRATA_8YR, weights = ~LOCO_WT, nest = TRUE, data = d)
  fit <- survey::svyglm(advanced_ckm ~ China_loading_PC1_z + China_loading_PC2_z + sex_f + smoke_f,
                        design = des, family = stats::quasibinomial())
  z <- extract_svy_or(fit, des, "China_loading_PC2_z")
  transform(z, Excluded_cycle = excl, N = nrow(d), Events = sum(d$advanced_ckm == 1, na.rm = TRUE))
}))

# Optional alcohol sensitivity. Missing local ALQ files cause a warning and skip, not pipeline failure.
alcohol_sensitivity <- NULL
if (isTRUE(RUN_NHANES_ALCOHOL)) {
  alq_info <- data.frame(
    cycle = c("2011-2012", "2013-2014", "2015-2016", "2017-2018"),
    suffix = c("G", "H", "I", "J"), stringsAsFactors = FALSE
  )
  alq_info$file <- file.path(NHANES_RAW_DIR, paste0("ALQ_", alq_info$suffix, ".XPT"))
  if (all(file.exists(alq_info$file))) {
    alq_list <- lapply(seq_len(nrow(alq_info)), function(i) {
      z <- haven::read_xpt(alq_info$file[i]); names(z) <- toupper(names(z))
      if (alq_info$suffix[i] %in% c("G", "H", "I")) {
        assert_columns(z, c("SEQN", "ALQ120Q"), basename(alq_info$file[i]))
        use <- dplyr::case_when(
          as.numeric(z$ALQ120Q) == 0 ~ FALSE,
          as.numeric(z$ALQ120Q) > 0 & !as.numeric(z$ALQ120Q) %in% c(777, 999) ~ TRUE,
          TRUE ~ NA
        )
      } else {
        assert_columns(z, c("SEQN", "ALQ121"), basename(alq_info$file[i]))
        use <- dplyr::case_when(
          as.numeric(z$ALQ121) == 0 ~ FALSE,
          as.numeric(z$ALQ121) >= 1 & as.numeric(z$ALQ121) <= 10 ~ TRUE,
          TRUE ~ NA
        )
      }
      data.frame(SEQN = as.numeric(z$SEQN), cycle = alq_info$cycle[i], alcohol_past12m = use)
    })
    alq <- do.call(rbind, alq_list)
    nh_alc <- dplyr::left_join(nh, alq, by = c("SEQN", "cycle"))
    nh_alc$alcohol_f <- factor(nh_alc$alcohol_past12m, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
    cc <- stats::complete.cases(nh_alc[, c("advanced_ckm", "China_loading_PC1_z", "China_loading_PC2_z", "sex_f", "smoke_f", "alcohol_f", "WTSAF8YR", "STRATA_8YR", "PSU_8YR")])
    dd_alc <- nh_alc[cc, , drop = FALSE]
    des_alc <- survey::svydesign(ids = ~PSU_8YR, strata = ~STRATA_8YR, weights = ~WTSAF8YR, nest = TRUE, data = dd_alc)
    f0 <- survey::svyglm(advanced_ckm ~ China_loading_PC1_z + China_loading_PC2_z + sex_f + smoke_f,
                         design = des_alc, family = stats::quasibinomial())
    f1 <- survey::svyglm(advanced_ckm ~ China_loading_PC1_z + China_loading_PC2_z + sex_f + smoke_f + alcohol_f,
                         design = des_alc, family = stats::quasibinomial())
    alcohol_sensitivity <- list(
      N = nrow(dd_alc),
      no_alcohol_adjustment = extract_svy_or(f0, des_alc, "China_loading_PC2_z"),
      alcohol_adjusted = extract_svy_or(f1, des_alc, "China_loading_PC2_z")
    )
  } else {
    warning("ALQ G/H/I/J XPT files were not all found in NHANES_RAW_DIR; alcohol sensitivity skipped.", call. = FALSE)
  }
}

# Descriptive external discrimination on a common cohort; these are apparent
# performance estimates in NHANES and should remain secondary/descriptive.
perf_vars <- c("advanced_ckm", "sex_f", "smoke_f", "BMI_z", "VAT_z", "PC1_z", "PC2_z", "WTSAF8YR", "STRATA_8YR", "PSU_8YR")
perf_dat <- nh[stats::complete.cases(nh[, perf_vars]), , drop = FALSE]
perf_des <- survey::svydesign(ids = ~PSU_8YR, strata = ~STRATA_8YR, weights = ~WTSAF8YR, nest = TRUE, data = perf_dat)
ext_formulas <- list(
  Baseline = advanced_ckm ~ sex_f + smoke_f,
  BMI = advanced_ckm ~ sex_f + smoke_f + BMI_z,
  VAT = advanced_ckm ~ sex_f + smoke_f + VAT_z,
  FDP = advanced_ckm ~ sex_f + smoke_f + PC1_z + PC2_z
)
ext_fits <- lapply(ext_formulas, function(fm) survey::svyglm(fm, design = perf_des, family = stats::quasibinomial()))
ext_y <- perf_dat$advanced_ckm; ext_w <- perf_dat$WTSAF8YR
external_performance <- do.call(rbind, lapply(names(ext_fits), function(nm) {
  p <- as.numeric(stats::fitted(ext_fits[[nm]]))
  data.frame(Model = nm, Weighted_AUROC = weighted_auc(ext_y, p, ext_w),
             Weighted_Brier = weighted_brier(ext_y, p, ext_w))
}))

# ==============================================================================
# 5. Secondary strict nested CV (optional)
# ==============================================================================

ml_results <- NULL
if (isTRUE(RUN_INTERNAL_ML)) {
  ml <- china_raw
  ml$group_factor <- normalize_ckm(ml$group)
  ml$group_factor <- factor(as.character(ml$group_factor), levels = c("Severe", "Mild"))
  ml$Sex <- normalize_sex(ml$Sex)
  ml$Smoke <- normalize_binary_factor(ml$Smoke, variable = "ML Smoke")
  ml$Alcohol_Use <- normalize_binary_factor(ml$Alcohol_Use, variable = "ML Alcohol_Use")
  for (v in CHINA_FAT_VARS) ml[[v]] <- as_numeric_strict(ml[[v]], paste0("ML ", v))
  ml <- ml[stats::complete.cases(ml[, c("group_factor", CHINA_BASELINE_VARS, CHINA_FAT_VARS)]), , drop = FALSE]
  assert_count(nrow(ml), EXPECTED_DERIVATION_N, "ML complete-case cohort")
  # Severe must be first because caret::twoClassSummary uses first level as event.
  ml$group_factor <- factor(ml$group_factor, levels = c("Severe", "Mild"))

  set.seed(SEED_MAIN)
  outer_train <- caret::createFolds(ml$group_factor, k = OUTER_FOLDS, returnTrain = TRUE)
  all_oof <- vector("list", length(outer_train)); tune_log <- vector("list", length(outer_train))

  make_recipe <- function(dat) {
    rec <- recipes::recipe(
      group_factor ~ Sex + Smoke + Alcohol_Use + `LL%` + `LA%` + `RA%` + `RL%` + `Trunk%` + BMI + VFA,
      data = dat
    )
    rec <- recipes::step_pca(rec, tidyselect::all_of(CHINA_FAT_VARS), num_comp = 2,
                             options = list(center = TRUE, scale. = TRUE, retx = FALSE))
    rec <- recipes::step_dummy(rec, recipes::all_nominal_predictors())
    rec <- recipes::step_zv(rec, recipes::all_predictors())
    # Critical fix: after zero-variance removal, PC scores/dummies are normalized
    # inside each resampling fold for KNN/SVM/penalized models.
    recipes::step_normalize(rec, recipes::all_numeric_predictors())
  }

  for (fold in seq_along(outer_train)) {
    tr_idx <- outer_train[[fold]]; te_idx <- setdiff(seq_len(nrow(ml)), tr_idx)
    tr <- ml[tr_idx, , drop = FALSE]; te <- ml[te_idx, , drop = FALSE]
    set.seed(SEED_MAIN + fold)
    inner_train <- caret::createFolds(tr$group_factor, k = INNER_FOLDS, returnTrain = TRUE)
    inner_test <- lapply(inner_train, function(i) setdiff(seq_len(nrow(tr)), i))
    ctrl <- caret::trainControl(method = "cv", index = inner_train, indexOut = inner_test,
                                classProbs = TRUE, summaryFunction = caret::twoClassSummary,
                                savePredictions = "final", allowParallel = FALSE)
    rec <- make_recipe(tr)
    specs <- list(
      LR = list(method = "glm", extra = list(family = stats::binomial())),
      RF = list(method = "rf", extra = list(tuneLength = ML_TUNE_LENGTH)),
      GBM = list(method = "gbm", extra = list(tuneLength = ML_TUNE_LENGTH, verbose = FALSE)),
      SVM = list(method = "svmRadial", extra = list(tuneLength = ML_TUNE_LENGTH)),
      ElasticNet = list(method = "glmnet", extra = list(tuneLength = ML_TUNE_LENGTH)),
      KNN = list(method = "knn", extra = list(tuneLength = ML_TUNE_LENGTH))
    )
    fits <- list()
    for (nm in names(specs)) {
      set.seed(SEED_MAIN + 1000L * fold + match(nm, names(specs)))
      args <- c(list(x = rec, data = tr, method = specs[[nm]]$method, metric = "ROC", trControl = ctrl), specs[[nm]]$extra)
      fits[[nm]] <- do.call(caret::train, args)
    }
    pred <- data.frame(row_id = te$row_id, Outer_Fold = fold, Outcome = as.character(te$group_factor))
    for (nm in names(fits)) pred[[paste0("prob_", nm)]] <- stats::predict(fits[[nm]], newdata = te, type = "prob")[, "Severe"]
    all_oof[[fold]] <- pred
    # Different caret models have different tuning-parameter columns
    # (e.g. RF: mtry; GBM: n.trees/interaction.depth/shrinkage/n.minobsinnode;
    # SVM: sigma/C; glmnet: alpha/lambda; KNN: k; GLM: no tuning columns).
    # Base rbind() requires identical column names and therefore fails here.
    # dplyr::bind_rows() safely takes the union of columns and fills absent
    # tuning parameters with NA.
    tune_log[[fold]] <- dplyr::bind_rows(lapply(names(fits), function(nm) {
      best_tune <- as.data.frame(fits[[nm]]$bestTune, check.names = FALSE)
      if (nrow(best_tune) == 0L) best_tune <- data.frame(.placeholder = NA)[, FALSE, drop = FALSE]
      cbind(
        data.frame(
          Outer_Fold = fold,
          Model = nm,
          Inner_best_ROC = max(fits[[nm]]$results$ROC, na.rm = TRUE),
          stringsAsFactors = FALSE
        ),
        best_tune
      )
    }))
  }
  oof_alg <- do.call(rbind, all_oof)
  oof_alg <- oof_alg[order(oof_alg$row_id), , drop = FALSE]
  y <- as.integer(oof_alg$Outcome == "Severe")
  alg_perf <- do.call(rbind, lapply(ML_ALGORITHMS, function(nm) {
    p <- oof_alg[[paste0("prob_", nm)]]
    roc <- pROC::roc(y, p, levels = c(0, 1), direction = "<", quiet = TRUE)
    pr <- PRROC::pr.curve(scores.class0 = p[y == 1], scores.class1 = p[y == 0], curve = FALSE)
    data.frame(Model = nm, AUROC = as.numeric(pROC::auc(roc)), AUPRC = pr$auc.integral,
               Brier = mean((y - p)^2))
  }))

  # GBM comparator analysis, using identical outer/inner partitions.
  comp_oof <- vector("list", length(outer_train))
  for (fold in seq_along(outer_train)) {
    tr_idx <- outer_train[[fold]]; te_idx <- setdiff(seq_len(nrow(ml)), tr_idx)
    tr <- ml[tr_idx, , drop = FALSE]; te <- ml[te_idx, , drop = FALSE]
    set.seed(SEED_MAIN + fold)
    inner_train <- caret::createFolds(tr$group_factor, k = INNER_FOLDS, returnTrain = TRUE)
    inner_test <- lapply(inner_train, function(i) setdiff(seq_len(nrow(tr)), i))
    ctrl <- caret::trainControl(method = "cv", index = inner_train, indexOut = inner_test,
                                classProbs = TRUE, summaryFunction = caret::twoClassSummary, allowParallel = FALSE)
    fm <- list(
      Baseline = group_factor ~ Sex + Smoke + Alcohol_Use,
      BMI = group_factor ~ Sex + Smoke + Alcohol_Use + BMI,
      VFA = group_factor ~ Sex + Smoke + Alcohol_Use + VFA
    )
    pred <- data.frame(row_id = te$row_id, Outer_Fold = fold, Outcome = as.character(te$group_factor))
    for (nm in names(fm)) {
      set.seed(SEED_MAIN + 5000L + fold)
      fit <- caret::train(fm[[nm]], data = tr, method = "gbm", metric = "ROC", tuneLength = ML_TUNE_LENGTH,
                          verbose = FALSE, trControl = ctrl)
      pred[[paste0("prob_", nm)]] <- stats::predict(fit, newdata = te, type = "prob")[, "Severe"]
    }
    # Reuse strict GBM OOF from the all-algorithm FDP pipeline.
    pred$prob_FDP <- oof_alg$prob_GBM[match(pred$row_id, oof_alg$row_id)]
    comp_oof[[fold]] <- pred
  }
  oof_comp <- do.call(rbind, comp_oof); oof_comp <- oof_comp[order(oof_comp$row_id), , drop = FALSE]
  yc <- as.integer(oof_comp$Outcome == "Severe")
  comp_names <- c("Baseline", "BMI", "VFA", "FDP")
  comp_perf <- dplyr::bind_rows(lapply(comp_names, function(nm) {
    p <- oof_comp[[paste0("prob_", nm)]]
    if (is.null(p) || length(p) != length(yc)) {
      stop("Comparator probability vector is missing or has the wrong length for model: ", nm, call. = FALSE)
    }

    ok_perf <- is.finite(yc) & is.finite(p)
    if (sum(ok_perf) < 10L || length(unique(yc[ok_perf])) < 2L) {
      stop("Too few valid observations or only one outcome class for comparator model: ", nm, call. = FALSE)
    }

    roc_obj <- pROC::roc(
      yc[ok_perf], p[ok_perf],
      levels = c(0, 1), direction = "<", quiet = TRUE
    )
    cal <- calc_calibration(yc[ok_perf], p[ok_perf])

    data.frame(
      Model = nm,
      AUROC = as.numeric(pROC::auc(roc_obj)),
      Brier = mean((yc[ok_perf] - p[ok_perf])^2),
      Calibration_Intercept = unname(cal[["intercept"]]),
      Calibration_Slope = unname(cal[["slope"]]),
      stringsAsFactors = FALSE,
      row.names = NULL,
      check.names = FALSE
    )
  }))

  nri_idi <- NULL
  if (isTRUE(RUN_NRI_IDI)) {
    nri_idi <- do.call(rbind, lapply(c("Baseline", "BMI", "VFA"), function(ref) {
      est <- continuous_nri_idi(yc, oof_comp[[paste0("prob_", ref)]], oof_comp$prob_FDP)
      data.frame(Reference = ref, t(est), row.names = NULL)
    }))
  }
  dca <- do.call(rbind, lapply(comp_names, function(nm) {
    z <- net_benefit(yc, oof_comp[[paste0("prob_", nm)]])
    transform(z, Model = nm)
  }))

  ml_results <- list(
    oof_algorithms = oof_alg,
    tuning = dplyr::bind_rows(tune_log),
    algorithm_performance = alg_perf,
    oof_comparators = oof_comp,
    comparator_performance = comp_perf,
    nri_idi = nri_idi,
    dca = dca
  )
}

# ==============================================================================
# 6. Optional full-cohort SHAP for interpretation ONLY
# ==============================================================================

shap_results <- NULL
if (isTRUE(RUN_SHAP) && isTRUE(RUN_INTERNAL_ML) && !is.null(ml_results)) {
  # Uses CURRENT locked Stage-01 scores, fixing the previous stale Stage-03 path.
  sh <- assoc[, c("severe", "Sex_f", "Smoke_f", "Alcohol_f", "PC1_Score", "PC2_Score")]
  sh$Outcome <- factor(ifelse(sh$severe == 1, "Severe", "Mild"), levels = c("Severe", "Mild"))
  X <- stats::model.matrix(~ Sex_f + Smoke_f + Alcohol_f + PC1_Score + PC2_Score, data = sh)[, -1, drop = FALSE]
  ysh <- sh$Outcome
  # Use a modest fixed GBM tuning specification for interpretation. No performance is reported.
  set.seed(SEED_SHAP)
  gbm_fit <- gbm::gbm.fit(
    x = X, y = as.integer(ysh == "Severe"), distribution = "bernoulli",
    n.trees = 500, interaction.depth = 2, shrinkage = 0.03,
    n.minobsinnode = 10, bag.fraction = 0.8, verbose = FALSE
  )
  pred_wrapper <- function(object, newdata) {
    stats::predict(object, newdata = as.matrix(newdata), n.trees = object$n.trees, type = "response")
  }
  set.seed(SEED_SHAP)
  sv <- fastshap::explain(gbm_fit, X = as.data.frame(X), pred_wrapper = pred_wrapper, nsim = 500, adjust = TRUE)
  shap_results <- list(
    model = gbm_fit,
    X = as.data.frame(X),
    shap = as.data.frame(sv),
    importance = data.frame(Feature = colnames(sv), MeanAbsSHAP = colMeans(abs(sv)))
  )
}


# ==============================================================================
# 6B. Figure-ready supplementary summaries (no plotting here)
# ==============================================================================

# ---- S2: strict nested-CV algorithm benchmark --------------------------------
supp_ml <- NULL
if (isTRUE(RUN_INTERNAL_ML) && !is.null(ml_results)) {
  y_alg <- as.integer(ml_results$oof_algorithms$Outcome == "Severe")
  alg_rows <- lapply(ML_ALGORITHMS, function(nm) {
    p <- ml_results$oof_algorithms[[paste0("prob_", nm)]]
    ok <- is.finite(y_alg) & is.finite(p)
    roc_obj <- pROC::roc(y_alg[ok], p[ok], levels = c(0, 1), direction = "<", quiet = TRUE)
    ci <- suppressWarnings(as.numeric(pROC::ci.auc(roc_obj, method = "delong")))
    pr <- PRROC::pr.curve(scores.class0 = p[ok & y_alg == 1], scores.class1 = p[ok & y_alg == 0], curve = FALSE)
    data.frame(
      Model = nm,
      AUROC = as.numeric(pROC::auc(roc_obj)),
      AUROC_Lower = ci[1],
      AUROC_Upper = ci[3],
      AUPRC = pr$auc.integral,
      Brier = mean((y_alg[ok] - p[ok])^2),
      stringsAsFactors = FALSE
    )
  })
  alg_benchmark <- dplyr::bind_rows(alg_rows)

  # Winner of each outer fold based on the best inner-CV ROC for each algorithm.
  tuning_compact <- ml_results$tuning |>
    dplyr::group_by(Outer_Fold, Model) |>
    dplyr::summarise(Inner_best_ROC = max(Inner_best_ROC, na.rm = TRUE), .groups = "drop")
  fold_winners <- tuning_compact |>
    dplyr::group_by(Outer_Fold) |>
    dplyr::slice_max(order_by = Inner_best_ROC, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  selection_frequency <- data.frame(Model = ML_ALGORITHMS, stringsAsFactors = FALSE) |>
    dplyr::left_join(
      fold_winners |>
        dplyr::count(Model, name = "Selected_N_Folds"),
      by = "Model"
    ) |>
    dplyr::mutate(Selected_N_Folds = dplyr::coalesce(Selected_N_Folds, 0L))

  # ---- S3: comparator performance, calibration and DCA -----------------------
  oofc <- ml_results$oof_comparators
  yc2 <- as.integer(oofc$Outcome == "Severe")
  comp_names2 <- c("Baseline", "BMI", "VFA", "FDP")

  comparator_perf_ci <- dplyr::bind_rows(lapply(comp_names2, function(nm) {
    p <- oofc[[paste0("prob_", nm)]]
    ok <- is.finite(yc2) & is.finite(p)
    rr <- pROC::roc(yc2[ok], p[ok], levels = c(0, 1), direction = "<", quiet = TRUE)
    ci <- suppressWarnings(as.numeric(pROC::ci.auc(rr, method = "delong")))
    data.frame(
      Model = nm,
      AUROC = as.numeric(pROC::auc(rr)),
      AUROC_Lower = ci[1],
      AUROC_Upper = ci[3],
      Brier = mean((yc2[ok] - p[ok])^2),
      stringsAsFactors = FALSE
    )
  }))

  roc_fdp <- pROC::roc(yc2, oofc$prob_FDP, levels = c(0, 1), direction = "<", quiet = TRUE)
  comparator_auc_diffs <- dplyr::bind_rows(lapply(c("Baseline", "BMI", "VFA"), function(ref) {
    roc_ref <- pROC::roc(yc2, oofc[[paste0("prob_", ref)]], levels = c(0, 1), direction = "<", quiet = TRUE)
    tst <- suppressWarnings(pROC::roc.test(roc_fdp, roc_ref, paired = TRUE, method = "delong"))
    data.frame(
      Reference = ref,
      Delta_AUROC_FDP_minus_ref = as.numeric(pROC::auc(roc_fdp) - pROC::auc(roc_ref)),
      P = as.numeric(tst$p.value),
      stringsAsFactors = FALSE
    )
  }))

  # Flexible calibration curves from the locked OOF predictions.
  calibration_curves <- dplyr::bind_rows(lapply(comp_names2, function(nm) {
    p <- oofc[[paste0("prob_", nm)]]
    ok <- is.finite(yc2) & is.finite(p)
    dd <- data.frame(y = yc2[ok], p = p[ok])
    grid <- seq(stats::quantile(dd$p, .025), stats::quantile(dd$p, .975), length.out = 150)
    fit <- tryCatch(stats::loess(y ~ p, data = dd, span = 0.75, degree = 1,
                                 control = stats::loess.control(surface = "direct")),
                    error = function(e) NULL)
    pred <- if (is.null(fit)) rep(NA_real_, length(grid)) else
      pmin(pmax(as.numeric(stats::predict(fit, newdata = data.frame(p = grid))), 0), 1)
    data.frame(Model = nm, Predicted = grid, Observed = pred, stringsAsFactors = FALSE)
  }))

  # FDP decile calibration points.
  fdp_dat <- data.frame(y = yc2, p = oofc$prob_FDP) |>
    dplyr::filter(is.finite(y), is.finite(p)) |>
    dplyr::mutate(Decile = dplyr::ntile(p, 10))
  fdp_deciles <- fdp_dat |>
    dplyr::group_by(Decile) |>
    dplyr::summarise(Predicted = mean(p), Observed = mean(y), N = dplyr::n(), .groups = "drop")

  # Bootstrap calibration intercept/slope for FDP.
  set.seed(SEED_MAIN + 6100L)
  cal_boot <- matrix(NA_real_, nrow = N_METRIC_BOOT, ncol = 2,
                     dimnames = list(NULL, c("Intercept", "Slope")))
  for (b in seq_len(N_METRIC_BOOT)) {
    ii <- sample.int(nrow(oofc), nrow(oofc), replace = TRUE)
    cb <- calc_calibration(yc2[ii], oofc$prob_FDP[ii])
    cal_boot[b, ] <- c(cb[["intercept"]], cb[["slope"]])
  }
  cal_point <- calc_calibration(yc2, oofc$prob_FDP)
  calibration_bootstrap <- data.frame(
    Metric = c("Intercept", "Slope"),
    Estimate = c(cal_point[["intercept"]], cal_point[["slope"]]),
    Lower = apply(cal_boot, 2, stats::quantile, probs = .025, na.rm = TRUE),
    Upper = apply(cal_boot, 2, stats::quantile, probs = .975, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  # ---- S4: bootstrap NRI/IDI and NRI components ------------------------------
  nri_boot_summary <- NULL
  if (isTRUE(RUN_NRI_IDI)) {
    set.seed(SEED_MAIN + 6200L)
    nri_boot_summary <- dplyr::bind_rows(lapply(c("Baseline", "BMI", "VFA"), function(ref) {
      p_ref <- oofc[[paste0("prob_", ref)]]
      p_new <- oofc$prob_FDP
      point <- continuous_nri_idi(yc2, p_ref, p_new)
      bootm <- matrix(NA_real_, nrow = N_METRIC_BOOT, ncol = 4,
                      dimnames = list(NULL, names(point)))
      event_idx <- which(yc2 == 1)
      nonevent_idx <- which(yc2 == 0)
      for (b in seq_len(N_METRIC_BOOT)) {
        ii <- c(
          sample(event_idx, length(event_idx), replace = TRUE),
          sample(nonevent_idx, length(nonevent_idx), replace = TRUE)
        )
        bootm[b, ] <- continuous_nri_idi(yc2[ii], p_ref[ii], p_new[ii])
      }
      dplyr::bind_rows(lapply(names(point), function(metric) {
        data.frame(
          Reference = ref,
          Metric = metric,
          Estimate = unname(point[[metric]]),
          Lower = stats::quantile(bootm[, metric], .025, na.rm = TRUE),
          Upper = stats::quantile(bootm[, metric], .975, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      }))
    }))
  }

  dca_difference <- NULL
  if (!is.null(ml_results$dca)) {
    dca_wide <- ml_results$dca |>
      dplyr::select(threshold, net_benefit, Model) |>
      tidyr::pivot_wider(names_from = Model, values_from = net_benefit)
    dca_difference <- dplyr::bind_rows(lapply(c("Baseline", "BMI", "VFA"), function(ref) {
      data.frame(
        threshold = dca_wide$threshold,
        Reference = ref,
        FDP_minus_reference = dca_wide$FDP - dca_wide[[ref]],
        stringsAsFactors = FALSE
      )
    }))
  }

  supp_ml <- list(
    algorithm_benchmark = alg_benchmark,
    selection_frequency = selection_frequency,
    comparator_performance = comparator_perf_ci,
    comparator_auc_differences = comparator_auc_diffs,
    calibration_curves = calibration_curves,
    fdp_deciles = fdp_deciles,
    calibration_bootstrap = calibration_bootstrap,
    nri_bootstrap = nri_boot_summary,
    dca = ml_results$dca,
    dca_difference = dca_difference
  )
}

# ---- S9: NHANES loading bootstrap intervals -----------------------------------
boot_loading_summary <- dplyr::bind_rows(lapply(c("PC1", "PC2"), function(pc) {
  bm <- if (pc == "PC1") boot_pc1 else boot_pc2
  obs <- nh_pca$rotation[, pc]
  data.frame(
    Variable = rownames(nh_pca$rotation),
    Component = pc,
    Estimate = as.numeric(obs),
    Lower = apply(bm, 2, stats::quantile, probs = .025, na.rm = TRUE),
    Upper = apply(bm, 2, stats::quantile, probs = .975, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))

# ---- S10: external weighted ROC curve data ------------------------------------
weighted_roc_curve <- function(y, pred, w) {
  ok <- is.finite(y) & is.finite(pred) & is.finite(w) & w > 0 & y %in% c(0, 1)
  y <- y[ok]; pred <- pred[ok]; w <- w[ok]
  ord <- order(pred, decreasing = TRUE)
  y <- y[ord]; pred <- pred[ord]; w <- w[ord]
  pos_total <- sum(w[y == 1]); neg_total <- sum(w[y == 0])
  if (pos_total <= 0 || neg_total <= 0) {
    return(data.frame(FPR = numeric(0), TPR = numeric(0)))
  }
  tpr <- cumsum(w * (y == 1)) / pos_total
  fpr <- cumsum(w * (y == 0)) / neg_total
  keep <- !duplicated(pred, fromLast = TRUE)
  out <- data.frame(FPR = c(0, fpr[keep], 1), TPR = c(0, tpr[keep], 1))
  out[order(out$FPR, out$TPR), , drop = FALSE]
}
external_predictions <- data.frame(
  Outcome = ext_y,
  Weight = ext_w,
  prob_Baseline = as.numeric(stats::fitted(ext_fits$Baseline)),
  prob_BMI = as.numeric(stats::fitted(ext_fits$BMI)),
  prob_VAT = as.numeric(stats::fitted(ext_fits$VAT)),
  prob_FDP = as.numeric(stats::fitted(ext_fits$FDP)),
  stringsAsFactors = FALSE
)
external_roc <- dplyr::bind_rows(lapply(c("Baseline", "BMI", "VAT", "FDP"), function(nm) {
  z <- weighted_roc_curve(ext_y, external_predictions[[paste0("prob_", nm)]], ext_w)
  transform(z, Model = nm)
}))

# ---- S11: expanded alcohol sensitivity + CKM-stage PC2 gradient ---------------
alcohol_full_or <- NULL
if (isTRUE(RUN_NHANES_ALCOHOL) && exists("dd_alc") && nrow(dd_alc) > 0L) {
  f_nh0 <- survey::svyglm(advanced_ckm ~ PC1_z + PC2_z + sex_f + smoke_f,
                          design = des_alc, family = stats::quasibinomial())
  f_nh1 <- survey::svyglm(advanced_ckm ~ PC1_z + PC2_z + sex_f + smoke_f + alcohol_f,
                          design = des_alc, family = stats::quasibinomial())
  des_alc_234 <- subset(des_alc, stage >= 2)
  f_fixed_alc_234 <- survey::svyglm(
    advanced_ckm ~ China_loading_PC1_z + China_loading_PC2_z + sex_f + smoke_f + alcohol_f,
    design = des_alc_234, family = stats::quasibinomial()
  )
  alcohol_full_or <- dplyr::bind_rows(
    transform(extract_svy_or(f0, des_alc, "China_loading_PC2_z"),
              Analysis = "Fixed China loading: same alcohol-complete sample, no alcohol adjustment"),
    transform(extract_svy_or(f1, des_alc, "China_loading_PC2_z"),
              Analysis = "Fixed China loading: alcohol-adjusted"),
    transform(extract_svy_or(f_nh0, des_alc, "PC2_z"),
              Analysis = "NHANES-derived PC2: same alcohol-complete sample, no alcohol adjustment"),
    transform(extract_svy_or(f_nh1, des_alc, "PC2_z"),
              Analysis = "NHANES-derived PC2: alcohol-adjusted"),
    transform(extract_svy_or(f_fixed_alc_234, des_alc_234, "China_loading_PC2_z"),
              Analysis = "Fixed China loading: alcohol-adjusted Stage 2 vs Stage 3-4")
  )
}

stage_means_raw <- survey::svyby(~PC2_z, ~stage, nh_design_all, survey::svymean,
                                 na.rm = TRUE, vartype = "se")
stage_means_df <- as.data.frame(stage_means_raw)
se_col <- grep("^se", names(stage_means_df), value = TRUE)[1]
stage_gradient <- data.frame(
  Stage = as.numeric(stage_means_df$stage),
  Mean_PC2 = as.numeric(stage_means_df$PC2_z),
  SE = as.numeric(stage_means_df[[se_col]]),
  stringsAsFactors = FALSE
)
stage_gradient$Lower <- stage_gradient$Mean_PC2 - 1.96 * stage_gradient$SE
stage_gradient$Upper <- stage_gradient$Mean_PC2 + 1.96 * stage_gradient$SE

# ---- S12: age-adjustment comparison and PC2-by-age interactions --------------
primary_pc2_china <- china_or[china_or$Exposure == "PC2" & china_or$Model == "Adjusted", , drop = FALSE]
primary_pc2_nh <- external_or[external_or$Analysis == "NHANES-derived PC2", , drop = FALSE]
primary_pc2_fixed <- external_or[external_or$Analysis == "Fixed China-loading PC2", , drop = FALSE]
age_pc2_china <- if (!is.null(age_sensitivity_china)) age_sensitivity_china$PC2 else NULL
age_pc2_nh <- age_sensitivity_nh[age_sensitivity_nh$Analysis == "NHANES-derived PC2 + age", , drop = FALSE]
age_pc2_fixed <- age_sensitivity_nh[age_sensitivity_nh$Analysis == "Fixed China-loading PC2 + age", , drop = FALSE]

age_comparison <- dplyr::bind_rows(
  data.frame(
    Label = "China derivation",
    Primary_OR = primary_pc2_china$OR, Primary_Lower = primary_pc2_china$Lower, Primary_Upper = primary_pc2_china$Upper,
    Age_OR = if (!is.null(age_pc2_china)) age_pc2_china$OR else NA_real_,
    Age_Lower = if (!is.null(age_pc2_china)) age_pc2_china$Lower else NA_real_,
    Age_Upper = if (!is.null(age_pc2_china)) age_pc2_china$Upper else NA_real_
  ),
  data.frame(
    Label = "NHANES-derived PC2",
    Primary_OR = primary_pc2_nh$OR, Primary_Lower = primary_pc2_nh$Lower, Primary_Upper = primary_pc2_nh$Upper,
    Age_OR = age_pc2_nh$OR, Age_Lower = age_pc2_nh$Lower, Age_Upper = age_pc2_nh$Upper
  ),
  data.frame(
    Label = "Fixed China-loading PC2",
    Primary_OR = primary_pc2_fixed$OR, Primary_Lower = primary_pc2_fixed$Lower, Primary_Upper = primary_pc2_fixed$Upper,
    Age_OR = age_pc2_fixed$OR, Age_Lower = age_pc2_fixed$Lower, Age_Upper = age_pc2_fixed$Upper
  )
)

age_interactions <- dplyr::bind_rows(
  transform(extract_glm_or(age_sensitivity_china$interaction_fit, "PC2_Score:Age10"),
            Label = "China derivation"),
  transform(extract_svy_or(fit_nh_age_int, nh_design_age, "PC2_z:Age10"),
            Label = "NHANES-derived PC2"),
  transform(extract_svy_or(fit_fixed_age_int, nh_design_age, "China_loading_PC2_z:Age10"),
            Label = "Fixed China-loading PC2")
)

supplementary_support <- list(
  ml = supp_ml,
  nhanes_bootstrap_loadings = boot_loading_summary,
  external_predictions = external_predictions,
  external_roc = external_roc,
  alcohol_full_or = alcohol_full_or,
  stage_gradient = stage_gradient,
  age_comparison = age_comparison,
  age_interactions = age_interactions
)


# ==============================================================================
# 7. Save ONE locked master result object
# ==============================================================================

results <- list(
  metadata = list(
    created = Sys.time(),
    R_version = R.version.string,
    input_manifest = input_manifest,
    config = list(expected_n = c(china = EXPECTED_DERIVATION_N, longitudinal = EXPECTED_LONGITUDINAL_N,
                                 nhanes = EXPECTED_NHANES_N), seeds = c(SEED_MAIN, SEED_BOOT, SEED_EXTERNAL, SEED_SHAP))
  ),
  china = list(
    raw_analysis = china,
    correlation = china_corr,
    vif = china_vif,
    pca_lock = china_pca_lock,
    primary_models = list(pc1_crude = fit_pc1_crude, pc2_crude = fit_pc2_crude, adjusted = fit_primary),
    primary_or = china_or,
    rcs = list(pc1_model = fit_rcs_pc1, pc2_model = fit_rcs_pc2, pc1_curve = rcs_curve_pc1,
               pc2_curve = rcs_curve_pc2, p_values = rcs_p, knots_pc1 = k1, knots_pc2 = k2),
    age_sensitivity = age_sensitivity_china
  ),
  longitudinal = long_summary,
  nhanes = list(
    data = nh,
    pca = nh_pca,
    weighted_pca_rotation = weighted_rotation,
    loading_comparison = loading_comparison,
    loading_similarity = loading_similarity,
    bootstrap_phi = bootstrap_phi_summary,
    bootstrap_phi_raw = boot_phi,
    bootstrap_pc1_raw = boot_pc1,
    bootstrap_pc2_raw = boot_pc2,
    two_sample_phi_raw = two_sample_phi,
    score_concordance = score_concordance,
    models = list(nhanes_pca = fit_nh_pca, fixed = fit_fixed, fixed_stage234 = fit_fixed_234,
                  age = fit_nh_age, age_interaction = fit_nh_age_int,
                  fixed_age = fit_fixed_age, fixed_age_interaction = fit_fixed_age_int,
                  fixed_sex_interaction = fit_fixed_sex_int),
    external_or = external_or,
    age_sensitivity = age_sensitivity_nh,
    loco = loco,
    alcohol_sensitivity = alcohol_sensitivity,
    performance = external_performance
  ),
  ml = ml_results,
  shap = shap_results,
  supplementary = supplementary_support
)

# Manuscript-number audit. This does not force equality, but it makes any drift explicit.
current_expected <- data.frame(
  Metric = c("China adjusted PC1 OR", "China adjusted PC2 OR", "Progressor PC2 Wilcoxon P",
             "PC2 Tucker phi", "NHANES-derived PC2 OR", "Fixed China-loading PC2 OR", "PC2 score Pearson r"),
  Expected = c(1.14, 0.57, 0.013, 0.952, 0.55, 0.58, 0.926),
  Observed = c(
    china_or$OR[china_or$Exposure == "PC1" & china_or$Model == "Adjusted"],
    china_or$OR[china_or$Exposure == "PC2" & china_or$Model == "Adjusted"],
    long_tests$P[long_tests$Outcome == "PC2 within progressors"],
    loading_similarity$Tucker_phi[loading_similarity$Component == "PC2"],
    external_or$OR[external_or$Analysis == "NHANES-derived PC2"],
    external_or$OR[external_or$Analysis == "Fixed China-loading PC2"],
    score_concordance$Pearson_r[score_concordance$Component == "PC2"]
  ),
  Tolerance = c(.03, .05, .01, .01, .05, .05, .02),
  stringsAsFactors = FALSE
)
current_expected$Absolute_difference <- abs(current_expected$Observed - current_expected$Expected)
current_expected$Within_tolerance <- current_expected$Absolute_difference <= current_expected$Tolerance
utils::write.csv(current_expected, file.path(DIR_AUDIT, "current_manuscript_number_audit.csv"), row.names = FALSE)
if (any(!current_expected$Within_tolerance, na.rm = TRUE)) {
  warning("At least one refactored primary result differs materially from the current manuscript lock. Review 05_audit/current_manuscript_number_audit.csv before updating the paper.", call. = FALSE)
}

master_file <- file.path(DIR_ANALYSIS, "CKM_refactored_master_results.rds")
saveRDS(results, master_file, compress = "xz")
writeLines(c(
  paste0("Master results: ", normalizePath(master_file, winslash = "/", mustWork = FALSE)),
  paste0("Created: ", Sys.time()), "", capture.output(sessionInfo())
), file.path(DIR_AUDIT, "analysis_sessionInfo.txt"))

message("Refactored statistical analysis complete.\nLocked master object:\n", master_file,
        "\n\nNext run: 02_CKM_tables_figures_highimpact.R")
