# Run from the Supplementary_Code_S1 directory.
# Rscript publication/reproduce_alcohol_sensitivity.R [output_directory]
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else file.path("results", "alcohol_sensitivity")
if (file.exists(out) && (!dir.exists(out) || length(list.files(out, all.files = TRUE, no.. = TRUE)))) stop("Use a new or empty output directory.")
CONFIG_FILE <- normalizePath(file.path("R", "00_CKM_refactored_config.R"), mustWork = TRUE)
Sys.setenv(CKM_OUTPUT_ROOT = out)
source(CONFIG_FILE, encoding = "UTF-8")
nh <- readRDS(NHANES_EXTERNAL_READY_FILE)
stopifnot(nrow(nh) == 2688L, !anyDuplicated(nh$SEQN))
loadings <- read.csv(file.path(CKM_CODE_ROOT, "publication", "fixed_pc_loadings.csv"))
stopifnot(identical(as.character(loadings$variable), as.character(NHANES_PCA_VARS)))
regional <- as.matrix(nh[NHANES_PCA_VARS])
pca <- orient_pca(prcomp(regional, center = TRUE, scale. = TRUE), "VAT_area", c("LL_pct", "RL_pct"))
fixed <- scale(regional) %*% as.matrix(loadings[c("PC1", "PC2")])
weighted_z <- function(x) {
  ms <- weighted_mean_sd(x, nh$WTSAF8YR)
  (x - ms["mean"]) / ms["sd"]
}
nh$fixed1 <- weighted_z(fixed[, 1])
nh$fixed2 <- weighted_z(fixed[, 2])
nh$local1 <- weighted_z(pca$x[, "PC1"])
nh$local2 <- weighted_z(pca$x[, "PC2"])
nh$sex_f <- factor(nh$sex)
nh$smoke_f <- factor(nh$current_smoker)
nh$stage <- nh$ckm_stage_primary_revised
nh$advanced <- as.integer(nh$advanced_ckm_primary_revised)
read_cycle <- function(suffix) {
  file <- file.path(NHANES_RAW_DIR, paste0("ALQ_", suffix, ".XPT"))
  x <- haven::read_xpt(file)
  if (suffix == "J") {
    use <- classify_alq_2017_18(x, basename(file))
  } else {
    frequency <- as.numeric(x$ALQ120Q)
    use <- dplyr::case_when(
      frequency == 0 ~ FALSE,
      frequency > 0 & !frequency %in% c(777, 999) ~ TRUE,
      TRUE ~ NA
    )
  }
  data.frame(SEQN = x$SEQN, use = use)
}
alq <- do.call(rbind, lapply(c("G","H","I","J"), read_cycle))
stopifnot(!anyDuplicated(alq$SEQN))
nh$alcohol <- as.numeric(alq$use[match(nh$SEQN, alq$SEQN)])
full <- survey::svydesign(ids = ~PSU_8YR, strata = ~STRATA_8YR, weights = ~WTSAF8YR, data = nh, nest = TRUE)
prop <- survey::svymean(~alcohol, full, na.rm = TRUE)
valid <- nh[!is.na(nh$alcohol), , drop = FALSE]
design <- survey::svydesign(ids = ~PSU_8YR, strata = ~STRATA_8YR, weights = ~WTSAF8YR, data = valid, nest = TRUE)
restricted <- subset(design, stage >= 2)
fit_one <- function(id, des, scores, add_alcohol) {
  terms <- c(scores, "sex_f", "smoke_f", if (add_alcohol) "alcohol")
  fit <- survey::svyglm(reformulate(terms, "advanced"), design = des, family = quasibinomial())
  pc2 <- scores[2]
  estimate <- extract_svy_or(fit, des, pc2)
  data.frame(ID = id, N = nrow(des$variables), advanced = sum(des$variables$advanced),
             OR = estimate$OR, lower = estimate$Lower, upper = estimate$Upper,
             t = unname(summary(fit)$coefficients[pc2, 3]), df = fit$df.residual,
             design_df = survey::degf(des), p = estimate$P)
}
fixed_scores <- c("fixed1", "fixed2")
local_scores <- c("local1", "local2")
models <- rbind(fit_one("C07", design, fixed_scores, FALSE), fit_one("C08", design, fixed_scores, TRUE),
                fit_one("C09", design, local_scores, FALSE), fit_one("C10", design, local_scores, TRUE),
                fit_one("C11", restricted, fixed_scores, TRUE))
summary_row <- data.frame(N = nrow(valid), drinkers = sum(valid$alcohol), unweighted = mean(valid$alcohol),
                          weighted = unname(coef(prop)[1]), lower = confint(prop)[1,1], upper = confint(prop)[1,2])
stopifnot(all(models$N == c(2262L,2262L,2262L,2262L,1445L)), all(models$advanced == 150L),
          summary_row$drinkers == 1853L)
write.csv(summary_row, file.path(out, "table2_alcohol.csv"), row.names = FALSE)
write.csv(models, file.path(out, "table_s11_s27_alcohol.csv"), row.names = FALSE)
by_cycle <- do.call(rbind, lapply(split(nh, nh$cycle), function(x) {
  known <- !is.na(x$alcohol)
  data.frame(cycle = x$cycle[1], N = nrow(x), known = sum(known), complete_percent = 100*mean(known),
             drinkers = sum(x$alcohol[known]), nondrinkers = sum(x$alcohol[known] == 0), missing = sum(!known),
             advanced = sum(x$advanced), advanced_with_alcohol = sum(x$advanced[known]))
}))
write.csv(by_cycle, file.path(out, "table_s12_alcohol.csv"), row.names = FALSE)
