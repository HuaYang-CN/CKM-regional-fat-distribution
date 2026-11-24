# Read-only descriptive comparison; no model fitting or outcome redefinition.
root <- normalizePath(Sys.getenv("CKM_CODE_ROOT", unset = getwd()), winslash = "/", mustWork = TRUE)
data_dir <- file.path(root, "data", "public")
out_dir <- file.path(Sys.getenv("CKM_OUTPUT_ROOT", unset = file.path(root, "results")),
                     "04_publication_tables", "Supplementary_Tables_v4", "individual_csv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
x <- readRDS(Sys.getenv("CKM_NHANES_STAGED_FILE",
             unset = file.path(data_dir, "NHANES_2011_2018_age30_59_CKM_staged_REVISED.rds")))
y <- readRDS(Sys.getenv("CKM_NHANES_READY_FILE",
             unset = file.path(data_dir, "NHANES_2011_2018_external_validation_ready_REVISED.rds")))
vars <- c("BMI", "VAT_area", "Trunk_pct", "LA_pct", "RA_pct", "LL_pct", "RL_pct")
stopifnot(nrow(x) == 11182L, nrow(y) == 2688L, all(vars %in% names(x)))
eligible <- x[!is.na(x$ckm_stage_primary_revised) & !is.na(x$WTSAF2YR) & x$WTSAF2YR > 0, , drop = FALSE]
inc <- eligible[complete.cases(eligible[vars]), , drop = FALSE]
exc <- eligible[!complete.cases(eligible[vars]), , drop = FALSE]
stopifnot(nrow(eligible) == 3523L, nrow(inc) == 2688L, nrow(exc) == 835L,
          identical(sort(as.numeric(inc$SEQN)), sort(as.numeric(y$SEQN))),
          !anyNA(eligible$age), !anyNA(eligible$sex))
summ <- function(d, label) {
  data.frame(Group = label, N = nrow(d), Age_mean = mean(d$age), Age_SD = sd(d$age),
             Age_min = min(d$age), Age_max = max(d$age), Male_n = sum(tolower(d$sex) == "male"),
             Male_percent = 100 * mean(tolower(d$sex) == "male"),
             Stage0_n = sum(d$ckm_stage_primary_revised == 0),
             Stage1_n = sum(d$ckm_stage_primary_revised == 1),
             Stage2_n = sum(d$ckm_stage_primary_revised == 2),
             Stage3_n = sum(d$ckm_stage_primary_revised == 3),
             Stage4_n = sum(d$ckm_stage_primary_revised == 4),
             Advanced_n = sum(d$ckm_stage_primary_revised >= 3),
             stringsAsFactors = FALSE)
}
ans <- rbind(summ(inc, "Included with complete adiposity inputs"), summ(exc, "Excluded for missing adiposity input(s)"))
write.csv(ans, file.path(out_dir, "S26_adiposity_complete_case_comparison.csv"), row.names = FALSE)
print(ans, row.names = FALSE)
