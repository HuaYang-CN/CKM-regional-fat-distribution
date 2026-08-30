# =============================================================================
# run_all.R
#
# Usage:
#   Rscript run_all.R china
#   Rscript run_all.R nhanes
#   Rscript run_all.R all
#
# Run from the repository root.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1L) tolower(args[1]) else "all"

allowed <- c("china", "nhanes", "all")
if (!mode %in% allowed) {
  stop("Mode must be one of: china, nhanes, all", call. = FALSE)
}

source("00_config.R", encoding = "UTF-8")

rscript <- Sys.which("Rscript")
if (!nzchar(rscript)) {
  stop("Rscript executable was not found on PATH.", call. = FALSE)
}

china_scripts <- c(
  "R/01_derivation_pca_collinearity.R",
  "R/02_association_rcs.R",
  "R/03_internal_nested_cv.R",
  "R/04_internal_secondary_metrics.R",
  "R/05_internal_posthoc_SHAP.R",
  "R/06_internal_longitudinal.R"
)

nhanes_scripts <- c(
  "R/07a_nhanes_download_harmonize.R",
  "R/07b_nhanes_revised_prevent_ckm_qc.R",
  "R/08_nhanes_external_replication.R",
  "R/09_nhanes_sensitivity_final_outputs.R"
)

run_one <- function(script) {
  message("\n============================================================")
  message("RUNNING: ", script)
  message("============================================================")
  status <- system2(rscript, script)
  if (!identical(status, 0L)) {
    stop("Script failed: ", script, call. = FALSE)
  }
}

if (mode %in% c("china", "all")) {
  required_private <- c(CHINA_CROSS_FILE, CHINA_LONGITUDINAL_FILE)
  missing_private <- required_private[!file.exists(required_private)]
  if (length(missing_private) > 0L) {
    stop(
      "Private Chinese cohort data are not bundled with this repository.\n",
      "Place authorized deidentified files at:\n  ",
      paste(missing_private, collapse = "\n  "),
      "\nSee data/private/china/README.md.",
      call. = FALSE
    )
  }
  for (s in china_scripts) run_one(s)
}

if (mode %in% c("nhanes", "all")) {
  for (s in nhanes_scripts) run_one(s)
}

if (mode == "all") {
  run_one("R/10_age_sensitivity.R")
}

run_one("R/11_study_flow.R")
run_one("R/99_capture_session_info.R")

message("\nPipeline completed.")
