# Controlled-data runner for the full study analysis.
# Start a fresh R session. From the bundle root, run:
#   source("RUN_ANALYSES.R", encoding = "UTF-8")
# or use Rscript RUN_ANALYSES.R. No packages are installed by this runner.

run_ckm_publication_pipeline <- function(package_root = getwd()) {
  package_root <- normalizePath(package_root, winslash = "/", mustWork = TRUE)
  old_root <- Sys.getenv("CKM_CODE_ROOT", unset = NA_character_)
  old_options <- options(c("datadist", "survey.lonely.psu", "survey.adjust.domain.lonely",
                            "stringsAsFactors", "scipen"))
  on.exit(options(old_options), add = TRUE)
  on.exit(if (is.na(old_root)) Sys.unsetenv("CKM_CODE_ROOT") else Sys.setenv(CKM_CODE_ROOT = old_root), add = TRUE)
  Sys.setenv(CKM_CODE_ROOT = package_root)
  paths <- c(
    China_cross = Sys.getenv("CKM_CHINA_CROSS_FILE", file.path(package_root, "restricted_data", "cross_filtered.xlsx")),
    China_repeated = Sys.getenv("CKM_CHINA_LONG_FILE", file.path(package_root, "restricted_data", "suifang.xlsx")),
    NHANES_ready = Sys.getenv("CKM_NHANES_READY_FILE", file.path(package_root, "data", "public", "NHANES_2011_2018_external_validation_ready_REVISED.rds")))
  paths <- c(paths, NHANES_staged = Sys.getenv("CKM_NHANES_STAGED_FILE", file.path(package_root, "data", "public", "NHANES_2011_2018_age30_59_CKM_staged_REVISED.rds")))
  missing <- paths[!file.exists(paths)]
  if (length(missing)) stop("Required input files are missing. The two Chinese workbooks require authorized access.\n",
                            paste(names(missing), missing, sep = ": ", collapse = "\n"), call. = FALSE)
  output <- Sys.getenv("CKM_OUTPUT_ROOT", file.path(package_root, "results"))
  if (file.exists(output) && (!dir.exists(output) || length(list.files(output, all.files = TRUE, no.. = TRUE)) > 0L)) {
    stop("The target output is not empty. Set CKM_OUTPUT_ROOT to a new directory before a full rerun.", call. = FALSE)
  }
  versions <- utils::read.csv(file.path(package_root, "provenance", "analysis_R1_package_versions.csv"), stringsAsFactors = FALSE)
  required <- versions$Package
  missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages)) stop("Install the missing packages before running: ",
                                    paste(missing_packages, collapse = ", "), call. = FALSE)
  alq_dir <- Sys.getenv("CKM_NHANES_RAW_DIR", file.path(package_root, "data", "public", "raw_official_ALQ"))
  manifest <- utils::read.csv(file.path(package_root, "manifests", "official_alq_inputs.csv"), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(manifest))) {
    f <- file.path(alq_dir, paste0(manifest$dataset[i], ".XPT"))
    if (!file.exists(f) || unname(tools::md5sum(f)) != manifest$md5[i]) {
      stop("The official alcohol input is missing or has a different checksum: ", f, call. = FALSE)
    }
  }
  nhanes <- readRDS(paths[["NHANES_ready"]])
  if (!is.data.frame(nhanes) ||
      !all(c("SEQN", "advanced_ckm_primary_revised") %in% names(nhanes)) ||
      nrow(nhanes) != 2688L || anyNA(nhanes$SEQN) || anyDuplicated(nhanes$SEQN) ||
      anyNA(nhanes$advanced_ckm_primary_revised) ||
      sum(nhanes$advanced_ckm_primary_revised == 1L, na.rm = TRUE) != 179L) {
    stop("The NHANES input does not meet the locked 2688-participant / 179-advanced-case checks.", call. = FALSE)
  }
  id_file <- tempfile("ckm_sorted_ids_")
  writeLines(as.character(sort(nhanes$SEQN)), id_file, useBytes = TRUE)
  id_md5 <- unname(tools::md5sum(id_file)); unlink(id_file)
  if (!id_md5 %in% c("328b055a375910a8e8768675744d520a", "a1ec78591a130daacd3dec44319bfc38")) {
    stop("The NHANES participant-ID set differs from the corrected cohort.", call. = FALSE)
  }
  rm(nhanes)
  # The source uses the global name dd for rms::Predict. Its stale alcohol object
  # must not survive from an earlier interactive run. A fresh session is required.
  if (exists("dd_alc", envir = .GlobalEnv, inherits = FALSE)) rm("dd_alc", envir = .GlobalEnv)
  stages <- c("01_CKM_refactored_analysis_v7_methodlock.R",
              "03_CKM_supplementary_tables_S1_S26_v8_reconciled_CORRECTED.R",
              "02_CKM_tables_figures_highimpact_v19_reconciled.R")
  for (stage in stages) {
    message("Running ", stage)
    source(file.path(package_root, "R", stage), local = .GlobalEnv, encoding = "UTF-8")
  }
  source(file.path(package_root, "publication", "describe_adiposity_exclusions.R"),
         local = .GlobalEnv, encoding = "UTF-8")
  message("Study analyses and analytical exports completed: ", output)
  invisible(output)
}

if (!identical(Sys.getenv("CKM_PIPELINE_NO_RUN"), "1")) run_ckm_publication_pipeline()
