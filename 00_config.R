# =============================================================================
# 00_config.R
# Public, portable configuration for the CKM regional fat-distribution study
# =============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

# Run scripts from the repository root. To run from elsewhere, set:
# Sys.setenv(CKM_REPO_ROOT = "/path/to/repository")
repo_env <- Sys.getenv("CKM_REPO_ROOT", unset = "")
REPO_ROOT <- if (nzchar(repo_env)) {
  normalizePath(repo_env, winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

PRIVATE_CHINA_DIR <- file.path(REPO_ROOT, "data", "private", "china")
PUBLIC_NHANES_DIR <- file.path(REPO_ROOT, "data", "public", "nhanes")
RESULTS_ROOT <- file.path(REPO_ROOT, "results")

CHINA_CROSS_FILE <- file.path(PRIVATE_CHINA_DIR, "cross_filtered.xlsx")
CHINA_LONGITUDINAL_FILE <- file.path(PRIVATE_CHINA_DIR, "suifang.xlsx")

DIR_DERIVATION <- file.path(RESULTS_ROOT, "01_Derivation_PCA_Collinearity")
DIR_ASSOCIATION <- file.path(RESULTS_ROOT, "02_Association_RCS")
DIR_NESTED_CV <- file.path(RESULTS_ROOT, "03_Internal_NestedCV")
DIR_SECONDARY_METRICS <- file.path(RESULTS_ROOT, "04_Internal_SecondaryMetrics")
DIR_SHAP <- file.path(RESULTS_ROOT, "05_Internal_SHAP")
DIR_LONGITUDINAL <- file.path(RESULTS_ROOT, "06_Internal_Longitudinal")
DIR_NHANES_STAGE <- file.path(RESULTS_ROOT, "07_NHANES_Staging_QC")
DIR_NHANES_REPLICATION <- file.path(RESULTS_ROOT, "08_NHANES_External_Replication")
DIR_NHANES_SENSITIVITY <- file.path(RESULTS_ROOT, "09_NHANES_Sensitivity_Final_Outputs")
DIR_AGE_SENSITIVITY <- file.path(RESULTS_ROOT, "11_Age_Confounding_Sensitivity")
DIR_GRAPHICAL_ABSTRACT <- file.path(RESULTS_ROOT, "12_Graphical_Abstract")

DIR_SUBMISSION <- file.path(RESULTS_ROOT, "submission")
DIR_MAIN_FIG <- file.path(DIR_SUBMISSION, "main_figures")
DIR_SUPP_FIG <- file.path(DIR_SUBMISSION, "supplementary_figures")
DIR_MAIN_TAB <- file.path(DIR_SUBMISSION, "main_tables")
DIR_SUPP_TAB <- file.path(DIR_SUBMISSION, "supplementary_tables")
DIR_AUDIT <- file.path(RESULTS_ROOT, "audit")

for (d in c(
  PRIVATE_CHINA_DIR, PUBLIC_NHANES_DIR, RESULTS_ROOT,
  DIR_DERIVATION, DIR_ASSOCIATION, DIR_NESTED_CV,
  DIR_SECONDARY_METRICS, DIR_SHAP, DIR_LONGITUDINAL,
  DIR_NHANES_STAGE, DIR_NHANES_REPLICATION, DIR_NHANES_SENSITIVITY,
  DIR_AGE_SENSITIVITY, DIR_GRAPHICAL_ABSTRACT,
  DIR_SUBMISSION, DIR_MAIN_FIG, DIR_SUPP_FIG, DIR_MAIN_TAB,
  DIR_SUPP_TAB, DIR_AUDIT
)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

CHINA_PCA_DATA <- file.path(DIR_DERIVATION, "Dataset_with_PC_Scores.csv")

SEED_MAIN <- 20260825L
EXPECTED_DERIVATION_N <- 955L

FAT_VARS <- c("LL%", "LA%", "RA%", "RL%", "Trunk%", "BMI", "VFA")
BASELINE_VARS <- c(
  "Age", "Sex", "Smoke", "Alcohol_Use", "Hypertension",
  "SBP", "DBP", "TC", "TG", "HDL-C", "LDL-C"
)

SEGMENT_LABELS <- c(
  "Trunk%" = "Trunk fat, % of reference",
  "LA%" = "Left-arm fat, % of reference",
  "RA%" = "Right-arm fat, % of reference",
  "LL%" = "Left-leg fat, % of reference",
  "RL%" = "Right-leg fat, % of reference"
)

COL_FDP <- "#B2182B"
COL_VFA <- "#2166AC"
COL_MILD <- "#4C78A8"
COL_SEVERE <- "#D65F5F"
COL_GRAY <- "#6B6B6B"
COL_GRAY_DARK <- "#4A4A4A"

ensure_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0L) return(invisible(TRUE))
  if (identical(Sys.getenv("CKM_AUTO_INSTALL", unset = "0"), "1")) {
    install.packages(missing, repos = c(CRAN = "https://cloud.r-project.org"))
    missing <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  }
  if (length(missing) > 0L) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      "\nRun: Rscript R/00_install_packages.R",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

assert_file <- function(path, label = "file") {
  if (!file.exists(path)) {
    stop(label, " not found:\n", path, call. = FALSE)
  }
  invisible(TRUE)
}

assert_columns <- function(data, columns, label = "dataset") {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(label, " is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

normalize_ckm_group <- function(x) {
  chr <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(chr))
  out[chr %in% c("0", "mild", "stage 0-2", "stages 0-2")] <- "Mild"
  out[chr %in% c("1", "severe", "advanced", "stage 3-4", "stages 3-4")] <- "Severe"
  factor(out, levels = c("Mild", "Severe"))
}

format_p <- function(p, digits = 3) {
  ifelse(
    is.na(p), "NA",
    ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = digits))
  )
}

journal_theme <- function(base_size = 10) {
  ggplot2::theme_classic(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(color = "black"),
      legend.title = ggplot2::element_blank()
    )
}

save_figure <- function(plot, name, width, height, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    file.path(dir, paste0(name, ".pdf")),
    plot = plot, width = width, height = height, units = "in",
    device = grDevices::cairo_pdf, limitsize = FALSE
  )
  ggplot2::ggsave(
    file.path(dir, paste0(name, ".jpg")),
    plot = plot, width = width, height = height, units = "in",
    dpi = 600, bg = "white", limitsize = FALSE
  )
  invisible(TRUE)
}

mirror_output <- function(source_file, destination_file) {
  if (!file.exists(source_file)) return(invisible(FALSE))
  dir.create(dirname(destination_file), recursive = TRUE, showWarnings = FALSE)
  file.copy(source_file, destination_file, overwrite = TRUE)
}

write_output_manifest <- function(paths, output_file, stop_if_missing = FALSE) {
  tab <- data.frame(
    file = basename(paths),
    path = normalizePath(paths, winslash = "/", mustWork = FALSE),
    exists = file.exists(paths),
    stringsAsFactors = FALSE
  )
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(tab, output_file, row.names = FALSE)
  if (stop_if_missing && any(!tab$exists)) {
    stop("One or more expected outputs were not created. See: ", output_file, call. = FALSE)
  }
  invisible(tab)
}

write_session_audit <- function(label, notes = character()) {
  out <- file.path(DIR_AUDIT, paste0(label, "_sessionInfo.txt"))
  writeLines(c(
    paste0("Analysis: ", label),
    paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    notes,
    "",
    capture.output(sessionInfo())
  ), out, useBytes = TRUE)
  invisible(out)
}
