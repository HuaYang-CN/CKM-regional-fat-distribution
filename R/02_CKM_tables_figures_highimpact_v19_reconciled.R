# ==============================================================================
# 02_CKM_tables_figures_highimpact_v19_reconciled.R
# Publication tables + figures ONLY. Does not refit primary statistical models.
# v7 dense/classical main-figure redesign:
#   * restrained color-blind-safe clinical palette;
#   * unified forest-plot scaffold across Figures 2 and 4;
#   * correlation bubble map and coefficient-style PCA loading display;
#   * cleaner paired-change / waterfall longitudinal display;
#   * loading-agreement plot for cross-platform replication;
#   * reduced visual clutter and stronger direct effect-size emphasis.
# ==============================================================================
# Run after 01_CKM_refactored_analysis.R. You can repeatedly edit/run this file
# to optimize visual style without touching the locked analysis objects.
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

pkgs <- c("ggplot2", "dplyr", "tidyr", "tibble", "patchwork", "scales", "openxlsx")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) stop("Install required packages: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)

master_file <- file.path(DIR_ANALYSIS, "CKM_refactored_master_results.rds")
assert_file(master_file, "Refactored master-results RDS")
res <- readRDS(master_file)

# Separate main and supplementary figure folders while keeping everything under CKM第五次分析.
DIR_MAIN_FIG <- file.path(DIR_FIG, "Main_Figures_v19_Reconciled")
DIR_SUPP_FIG <- file.path(DIR_FIG, "Supplementary_Figures_v4_Optimized")
dir.create(DIR_MAIN_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_SUPP_FIG, recursive = TRUE, showWarnings = FALSE)

if (is.null(res$supplementary)) {
  stop(
    "The master RDS does not contain full supplementary figure support data.\n",
    "Please run 01_CKM_refactored_analysis_v6_fullsupp_support.R first.",
    call. = FALSE
  )
}
sup <- res$supplementary

# ==============================================================================
# 1. Publication style
# ==============================================================================

# Restrained, color-blind-safe palette chosen for clinical-journal figures.
# Blue is the dominant analytical color; red is reserved for contrast/risk;
# gray is used for secondary/sensitivity information.
J_BLUE      <- "#2F5D8A"
J_BLUE_DARK <- "#234563"
J_RED       <- "#B54A4A"
J_TEAL      <- "#4F7C78"
J_GOLD      <- "#B8862E"
J_GRAY      <- "#7A828B"
J_GRAY_DARK <- "#444B52"
J_LIGHT     <- "#D7DCE1"
J_PALE      <- "#EEF2F5"
J_INK       <- "#22272B"

pub_theme <- function(base_size = 10.6, border = FALSE) {
  th <- ggplot2::theme_classic(base_size = base_size, base_family = PLOT_FONT) +
    ggplot2::theme(
      text = ggplot2::element_text(color = J_INK),
      axis.text = ggplot2::element_text(color = J_INK, size = base_size - 0.2),
      axis.title = ggplot2::element_text(color = J_INK, face = "plain", size = base_size),
      axis.line = ggplot2::element_line(linewidth = 0.45, color = J_INK),
      axis.ticks = ggplot2::element_line(linewidth = 0.40, color = J_INK),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 0.5, color = J_INK),
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.spacing.x = grid::unit(0.08, "inches"),
      plot.title = ggplot2::element_text(
        face = "bold", size = base_size + 0.7, hjust = 0,
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size - 1.0, color = J_GRAY_DARK,
        margin = ggplot2::margin(b = 4)
      ),
      plot.tag = ggplot2::element_text(face = "bold", size = base_size + 1.8, color = J_INK),
      plot.tag.position = c(0, 1),
      plot.margin = ggplot2::margin(6, 8, 6, 8),
      panel.grid = ggplot2::element_blank()
    )
  if (border) {
    th <- th + ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color = J_INK, linewidth = 0.45)
    )
  }
  th
}

save_pub_figure <- function(plot, name, width, height, dpi = 600, out_dir = DIR_MAIN_FIG) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  pdf_dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  ggplot2::ggsave(
    file.path(out_dir, paste0(name, ".pdf")), plot, width = width, height = height,
    units = "in", device = pdf_dev, bg = "white"
  )
  ggplot2::ggsave(
    file.path(out_dir, paste0(name, ".tiff")), plot, width = width, height = height,
    units = "in", dpi = dpi, compression = "lzw", bg = "white"
  )
  ggplot2::ggsave(
    file.path(out_dir, paste0(name, "_preview.png")), plot, width = width, height = height,
    units = "in", dpi = 220, bg = "white"
  )
  invisible(TRUE)
}

# Main figures are intentionally NOT forced to a common aspect ratio.
# Each figure uses dimensions chosen for its panel geometry:
#   - heatmaps / score plots: near-square plotting regions;
#   - forest plots: wider horizontal plotting regions;
#   - mixed figures: asymmetric patchwork column widths.
# This avoids stretching panels merely to fill a predetermined canvas.

fmt_ci <- function(or, lo, hi) sprintf("%.2f (%.2f-%.2f)", or, lo, hi)
fmt_mean_sd <- function(x) sprintf("%.1f ± %.1f", mean(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE))
fmt_median_iqr <- function(x) {
  q <- stats::quantile(x, c(.25, .5, .75), na.rm = TRUE, names = FALSE)
  sprintf("%.2f (%.2f to %.2f)", q[2], q[1], q[3])
}

wrap_label <- function(x, width = 28) {
  vapply(x, function(s) paste(strwrap(as.character(s), width = width), collapse = "\n"), character(1))
}

# One reusable forest-plot scaffold is used in all main figures.
# v8 redesign: the forest plot is rendered as a three-block table layout
# (left label column, central effect-size panel, right numeric results column),
# which keeps the graph widths stable even when labels are long.
forest_panel <- function(dat, tag, xlim, breaks, axis_title,
                         label_col = "Label", color_col = "PointColor",
                         shape_col = "PointShape", text_col = "Text",
                         label_header = "Variable",
                         numeric_header = expression(beta~" (95% CI)      "*italic(P)[adj]),
                         label_width = 1.65, plot_width = 1.85, text_width = 1.35,
                         wrap_width = 28, header_size = 9.0,
                         log_scale = TRUE) {
  dd <- dat
  dd[[label_col]] <- wrap_label(dd[[label_col]], width = wrap_width)
  dd$.Row <- factor(dd[[label_col]], levels = rev(dd[[label_col]]))
  if (!color_col %in% names(dd)) dd[[color_col]] <- J_BLUE
  if (!shape_col %in% names(dd)) dd[[shape_col]] <- 15

  header_text <- paste0(tag, "   ", label_header)
  p_label <- ggplot2::ggplot(dd, ggplot2::aes(x = 0, y = .Row, label = .data[[label_col]])) +
    ggplot2::geom_text(hjust = 0, size = 3.05, family = PLOT_FONT, color = J_INK, lineheight = .95) +
    ggplot2::coord_cartesian(xlim = c(0, 1), clip = "off") +
    ggplot2::labs(title = header_text) +
    ggplot2::theme_void(base_family = PLOT_FONT) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = header_size + 0.3, hjust = 0, color = J_INK,
                                         margin = ggplot2::margin(b = 6)),
      plot.margin = ggplot2::margin(7, 0, 7, 8)
    )

  p_mid <- ggplot2::ggplot(dd, ggplot2::aes(x = OR, y = .Row)) +
    ggplot2::annotate("rect", xmin = xlim[1], xmax = 1, ymin = -Inf, ymax = Inf,
                      fill = "#EEF3F7", alpha = .85) +
    ggplot2::annotate("rect", xmin = 1, xmax = xlim[2], ymin = -Inf, ymax = Inf,
                      fill = "#F8EEEE", alpha = .85) +
    ggplot2::geom_vline(xintercept = 1, linetype = 2, color = J_GRAY_DARK, linewidth = .45) +
    ggplot2::geom_segment(
      ggplot2::aes(x = Lower, xend = Upper, yend = .Row),
      color = "#6B737B", linewidth = .78, lineend = "round"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = .data[[color_col]], shape = .data[[shape_col]]),
      size = 2.75, color = "black", stroke = .35
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_shape_identity() +
    ggplot2::coord_cartesian(xlim = xlim, clip = "off") +
    ggplot2::labs(x = axis_title, y = NULL) +
    pub_theme() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(22, 2, 7, 0)
    )
  if (isTRUE(log_scale)) {
    p_mid <- p_mid + ggplot2::scale_x_log10(breaks = breaks, labels = scales::label_number(accuracy = .01))
  } else {
    p_mid <- p_mid + ggplot2::scale_x_continuous(breaks = breaks, labels = scales::label_number(accuracy = .01),
                                                 expand = ggplot2::expansion(mult = c(0, 0)))
  }

  p_text <- ggplot2::ggplot(dd, ggplot2::aes(x = 0, y = .Row, label = .data[[text_col]])) +
    ggplot2::geom_text(hjust = 0, size = 3.00, family = PLOT_FONT, color = J_INK, lineheight = .95) +
    ggplot2::coord_cartesian(xlim = c(0, 1), clip = "off") +
    ggplot2::labs(title = numeric_header) +
    ggplot2::theme_void(base_family = PLOT_FONT) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = header_size, hjust = 0, color = J_INK,
                                         margin = ggplot2::margin(b = 5)),
      plot.margin = ggplot2::margin(7, 8, 7, 2)
    )

  p_label + p_mid + p_text + patchwork::plot_layout(widths = c(label_width, plot_width, text_width))
}

# ==============================================================================
# 2. Publication tables
# ==============================================================================

china <- res$china$raw_analysis
china$Group <- factor(as.character(china$group_factor), levels = c("Mild", "Severe"))

# ---- Table 1 ------------------------------------------------------------------
continuous_vars <- intersect(
  c("Age", "SBP", "DBP", "BMI", "TC", "TG", "HDL-C", "LDL-C", "VFA", "Trunk%", "LL%", "LA%", "RL%", "RA%"),
  names(china)
)
continuous_labels <- c(
  Age = "Age, years", SBP = "SBP, mmHg", DBP = "DBP, mmHg", BMI = "BMI, kg/m²",
  TC = "TC, mmol/L", TG = "TG, mmol/L", `HDL-C` = "HDL-C, mmol/L", `LDL-C` = "LDL-C, mmol/L",
  VFA = "VFA, cm²", `Trunk%` = "Trunk fat, % of reference", `LL%` = "LL fat, % of reference",
  `LA%` = "LA fat, % of reference", `RL%` = "RL fat, % of reference", `RA%` = "RA fat, % of reference"
)
cont_rows <- do.call(rbind, lapply(continuous_vars, function(v) {
  x <- suppressWarnings(as.numeric(china[[v]])); g <- china$Group
  p <- tryCatch(stats::t.test(x ~ g)$p.value, error = function(e) NA_real_)
  m0 <- mean(x[g == "Mild"], na.rm = TRUE); m1 <- mean(x[g == "Severe"], na.rm = TRUE)
  s0 <- stats::sd(x[g == "Mild"], na.rm = TRUE); s1 <- stats::sd(x[g == "Severe"], na.rm = TRUE)
  pooled <- sqrt((s0^2 + s1^2) / 2)
  smd <- if (is.finite(pooled) && pooled > 0) (m1 - m0) / pooled else NA_real_
  data.frame(
    Variable = unname(continuous_labels[v]),
    Overall = fmt_mean_sd(x), Mild = fmt_mean_sd(x[g == "Mild"]), Severe = fmt_mean_sd(x[g == "Severe"]),
    `P value` = format_p(p), SMD = sprintf("%.2f", smd), check.names = FALSE
  )
}))

# Categorical rows; these are expanded to levels in a journal-friendly format.
cat_specs <- list(
  Sex = list(values = normalize_sex(china$Sex), label = "Sex"),
  Smoke = list(values = normalize_binary_factor(china$Smoke, variable = "Smoke"), label = "Smoking"),
  Alcohol_Use = list(values = normalize_binary_factor(china$Alcohol_Use, variable = "Alcohol use"), label = "Alcohol use")
)
if ("Hypertension" %in% names(china)) {
  cat_specs$Hypertension <- list(values = normalize_binary_factor(china$Hypertension, variable = "Hypertension"), label = "Hypertension")
}
cat_rows <- do.call(rbind, lapply(cat_specs, function(sp) {
  f <- sp$values; tab <- table(f, china$Group); p <- tryCatch(stats::chisq.test(tab)$p.value, error = function(e) NA_real_)
  lev <- levels(f)
  rbind(
    data.frame(Variable = sp$label, Overall = "", Mild = "", Severe = "", `P value` = format_p(p), SMD = "", check.names = FALSE),
    do.call(rbind, lapply(lev, function(lv) {
      n_all <- sum(f == lv, na.rm = TRUE); n_m <- sum(f == lv & china$Group == "Mild", na.rm = TRUE); n_s <- sum(f == lv & china$Group == "Severe", na.rm = TRUE)
      data.frame(
        Variable = paste0("  ", lv),
        Overall = sprintf("%d (%.1f%%)", n_all, 100 * n_all / sum(!is.na(f))),
        Mild = sprintf("%d (%.1f%%)", n_m, 100 * n_m / sum(china$Group == "Mild" & !is.na(f))),
        Severe = sprintf("%d (%.1f%%)", n_s, 100 * n_s / sum(china$Group == "Severe" & !is.na(f))),
        `P value` = "", SMD = "", check.names = FALSE
      )
    }))
  )
}))
Table1 <- rbind(cat_rows, cont_rows)

# ---- Table 2 ------------------------------------------------------------------
lsim <- res$nhanes$loading_similarity
boot <- res$nhanes$bootstrap_phi
conc <- res$nhanes$score_concordance
ext_or <- res$nhanes$external_or
get_or_row <- function(label) ext_or[ext_or$Analysis == label, , drop = FALSE]
rows_t2 <- list(
  data.frame(Analysis = "PC2 loading congruence, China BIA vs NHANES DXA",
             Estimate = sprintf("Tucker φ = %.3f", lsim$Tucker_phi[lsim$Component == "PC2"]),
             `95% CI / robustness` = sprintf("Bootstrap interval %.3f–%.3f", boot$Lower[boot$Component == "PC2"], boot$Upper[boot$Component == "PC2"]), `P value` = "N/A", check.names = FALSE),
  data.frame(Analysis = "PC2 score concordance in NHANES",
             Estimate = sprintf("Pearson r = %.3f", conc$Pearson_r[conc$Component == "PC2"]),
             `95% CI / robustness` = "Individual-level correlation", `P value` = "N/A", check.names = FALSE)
)
for (lab in c("NHANES-derived PC1", "NHANES-derived PC2", "Fixed China-loading PC2", "Fixed China-loading PC2, Stage 2 vs 3-4")) {
  z <- get_or_row(lab)
  if (nrow(z)) rows_t2[[length(rows_t2) + 1L]] <- data.frame(
    Analysis = lab, Estimate = sprintf("OR = %.2f", z$OR), `95% CI / robustness` = sprintf("%.2f–%.2f", z$Lower, z$Upper),
    `P value` = format_p(z$P), check.names = FALSE
  )
}
age_t2 <- res$nhanes$age_sensitivity[
  res$nhanes$age_sensitivity$Analysis %in% c(
    "NHANES-derived PC2 + age",
    "Fixed China-loading PC2 + age"
  ),
  , drop = FALSE
]
for (i in seq_len(nrow(age_t2))) {
  z <- age_t2[i, , drop = FALSE]
  rows_t2[[length(rows_t2) + 1L]] <- data.frame(
    Analysis = z$Analysis, Estimate = sprintf("OR = %.2f", z$OR), `95% CI / robustness` = sprintf("%.2f–%.2f", z$Lower, z$Upper),
    `P value` = format_p(z$P), check.names = FALSE
  )
}
Table2 <- do.call(rbind, rows_t2)

# ---- Supplementary workbook ---------------------------------------------------
wb <- openxlsx::createWorkbook()
add_sheet <- function(name, x) {
  openxlsx::addWorksheet(wb, name)
  openxlsx::writeData(wb, name, x)
  openxlsx::freezePane(wb, name, firstRow = TRUE)
  openxlsx::setColWidths(wb, name, cols = seq_len(ncol(x)), widths = "auto")
}
add_sheet("Table1", Table1)
add_sheet("Table2", Table2)
add_sheet("China_loadings", data.frame(Variable = rownames(res$china$pca_lock$rotation), res$china$pca_lock$rotation, row.names = NULL))
add_sheet("China_primary_OR", res$china$primary_or)
add_sheet("RCS_P", data.frame(Component = rownames(res$china$rcs$p_values), res$china$rcs$p_values, row.names = NULL))
add_sheet("Longitudinal_tests", res$longitudinal$tests)
add_sheet("Loading_comparison", res$nhanes$loading_comparison)
add_sheet("Loading_similarity", res$nhanes$loading_similarity)
add_sheet("Loading_bootstrap", res$nhanes$bootstrap_phi)
add_sheet("External_OR", res$nhanes$external_or)
add_sheet("External_age", res$nhanes$age_sensitivity)
add_sheet("LOCO", res$nhanes$loco)
add_sheet("External_performance", res$nhanes$performance)
if (!is.null(res$ml)) {
  add_sheet("ML_algorithms", res$ml$algorithm_performance)
  add_sheet("ML_comparators", res$ml$comparator_performance)
  if (!is.null(res$ml$nri_idi)) add_sheet("NRI_IDI", res$ml$nri_idi)
}
if (!is.null(res$shap)) add_sheet("SHAP_importance", res$shap$importance)
openxlsx::saveWorkbook(wb, file.path(DIR_TABLE, "CKM_publication_tables.xlsx"), overwrite = TRUE)
utils::write.csv(Table1, file.path(DIR_TABLE, "Table_1_China_derivation.csv"), row.names = FALSE, na = "")
utils::write.csv(Table2, file.path(DIR_TABLE, "Table_2_NHANES_replication.csv"), row.names = FALSE, na = "")

# Optional Word output if flextable/officer are installed.
if (requireNamespace("flextable", quietly = TRUE) && requireNamespace("officer", quietly = TRUE)) {
  d <- officer::read_docx()
  make_ft <- function(x) {
    flextable::flextable(x) |>
      flextable::theme_booktabs() |>
      flextable::font(fontname = PLOT_FONT, part = "all") |>
      flextable::fontsize(size = 9, part = "all") |>
      flextable::autofit()
  }
  d <- officer::body_add_par(d, "Table 1. Characteristics of the Chinese derivation cohort", style = "heading 1")
  d <- flextable::body_add_flextable(d, make_ft(Table1))
  d <- officer::body_add_par(d, "Table 2. Cross-platform replication and sensitivity analyses in NHANES", style = "heading 1")
  d <- flextable::body_add_flextable(d, make_ft(Table2))
  print(d, target = file.path(DIR_TABLE, "CKM_main_tables.docx"))
}

# ==============================================================================
# 3. Figure 1: derivation of adiposity phenotypes
# ==============================================================================
# v7 adjustments requested by user:
#   A) keep a more information-dense heatmap presentation;
#   B) retain the log-scale VIF bar chart;
#   C) keep a fuller point-cloud display for the PC score space;
#   D) restore the loading heatmap form.

corr <- res$china$correlation
corr_long <- as.data.frame(as.table(corr), stringsAsFactors = FALSE)
names(corr_long) <- c("Row", "Col", "r")
ord <- CHINA_FAT_VARS
corr_long$Row <- factor(corr_long$Row, levels = rev(ord))
corr_long$Col <- factor(corr_long$Col, levels = ord)
ri <- match(as.character(corr_long$Row), ord)
ci <- match(as.character(corr_long$Col), ord)
corr_lower <- corr_long[ci <= ri, , drop = FALSE]
corr_lower$TextColor <- ifelse(corr_lower$r >= .88, "white", J_INK)

p1A <- ggplot2::ggplot(
  corr_lower, ggplot2::aes(Col, Row, fill = r)
) +
  ggplot2::geom_tile(color = "white", linewidth = .55) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", r), color = TextColor),
    size = 2.65, family = PLOT_FONT
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_fill_gradient(
    low = "#F3F5F7", high = J_RED,
    limits = c(min(corr_lower$r, na.rm = TRUE), 1)
  ) +
  ggplot2::coord_fixed() +
  ggplot2::labs(tag = "A", x = NULL, y = NULL) +
  pub_theme() +
  ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
    legend.position = "none"
  )

vif <- res$china$vif
vif$Variable <- factor(vif$Variable, levels = vif$Variable[order(vif$VIF)])
vif$Band <- cut(
  vif$VIF, breaks = c(-Inf, 5, 10, Inf),
  labels = c("<5", "5-10", ">10")
)
vif$ValueLabel <- ifelse(vif$VIF < 100, sprintf("%.1f", vif$VIF), sprintf("%.0f", vif$VIF))

p1B <- ggplot2::ggplot(vif, ggplot2::aes(x = Variable, y = VIF, fill = Band)) +
  ggplot2::geom_col(width = .64, color = "white", linewidth = .22) +
  ggplot2::geom_hline(yintercept = 5, linetype = 3, color = J_GRAY, linewidth = .45) +
  ggplot2::geom_hline(yintercept = 10, linetype = 2, color = J_GRAY, linewidth = .45) +
  ggplot2::geom_text(
    ggplot2::aes(label = ValueLabel), hjust = -.10,
    size = 2.8, family = PLOT_FONT, color = J_INK
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::scale_y_log10(
    breaks = c(1, 5, 10, 100, 1000),
    labels = c("1", "5", "10", "100", "1000"),
    expand = ggplot2::expansion(mult = c(.01, .16))
  ) +
  ggplot2::scale_fill_manual(
    values = c("<5" = "#8EA6BD", "5-10" = J_GOLD, ">10" = J_RED)
  ) +
  ggplot2::labs(tag = "B", x = NULL, y = "Variance inflation factor (log scale)") +
  pub_theme() +
  ggplot2::theme(legend.position = "none")

score_dat <- res$china$raw_analysis
score_cols <- c("Mild" = J_BLUE, "Severe" = J_RED)

p1C <- ggplot2::ggplot(
  score_dat, ggplot2::aes(PC1_Score, PC2_Score, color = group_factor)
) +
  ggplot2::geom_point(size = 1.35, alpha = .38) +
  ggplot2::stat_ellipse(
    ggplot2::aes(group = group_factor, color = group_factor),
    type = "norm", level = .80, linewidth = .95, alpha = .95
  ) +
  ggplot2::scale_color_manual(
    values = score_cols,
    labels = c("Mild CKM", "Severe CKM")
  ) +
  ggplot2::labs(tag = "C", x = "PC1 score", y = "PC2 score") +
  pub_theme() +
  ggplot2::theme(
    legend.position = "top",
    legend.justification = "left"
  )

load <- data.frame(
  Variable = rownames(res$china$pca_lock$rotation),
  res$china$pca_lock$rotation,
  row.names = NULL
)
load_hm <- tidyr::pivot_longer(
  load, c("PC1", "PC2"), names_to = "Component", values_to = "Loading"
)
pretty_china_var <- c(
  "LL%" = "LL%", "LA%" = "LA%", "RA%" = "RA%", "RL%" = "RL%",
  "Trunk%" = "Trunk%", "BMI" = "BMI", "VFA" = "VFA"
)
load_hm$VariableLabel <- unname(pretty_china_var[load_hm$Variable])
load_hm$VariableLabel <- factor(load_hm$VariableLabel, levels = rev(c("LL%", "LA%", "RA%", "RL%", "Trunk%", "BMI", "VFA")))
load_hm$TextColor <- ifelse(abs(load_hm$Loading) >= .35, "white", J_INK)

p1D <- ggplot2::ggplot(
  load_hm, ggplot2::aes(Component, VariableLabel, fill = Loading)
) +
  ggplot2::geom_tile(color = "white", linewidth = .55) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.3f", Loading), color = TextColor),
    size = 3.15, family = PLOT_FONT
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_fill_gradient2(
    low = J_BLUE_DARK, mid = "#F4F4F4", high = J_RED, midpoint = 0,
    limits = c(-.80, .45)
  ) +
  ggplot2::labs(tag = "D", x = NULL, y = NULL, fill = NULL) +
  pub_theme() +
  ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    legend.position = "right"
  )

# Figure 1 contains two matrix-style panels (A, D) and two wider panels
# (B, C). Give the right column slightly more width, while preserving the
# fixed cell geometry of the heatmaps.
Figure1 <- (p1A | p1B) / (p1C | p1D) +
  patchwork::plot_layout(
    widths = c(1.06, 0.94),
    heights = c(1.00, 1.02)
  )
save_pub_figure(
  Figure1, "Figure_1_Phenotype_derivation", 11.4, 8.3
)

# ==============================================================================
# 4. Figure 2: association + restricted cubic splines
# ==============================================================================
# Forest panel and spline panels now use a unified clinical-journal visual system.

# Recompute the Figure 2 Wald ORs directly from the locked model objects rather
# than relying on a cached summary table. This guarantees exact numerical
# concordance with Supplementary Table S4.
extract_wald_or <- function(fit, term) {
  sm <- summary(fit)$coefficients
  b <- sm[term, 1]
  se <- sm[term, 2]
  data.frame(
    term = term,
    OR = exp(b),
    Lower = exp(b - 1.96 * se),
    Upper = exp(b + 1.96 * se),
    P = sm[term, ncol(sm)],
    stringsAsFactors = FALSE
  )
}
fd <- rbind(
  transform(extract_wald_or(res$china$primary_models$pc1_crude, "PC1_Score"), Exposure = "PC1", Model = "Crude"),
  transform(extract_wald_or(res$china$primary_models$adjusted, "PC1_Score"), Exposure = "PC1", Model = "Adjusted"),
  transform(extract_wald_or(res$china$primary_models$pc2_crude, "PC2_Score"), Exposure = "PC2", Model = "Crude"),
  transform(extract_wald_or(res$china$primary_models$adjusted, "PC2_Score"), Exposure = "PC2", Model = "Adjusted")
)
fd$Label <- c("PC1, crude", "PC1, adjusted", "PC2, crude", "PC2, adjusted")
fd$PointColor <- ifelse(fd$Model == "Adjusted", J_BLUE_DARK, J_GRAY)
fd$PointShape <- ifelse(fd$Model == "Adjusted", 15, 16)
fd$Text <- paste0(
  fmt_ci(fd$OR, fd$Lower, fd$Upper), "   ",
  vapply(fd$P, format_p, character(1))
)

p2A <- forest_panel(
  fd, tag = "A", xlim = c(.38, 1.28),
  breaks = c(.4, .6, .8, 1.0, 1.2),
  axis_title = "Odds ratio per 1-unit increase",
  label_header = "Variable",
  numeric_header = "OR (95% CI)      P",
  label_width = 1.05, plot_width = 2.25, text_width = 1.00,
  wrap_width = 18,
  log_scale = FALSE
)

pvals <- res$china$rcs$p_values
rug_dat <- res$china$raw_analysis

r1 <- res$china$rcs$pc1_curve
p2B <- ggplot2::ggplot(r1, ggplot2::aes(PC1_Score, yhat)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower, ymax = upper),
    fill = J_RED, alpha = .12, color = NA
  ) +
  ggplot2::geom_line(color = J_RED, linewidth = 1.0) +
  ggplot2::geom_hline(yintercept = 1, linetype = 2, color = J_GRAY, linewidth = .42) +
  ggplot2::geom_vline(
    xintercept = stats::median(rug_dat$PC1_Score, na.rm = TRUE),
    linetype = 3, color = J_LIGHT, linewidth = .40
  ) +
  ggplot2::geom_rug(
    data = rug_dat, ggplot2::aes(x = PC1_Score), inherit.aes = FALSE,
    sides = "b", alpha = .08, color = J_GRAY_DARK,
    length = grid::unit(.025, "npc")
  ) +
  ggplot2::scale_y_log10(
    breaks = c(.5, 1, 2, 4),
    labels = scales::label_number(accuracy = .1)
  ) +
  ggplot2::labs(
    tag = "B", x = "PC1 score", y = "Adjusted odds ratio",
    subtitle = paste0(
      "Overall P ", format_p(pvals["PC1", "overall"]),
      "; nonlinear P = ", sprintf("%.3f", pvals["PC1", "nonlinear"])
    )
  ) +
  ggplot2::coord_cartesian(xlim = range(r1$PC1_Score, na.rm = TRUE)) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(.01, .01))) +
  pub_theme() +
  ggplot2::theme(aspect.ratio = .92)

r2 <- res$china$rcs$pc2_curve
p2C <- ggplot2::ggplot(r2, ggplot2::aes(PC2_Score, yhat)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower, ymax = upper),
    fill = J_BLUE, alpha = .13, color = NA
  ) +
  ggplot2::geom_line(color = J_BLUE_DARK, linewidth = 1.0) +
  ggplot2::geom_hline(yintercept = 1, linetype = 2, color = J_GRAY, linewidth = .42) +
  ggplot2::geom_vline(
    xintercept = stats::median(rug_dat$PC2_Score, na.rm = TRUE),
    linetype = 3, color = J_LIGHT, linewidth = .40
  ) +
  ggplot2::geom_rug(
    data = rug_dat, ggplot2::aes(x = PC2_Score), inherit.aes = FALSE,
    sides = "b", alpha = .08, color = J_GRAY_DARK,
    length = grid::unit(.025, "npc")
  ) +
  ggplot2::scale_y_log10(
    breaks = c(.5, 1, 2, 4),
    labels = scales::label_number(accuracy = .1)
  ) +
  ggplot2::labs(
    tag = "C", x = "PC2 score", y = "Adjusted odds ratio",
    subtitle = paste0(
      "Overall P = ", sprintf("%.3f", pvals["PC2", "overall"]),
      "; nonlinear P = ", sprintf("%.3f", pvals["PC2", "nonlinear"])
    )
  ) +
  ggplot2::coord_cartesian(xlim = range(r2$PC2_Score, na.rm = TRUE)) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(.01, .01))) +
  pub_theme() +
  ggplot2::theme(aspect.ratio = .92)

# Figure 2 is intentionally compact in width; the bottom spline panels are
# given most of the canvas and are kept close to square.
Figure2 <- p2A / (p2B | p2C) +
  patchwork::plot_layout(heights = c(.55, 1.25))
save_pub_figure(
  Figure2, "Figure_2_Association_RCS", 10.8, 8.2
)

# ==============================================================================
# 5. Figure 3: repeated-measures changes
# ==============================================================================
# v7: restore the fuller classical longitudinal display. Panels A/B return to
# boxplot-style summaries overlaid with individual paired trajectories.

prog <- res$longitudinal$progressors
prog$Direction <- ifelse(
  prog$Delta_PC2 < 0, "PC2 decreased", "PC2 increased or unchanged"
)
direction_cols <- c(
  "PC2 decreased" = J_RED,
  "PC2 increased or unchanged" = J_GRAY
)

paired_box_panel <- function(base, follow, pval, tag, ylab, ylim = NULL) {
  dd <- rbind(
    data.frame(ID = prog$ID, Time = "Baseline", Score = base),
    data.frame(ID = prog$ID, Time = "Follow-up", Score = follow)
  )
  dd$Time <- factor(dd$Time, levels = c("Baseline", "Follow-up"))
  y_raw <- range(dd$Score, na.rm = TRUE)
  if (!is.null(ylim)) y_raw <- c(min(ylim[1], y_raw[1]), max(ylim[2], y_raw[2]))
  yr <- diff(y_raw)
  if (!is.finite(yr) || yr <= 0) yr <- 1
  bracket_y <- y_raw[2] + 0.06 * yr
  tick_y <- bracket_y - 0.03 * yr
  text_y <- bracket_y + 0.05 * yr
  ylim_use <- c(y_raw[1], text_y + 0.03 * yr)

  gp <- ggplot2::ggplot(dd, ggplot2::aes(Time, Score, group = ID)) +
    ggplot2::geom_line(color = "#C8CDD2", linewidth = .38, alpha = .75) +
    ggplot2::geom_boxplot(
      data = dd,
      ggplot2::aes(x = Time, y = Score, group = Time, fill = Time),
      inherit.aes = FALSE,
      width = .34, outlier.shape = NA,
      alpha = .20, color = J_INK, linewidth = .60
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = Time), shape = 21, size = 1.9,
      color = "white", stroke = .28, alpha = .92,
      position = ggplot2::position_jitter(width = .04, height = 0)
    ) +
    ggplot2::scale_fill_manual(values = c("Baseline" = J_BLUE, "Follow-up" = J_RED), guide = "none") +
    ggplot2::annotate("segment", x = 1, xend = 2, y = bracket_y, yend = bracket_y, linewidth = .55, color = J_INK) +
    ggplot2::annotate("segment", x = 1, xend = 1, y = tick_y, yend = bracket_y, linewidth = .55, color = J_INK) +
    ggplot2::annotate("segment", x = 2, xend = 2, y = tick_y, yend = bracket_y, linewidth = .55, color = J_INK) +
    ggplot2::annotate("text", x = 1.5, y = text_y, label = paste0("Wilcoxon P = ", format_p(pval)),
                      family = PLOT_FONT, size = 3.0, color = J_INK) +
    ggplot2::labs(tag = tag, x = NULL, y = ylab) +
    ggplot2::coord_cartesian(ylim = ylim_use, clip = "off") +
    pub_theme() +
    ggplot2::theme(legend.position = "none")
  gp
}

p3A <- paired_box_panel(
  prog$PC1_Score_0, prog$PC1_Score_1,
  res$longitudinal$tests$P[res$longitudinal$tests$Outcome == "PC1 within progressors"],
  "A", "PC1 score"
)

p3B <- paired_box_panel(
  prog$PC2_Score_0, prog$PC2_Score_1,
  res$longitudinal$tests$P[res$longitudinal$tests$Outcome == "PC2 within progressors"],
  "B", "PC2 score"
)

arrows <- data.frame(
  x = prog$PC1_Score_0, y = prog$PC2_Score_0,
  xend = prog$PC1_Score_1, yend = prog$PC2_Score_1,
  Direction = prog$Direction
)
pts0 <- data.frame(PC1 = prog$PC1_Score_0, PC2 = prog$PC2_Score_0, Time = "Baseline")
pts1 <- data.frame(PC1 = prog$PC1_Score_1, PC2 = prog$PC2_Score_1, Time = "Follow-up")

p3C <- ggplot2::ggplot() +
  ggplot2::geom_segment(
    data = arrows,
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = Direction),
    linewidth = .60, alpha = .85,
    arrow = grid::arrow(length = grid::unit(.07, "inches"))
  ) +
  ggplot2::geom_point(
    data = pts0, ggplot2::aes(PC1, PC2),
    shape = 21, size = 1.55, fill = "white", color = J_GRAY_DARK, stroke = .35
  ) +
  ggplot2::geom_point(
    data = pts1, ggplot2::aes(PC1, PC2),
    shape = 16, size = 1.55, color = J_RED, alpha = .88
  ) +
  ggplot2::scale_color_manual(values = direction_cols) +
  ggplot2::labs(tag = "C", x = "PC1 score", y = "PC2 score") +
  pub_theme() +
  ggplot2::theme(
    legend.position = c(.72, .16),
    legend.justification = c(0, 0),
    legend.background = ggplot2::element_rect(fill = scales::alpha("white", .78), color = NA),
    legend.key.height = grid::unit(.30, "cm"),
    legend.key.width = grid::unit(.48, "cm"),
    legend.title = ggplot2::element_blank(),
    legend.margin = ggplot2::margin(2, 2, 2, 2)
  )

p3D <- ggplot2::ggplot(
  prog, ggplot2::aes(Delta_PC1, Delta_PC2, color = Direction)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = 2, color = J_GRAY, linewidth = .42) +
  ggplot2::geom_vline(xintercept = 0, linetype = 2, color = J_GRAY, linewidth = .42) +
  ggplot2::geom_point(size = 2.15, alpha = .88) +
  ggplot2::scale_color_manual(values = direction_cols) +
  ggplot2::annotate(
    "label", x = -Inf, y = Inf, hjust = -0.04, vjust = 1.14,
    label = sprintf("%.0f%% showed lower PC2 at follow-up", res$longitudinal$pc2_decrease_percent),
    size = 2.8, family = PLOT_FONT, color = J_INK, fill = "white",
    label.size = 0
  ) +
  ggplot2::labs(
    tag = "D", x = "ΔPC1 (follow-up - baseline)",
    y = "ΔPC2 (follow-up - baseline)"
  ) +
  pub_theme() +
  ggplot2::theme(
    legend.position = c(.72, .14),
    legend.justification = c(0, 0),
    legend.background = ggplot2::element_rect(fill = scales::alpha("white", .78), color = NA),
    legend.key.height = grid::unit(.30, "cm"),
    legend.key.width = grid::unit(.48, "cm"),
    legend.title = ggplot2::element_blank(),
    legend.margin = ggplot2::margin(2, 2, 2, 2)
  )

# Figure 3 is a true 2 x 2 visual grid, so it benefits from a somewhat
# taller canvas than the previous version. This keeps paired boxplots and
# trajectory/change panels from looking vertically squashed.
p3C <- p3C + ggplot2::theme(legend.position = "none")
p3D <- p3D

Figure3 <- ((p3A | p3B) / (p3C | p3D)) +
  patchwork::plot_layout(
    widths = c(1, 1),
    heights = c(.98, .98)
  )

save_pub_figure(
  Figure3, "Figure_3_Repeated_measures", 11.2, 8.2
)

# ==============================================================================
# 6. Figure 4: cross-platform replication / transportability
# ==============================================================================
# v7: move back toward a denser, publication-style layout. (A) restores
# the loading heatmap; B/C keep unified forest plots; D keeps score concordance.

lc <- res$nhanes$loading_comparison
phi1 <- res$nhanes$loading_similarity$Tucker_phi[
  res$nhanes$loading_similarity$Component == "PC1"
]
phi2 <- res$nhanes$loading_similarity$Tucker_phi[
  res$nhanes$loading_similarity$Component == "PC2"
]

load4 <- data.frame(
  Variable = lc$Variable,
  China_PC1 = lc$China_PC1, NHANES_PC1 = lc$NHANES_PC1,
  China_PC2 = lc$China_PC2, NHANES_PC2 = lc$NHANES_PC2,
  check.names = FALSE
)
load4_long <- tidyr::pivot_longer(
  load4, -Variable, names_to = "Metric", values_to = "Loading"
)
metric_levels <- c("China_PC1", "NHANES_PC1", "China_PC2", "NHANES_PC2")
metric_labels <- c("China\nPC1", "NHANES\nPC1", "China\nPC2", "NHANES\nPC2")
load4_long$Metric <- factor(load4_long$Metric, levels = metric_levels, labels = metric_labels)
load4_long$Variable <- factor(
  load4_long$Variable,
  levels = rev(c("BMI", "VAT", "Trunk", "Left arm", "Right arm", "Left leg", "Right leg"))
)
load4_long$TextColor <- ifelse(abs(load4_long$Loading) >= .35, "white", J_INK)

p4A <- ggplot2::ggplot(load4_long, ggplot2::aes(Metric, Variable, fill = Loading)) +
  ggplot2::geom_tile(color = "white", linewidth = .55) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.3f", Loading), color = TextColor),
    size = 3.15, family = PLOT_FONT
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_fill_gradient2(
    low = J_BLUE_DARK, mid = "#F4F4F4", high = J_RED, midpoint = 0,
    limits = c(-.80, .45)
  ) +
  ggplot2::labs(
    tag = "A", x = NULL, y = NULL, fill = NULL,
    subtitle = sprintf("Tucker phi, PC1 = %.3f; PC2 = %.3f", phi1, phi2)
  ) +
  pub_theme() +
  ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    legend.position = "right"
  )

forest_labels <- c(
  "NHANES-derived PC2",
  "Fixed China-loading PC2",
  "Fixed China-loading PC2, Stage 2 vs 3-4"
)
f4 <- res$nhanes$external_or[
  match(forest_labels, res$nhanes$external_or$Analysis),
  , drop = FALSE
]

if (!is.null(res$nhanes$alcohol_sensitivity)) {
  a <- res$nhanes$alcohol_sensitivity$alcohol_adjusted
  a$Analysis <- "Fixed China-loading PC2 + alcohol"
  f4 <- rbind(
    f4[1:2, , drop = FALSE],
    a,
    f4[3, , drop = FALSE]
  )
}

label_map4 <- c(
  "NHANES-derived PC2" = "NHANES-derived PC2",
  "Fixed China-loading PC2" = "Fixed China-loading PC2",
  "Fixed China-loading PC2 + alcohol" = "Fixed China-loading PC2 + alcohol",
  "Fixed China-loading PC2, Stage 2 vs 3-4" = "Fixed China-loading,\nStage 2 vs 3-4"
)
f4$Label <- unname(label_map4[f4$Analysis])
f4$Label[is.na(f4$Label)] <- f4$Analysis[is.na(f4$Label)]
f4$IsPrimary <- f4$Analysis %in% c("NHANES-derived PC2", "Fixed China-loading PC2")
f4$PointColor <- ifelse(f4$IsPrimary, J_BLUE_DARK, J_GRAY_DARK)
f4$PointShape <- ifelse(f4$IsPrimary, 16, 21)
f4$Text <- paste0(
  fmt_ci(f4$OR, f4$Lower, f4$Upper), "   ",
  vapply(f4$P, format_p, character(1))
)

p4B <- forest_panel(
  f4, tag = "B", xlim = c(.30, 1.02),
  breaks = c(.3, .5, .7, .9, 1.0),
  axis_title = "Odds ratio per 1 weighted SD",
  label_header = "Analysis",
  numeric_header = "OR (95% CI)      P",
  label_width = 1.28, plot_width = 2.26, text_width = 1.04,
  wrap_width = 18,
  log_scale = FALSE
)

loco <- res$nhanes$loco
loco$Label <- paste("Exclude", loco$Excluded_cycle)
loco$PointColor <- J_BLUE_DARK
loco$PointShape <- 16
loco$Text <- paste0(
  sprintf("%.2f (%.2f-%.2f)", loco$OR, loco$Lower, loco$Upper),
  "   ", vapply(loco$P, format_p, character(1))
)

p4C <- forest_panel(
  loco, tag = "C", xlim = c(.30, 1.02),
  breaks = c(.3, .5, .7, .9, 1.0),
  axis_title = "Odds ratio per 1 weighted SD",
  label_header = "Leave-one-cycle-out",
  numeric_header = "OR (95% CI)      P",
  label_width = 1.28, plot_width = 2.26, text_width = 1.04,
  wrap_width = 18,
  log_scale = FALSE
)

nh <- res$nhanes$data
p4D <- ggplot2::ggplot(
  nh, ggplot2::aes(PC2_z, China_loading_PC2_z)
) +
  ggplot2::geom_abline(
    intercept = 0, slope = 1, linetype = 2,
    color = J_GRAY, linewidth = .48
  ) +
  ggplot2::geom_point(
    color = J_BLUE, size = .85, alpha = .18
  ) +
  ggplot2::stat_density_2d(
    color = J_GRAY_DARK, bins = 6, linewidth = .35, alpha = .50
  ) +
  ggplot2::geom_smooth(
    method = "lm", se = FALSE,
    color = J_BLUE_DARK, linewidth = .90
  ) +
  ggplot2::coord_equal() +
  ggplot2::annotate(
    "label", x = -Inf, y = Inf, hjust = -0.04, vjust = 1.14,
    label = sprintf(
      "Pearson r = %.3f",
      res$nhanes$score_concordance$Pearson_r[
        res$nhanes$score_concordance$Component == "PC2"
      ]
    ),
    size = 2.8, family = PLOT_FONT, fill = "white",
    color = J_INK, label.size = 0
  ) +
  ggplot2::labs(
    tag = "D",
    x = "NHANES-derived PC2, weighted SD units",
    y = "Fixed China-loading PC2, weighted SD units"
  ) +
  pub_theme()

# Figure 4 uses equal outer columns and identical B/C forest internals so the
# two forest plotting regions have the same absolute width. Long labels wrap
# within the dedicated label column rather than squeezing the forest graph.
Figure4 <- ((p4A | p4B) / (p4C | p4D)) +
  patchwork::plot_layout(
    widths = c(1.00, 1.00),
    heights = c(1.00, 1.00)
  )

save_pub_figure(
  Figure4, "Figure_4_Cross_platform_replication", 12.0, 8.4
)

# ==============================================================================
# 7. Complete supplementary figures S1-S12
# ==============================================================================

model_cols <- c(
  "Baseline" = COL_GRAY,
  "BMI" = COL_GOLD,
  "VFA" = COL_BLUE,
  "VAT" = COL_BLUE,
  "FDP" = COL_RED
)
model_labs <- c("Baseline" = "Baseline", "BMI" = "+BMI", "VFA" = "+VFA", "VAT" = "+VAT", "FDP" = "+FDP")

pretty_feature <- function(x) {
  map <- c(
    "PC1_Score" = "PC1 score",
    "PC2_Score" = "PC2 score",
    "Sex_fMale" = "Sex: Male",
    "Smoke_fYes" = "Smoking: Yes",
    "Alcohol_fYes" = "Alcohol use: Yes"
  )
  out <- unname(map[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

# ---- Supplementary Figure S1: study flow --------------------------------------
flow_nodes <- data.frame(
  x = c(5, 5, 5, 2.7, 7.3),
  y = c(9.2, 7.2, 5.2, 2.7, 2.7),
  w = c(5.4, 6.5, 5.4, 4.2, 4.2),
  h = c(1.05, 1.35, 1.05, 1.15, 1.15),
  label = c(
    "Adults screened for multifrequency BIA\nn = 1676",
    "Excluded\nClinical criteria: n = 475\nInsufficient CKM-staging data: n = 146",
    "Eligible participants\nn = 1055",
    "Chinese derivation cohort\nn = 955",
    "Distinct repeated-measures cohort\nn = 100"
  ),
  type = c("main", "exclude", "main", "derivation", "repeat"),
  stringsAsFactors = FALSE
)
flow_nodes$xmin <- flow_nodes$x - flow_nodes$w / 2
flow_nodes$xmax <- flow_nodes$x + flow_nodes$w / 2
flow_nodes$ymin <- flow_nodes$y - flow_nodes$h / 2
flow_nodes$ymax <- flow_nodes$y + flow_nodes$h / 2
flow_cols <- c(main = "white", exclude = "#F4F4F4", derivation = "#EAF0F8", "repeat" = "#F7ECEC")
pS1 <- ggplot2::ggplot() +
  ggplot2::geom_rect(
    data = flow_nodes,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
    color = COL_BLACK, linewidth = .55
  ) +
  ggplot2::geom_text(
    data = flow_nodes,
    ggplot2::aes(x, y, label = label),
    family = PLOT_FONT, size = 3.6, lineheight = 1.05
  ) +
  ggplot2::annotate("segment", x = 5, xend = 5, y = 8.67, yend = 7.90,
                    arrow = grid::arrow(length = grid::unit(.09, "inches")), linewidth = .55) +
  ggplot2::annotate("segment", x = 5, xend = 5, y = 6.51, yend = 5.75,
                    arrow = grid::arrow(length = grid::unit(.09, "inches")), linewidth = .55) +
  ggplot2::annotate("segment", x = 5, xend = 2.7, y = 4.67, yend = 3.30,
                    arrow = grid::arrow(length = grid::unit(.09, "inches")), linewidth = .55) +
  ggplot2::annotate("segment", x = 5, xend = 7.3, y = 4.67, yend = 3.30,
                    arrow = grid::arrow(length = grid::unit(.09, "inches")), linewidth = .55) +
  ggplot2::scale_fill_manual(values = flow_cols) +
  ggplot2::coord_cartesian(xlim = c(.2, 9.8), ylim = c(1.8, 9.9), clip = "off") +
  ggplot2::theme_void(base_family = PLOT_FONT) +
  ggplot2::theme(legend.position = "none", plot.margin = ggplot2::margin(12, 16, 12, 16))
save_pub_figure(pS1, "Supplementary_Figure_S1", 8.2, 7.2, out_dir = DIR_SUPP_FIG)

# ---- Supplementary Figure S2: strict nested-CV benchmark ----------------------
if (!is.null(sup$ml)) {
  ab <- sup$ml$algorithm_benchmark
  alg_order <- ab$Model[order(ab$AUROC)]
  ab$Model <- factor(ab$Model, levels = alg_order)

  pS2A <- ggplot2::ggplot(ab, ggplot2::aes(AUROC, Model)) +
    ggplot2::geom_vline(xintercept = .5, linetype = 2, color = COL_GRAY, linewidth = .45) +
    ggplot2::geom_segment(ggplot2::aes(x = AUROC_Lower, xend = AUROC_Upper, yend = Model),
                          color = COL_BLUE, linewidth = .85) +
    ggplot2::geom_point(color = COL_RED, size = 2.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", AUROC), x = AUROC_Upper + .006),
                       hjust = 0, family = PLOT_FONT, size = 2.7) +
    ggplot2::coord_cartesian(xlim = c(.48, max(ab$AUROC_Upper, na.rm = TRUE) + .055), clip = "off") +
    ggplot2::labs(tag = "A", title = "AUROC", x = "AUROC (95% CI)", y = NULL) +
    pub_theme()

  pS2B <- ggplot2::ggplot(ab, ggplot2::aes(Model, AUPRC)) +
    ggplot2::geom_col(fill = COL_TEAL, width = .66) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", AUPRC)), hjust = -.15, size = 2.75, family = PLOT_FONT) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .14))) +
    ggplot2::labs(tag = "B", title = "AUPRC", x = NULL, y = "AUPRC") +
    pub_theme()

  pS2C <- ggplot2::ggplot(ab, ggplot2::aes(Model, Brier)) +
    ggplot2::geom_col(fill = "#718096", width = .66) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", Brier)), hjust = -.15, size = 2.75, family = PLOT_FONT) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .14))) +
    ggplot2::labs(tag = "C", title = "Brier score", x = NULL, y = "Brier score (lower is better)") +
    pub_theme()

  sf <- sup$ml$selection_frequency
  sf$Model <- factor(sf$Model, levels = alg_order)
  pS2D <- ggplot2::ggplot(sf, ggplot2::aes(Model, Selected_N_Folds)) +
    ggplot2::geom_col(fill = COL_BLUE, width = .66) +
    ggplot2::geom_text(ggplot2::aes(label = Selected_N_Folds), hjust = -.25, size = 2.9, family = PLOT_FONT) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(breaks = 0:10, limits = c(0, max(10, max(sf$Selected_N_Folds) + 1))) +
    ggplot2::labs(tag = "D", title = "Selection frequency", x = NULL, y = "Selected outer folds") +
    pub_theme()

  figS2 <- (pS2A | pS2B) / (pS2C | pS2D)
  save_pub_figure(figS2, "Supplementary_Figure_S2", 10.8, 8.0, out_dir = DIR_SUPP_FIG)

  # ---- Supplementary Figure S3: secondary internal performance ---------------
  cp <- sup$ml$comparator_performance
  cp$Model <- factor(cp$Model, levels = c("Baseline", "BMI", "VFA", "FDP"))
  diffs <- sup$ml$comparator_auc_differences
  delta_lab <- setNames(
    paste0("Δ=", sprintf("%.3f", diffs$Delta_AUROC_FDP_minus_ref), "; P=", vapply(diffs$P, format_p, character(1))),
    diffs$Reference
  )
  cp$DeltaText <- ifelse(as.character(cp$Model) == "FDP", "Reference",
                         unname(delta_lab[as.character(cp$Model)]))
  cp$Display <- factor(model_labs[as.character(cp$Model)], levels = rev(model_labs[c("Baseline","BMI","VFA","FDP")]))

  pS3A <- ggplot2::ggplot(cp, ggplot2::aes(AUROC, Display, color = Model)) +
    ggplot2::geom_segment(ggplot2::aes(x = AUROC_Lower, xend = AUROC_Upper, yend = Display), linewidth = .85) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::geom_text(ggplot2::aes(label = DeltaText, x = AUROC_Upper + .008),
                       hjust = 0, size = 2.55, color = COL_BLACK, family = PLOT_FONT) +
    ggplot2::scale_color_manual(values = model_cols) +
    ggplot2::coord_cartesian(xlim = c(min(cp$AUROC_Lower)-.02, max(cp$AUROC_Upper)+.15), clip = "off") +
    ggplot2::labs(tag = "A", title = "AUROC", x = "Pooled outer-fold AUROC (95% CI)", y = NULL) +
    pub_theme() + ggplot2::theme(legend.position = "none")

  pS3B <- ggplot2::ggplot(cp, ggplot2::aes(Display, Brier, fill = Model)) +
    ggplot2::geom_col(width = .65) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", Brier)), hjust = -.15, size = 2.75, family = PLOT_FONT) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_manual(values = model_cols) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .16))) +
    ggplot2::labs(tag = "B", title = "Brier score", x = NULL, y = "Brier score (lower is better)") +
    pub_theme() + ggplot2::theme(legend.position = "none")

  cal_all <- sup$ml$calibration_curves
  cal_fdp <- cal_all[cal_all$Model == "FDP", , drop = FALSE]
  dec <- sup$ml$fdp_deciles
  cb <- sup$ml$calibration_bootstrap
  introw <- cb[cb$Metric == "Intercept", , drop = FALSE]
  slprow <- cb[cb$Metric == "Slope", , drop = FALSE]
  cal_annot <- sprintf(
    "Intercept %.2f (%.2f to %.2f)\nSlope %.2f (%.2f to %.2f)",
    introw$Estimate, introw$Lower, introw$Upper, slprow$Estimate, slprow$Lower, slprow$Upper
  )
  pS3C <- ggplot2::ggplot(cal_fdp, ggplot2::aes(Predicted, Observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, color = COL_GRAY, linewidth = .45) +
    ggplot2::geom_line(color = COL_RED, linewidth = .95) +
    ggplot2::geom_point(data = dec, ggplot2::aes(Predicted, Observed), inherit.aes = FALSE,
                        shape = 21, fill = "white", color = COL_RED, size = 2.3, stroke = .7) +
    ggplot2::annotate("text", x = Inf, y = -Inf, label = cal_annot, hjust = 1.03, vjust = -.35,
                      family = PLOT_FONT, size = 2.55) +
    ggplot2::coord_equal(xlim = c(0, max(cal_fdp$Predicted, na.rm = TRUE)),
                         ylim = c(0, max(c(cal_fdp$Observed, cal_fdp$Predicted), na.rm = TRUE))) +
    ggplot2::labs(tag = "C", title = "Calibration", x = "Predicted probability", y = "Observed probability") +
    pub_theme() +
    ggplot2::theme(aspect.ratio = .95)

  dca <- sup$ml$dca
  yprev <- mean(as.integer(res$ml$oof_comparators$Outcome == "Severe"))
  dca_ref <- data.frame(
    threshold = sort(unique(dca$threshold)),
    Treat_all = yprev - (1 - yprev) * sort(unique(dca$threshold)) / (1 - sort(unique(dca$threshold)))
  )
  dca_xlim <- range(dca$threshold, na.rm = TRUE)
  dca_ref_plot <- dca_ref[dca_ref$threshold >= dca_xlim[1] & dca_ref$threshold <= dca_xlim[2], , drop = FALSE]
  dca_ref_plot$Treat_all <- pmax(dca_ref_plot$Treat_all, -0.12)
  pS3D <- ggplot2::ggplot(dca, ggplot2::aes(threshold, net_benefit, color = Model)) +
    ggplot2::geom_hline(yintercept = 0, color = COL_BLACK, linewidth = .45) +
    ggplot2::geom_line(data = dca_ref_plot, ggplot2::aes(threshold, Treat_all), inherit.aes = FALSE,
                       linetype = 2, color = COL_GRAY, linewidth = .7, lineend = "round") +
    ggplot2::geom_line(linewidth = .92) +
    ggplot2::scale_color_manual(values = model_cols, labels = model_labs) +
    ggplot2::coord_cartesian(xlim = dca_xlim, ylim = c(-0.12, 0.52), clip = "on") +
    ggplot2::labs(tag = "D", title = "Decision-curve analysis",
                  x = "Threshold probability", y = "Net benefit") +
    pub_theme() +
    ggplot2::theme(legend.position = "top", aspect.ratio = .95)

  figS3 <- (pS3A | pS3B) / (pS3C | pS3D) +
    patchwork::plot_layout(heights = c(.82, 1.08))
  save_pub_figure(figS3, "Supplementary_Figure_S3", 10.2, 9.2, out_dir = DIR_SUPP_FIG)

  # ---- Supplementary Figure S4: reclassification + calibration ---------------
  nr <- sup$ml$nri_bootstrap
  if (!is.null(nr)) {
    ref_levels <- c("Baseline", "BMI", "VFA")
    nr$Reference <- factor(nr$Reference, levels = ref_levels)

    make_metric_forest <- function(metric, tag, title, xlab) {
      d <- nr[nr$Metric == metric, , drop = FALSE]
      d$Label <- factor(model_labs[as.character(d$Reference)], levels = rev(model_labs[ref_levels]))
      ggplot2::ggplot(d, ggplot2::aes(Estimate, Label)) +
        ggplot2::geom_vline(xintercept = 0, linetype = 2, color = COL_GRAY, linewidth = .45) +
        ggplot2::geom_segment(ggplot2::aes(x = Lower, xend = Upper, yend = Label), color = COL_BLUE, linewidth = .85) +
        ggplot2::geom_point(color = COL_RED, size = 2.7) +
        ggplot2::labs(tag = tag, title = title, x = xlab, y = NULL) + pub_theme()
    }
    pS4A <- make_metric_forest("Continuous_NRI", "A", "Continuous NRI", "Estimate (bootstrap 95% CI)")
    pS4B <- make_metric_forest("IDI", "B", "Integrated discrimination improvement", "IDI (bootstrap 95% CI)")

    comp <- nr[nr$Metric %in% c("NRI_event", "NRI_nonevent"), , drop = FALSE]
    comp$Component <- factor(comp$Metric, levels = c("NRI_event", "NRI_nonevent"),
                             labels = c("Event NRI", "Non-event NRI"))
    comp$Label <- factor(model_labs[as.character(comp$Reference)], levels = rev(model_labs[ref_levels]))
    dodge_s4 <- ggplot2::position_dodge(width = .46)
    pS4C <- ggplot2::ggplot(
      comp,
      ggplot2::aes(Estimate, Label, color = Component, group = Component)
    ) +
      ggplot2::geom_vline(
        xintercept = 0, linetype = 2,
        color = COL_GRAY, linewidth = .45
      ) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = Lower, xmax = Upper),
        height = .11,
        linewidth = .75,
        position = dodge_s4
      ) +
      ggplot2::geom_point(
        size = 2.55,
        position = dodge_s4
      ) +
      ggplot2::scale_color_manual(
        values = c("Event NRI" = COL_RED, "Non-event NRI" = COL_BLUE)
      ) +
      ggplot2::labs(
        tag = "C",
        title = "NRI components",
        x = "Estimate (bootstrap 95% CI)",
        y = NULL
      ) +
      pub_theme() +
      ggplot2::theme(legend.position = "top")

    ca <- sup$ml$calibration_curves
    pS4D <- ggplot2::ggplot(ca, ggplot2::aes(Predicted, Observed, color = Model)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, color = COL_GRAY, linewidth = .45) +
      ggplot2::geom_line(linewidth = .9, na.rm = TRUE) +
      ggplot2::scale_color_manual(values = model_cols, labels = model_labs) +
      ggplot2::labs(tag = "D", title = "Calibration curves",
                    x = "Predicted probability", y = "Observed probability") +
      pub_theme() + ggplot2::theme(legend.position = "top")

    figS4 <- (pS4A | pS4B) / (pS4C | pS4D)
    save_pub_figure(figS4, "Supplementary_Figure_S4", 11.0, 8.0, out_dir = DIR_SUPP_FIG)
  }

  # ---- Supplementary Figure S5: incremental DCA ------------------------------
  dd <- sup$ml$dca_difference
  if (!is.null(dd)) {
    pS5 <- ggplot2::ggplot(dd, ggplot2::aes(threshold, FDP_minus_reference, color = Reference)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, color = COL_GRAY, linewidth = .45) +
      ggplot2::geom_line(linewidth = 1.0) +
      ggplot2::scale_color_manual(values = c(Baseline = COL_GRAY, BMI = COL_GOLD, VFA = COL_BLUE),
                                  labels = c(Baseline = "Baseline", BMI = "+BMI", VFA = "+VFA")) +
      ggplot2::labs(x = "Threshold probability", y = "FDP minus reference net benefit") +
      pub_theme(base_size = 10.8) + ggplot2::theme(legend.position = "top")
    save_pub_figure(pS5, "Supplementary_Figure_S5", 7.8, 5.4, out_dir = DIR_SUPP_FIG)
  }
}

# ---- Supplementary Figures S6-S7: SHAP ---------------------------------------
if (!is.null(res$shap)) {
  imp <- res$shap$importance
  imp$Display <- pretty_feature(imp$Feature)
  imp$Display <- factor(
    imp$Display,
    levels = imp$Display[order(imp$MeanAbsSHAP)]
  )

  pS6A <- ggplot2::ggplot(
    imp,
    ggplot2::aes(Display, MeanAbsSHAP)
  ) +
    ggplot2::geom_col(
      fill = COL_BLUE,
      width = .60
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", MeanAbsSHAP)),
      hjust = -.14,
      family = PLOT_FONT,
      size = 2.8
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, .18))
    ) +
    ggplot2::labs(
      tag = "A",
      title = "SHAP importance",
      x = NULL,
      y = "Mean |SHAP value|"
    ) +
    pub_theme()

  shap <- res$shap$shap
  X <- res$shap$X

  shap_long <- tidyr::pivot_longer(
    data.frame(
      row_id = seq_len(nrow(shap)),
      shap,
      check.names = FALSE
    ),
    -row_id,
    names_to = "Feature",
    values_to = "SHAP"
  )

  x_long <- tidyr::pivot_longer(
    data.frame(
      row_id = seq_len(nrow(X)),
      X,
      check.names = FALSE
    ),
    -row_id,
    names_to = "Feature",
    values_to = "FeatureValue"
  )

  shap_long <- dplyr::left_join(
    shap_long,
    x_long,
    by = c("row_id", "Feature")
  )

  # IMPORTANT visual fix:
  # raw feature scales are not comparable across PC scores and binary variables.
  # Color therefore represents the within-feature percentile (0 = low, 1 = high)
  # rather than the raw numeric value. This is the standard semantic meaning of
  # a SHAP beeswarm color legend and avoids a misleading shared raw-value scale.
  shap_long <- shap_long |>
    dplyr::group_by(Feature) |>
    dplyr::mutate(
      FeaturePercentile = dplyr::percent_rank(FeatureValue)
    ) |>
    dplyr::ungroup()

  order_features <- imp$Feature[order(imp$MeanAbsSHAP)]
  shap_long$Display <- pretty_feature(shap_long$Feature)
  shap_long$Display <- factor(
    shap_long$Display,
    levels = pretty_feature(order_features)
  )

  pS6B <- ggplot2::ggplot(
    shap_long,
    ggplot2::aes(
      SHAP,
      Display,
      color = FeaturePercentile
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      color = COL_GRAY,
      linewidth = .45
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(
        height = .15,
        width = 0
      ),
      size = .92,
      alpha = .62
    ) +
    ggplot2::scale_color_gradient2(
      low = COL_BLUE,
      mid = "#F2F2F2",
      high = COL_RED,
      midpoint = .5,
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("Low", "High")
    ) +
    ggplot2::labs(
      tag = "B",
      title = "Summary plot",
      x = "SHAP value",
      y = NULL,
      color = "Feature value"
    ) +
    pub_theme() +
    ggplot2::theme(
      legend.position = "right",
      legend.key.height = grid::unit(.75, "inches")
    )

  figS6 <- pS6A | pS6B
  save_pub_figure(
    figS6,
    "Supplementary_Figure_S6",
    11.2, 5.5,
    out_dir = DIR_SUPP_FIG
  )

  make_dep <- function(feature, tag, xlab) {
    if (!feature %in% names(X) || !feature %in% names(shap)) {
      return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = paste(feature, "not available")) +
               ggplot2::labs(tag = tag) + ggplot2::theme_void())
    }
    ddp <- data.frame(Value = X[[feature]], SHAP = shap[[feature]])
    ggplot2::ggplot(ddp, ggplot2::aes(Value, SHAP)) +
      ggplot2::geom_hline(yintercept = 0, color = COL_LIGHT_GRAY, linewidth = .45) +
      ggplot2::geom_point(color = COL_BLUE, alpha = .35, size = 1.05) +
      ggplot2::geom_smooth(method = "loess", se = TRUE, color = COL_RED, fill = "#F4CCCC",
                           alpha = .25, linewidth = .9) +
      ggplot2::labs(tag = tag, x = xlab, y = paste0(xlab, " SHAP value")) +
      pub_theme()
  }
  pS7A <- make_dep("PC1_Score", "A", "PC1 score")
  pS7B <- make_dep("PC2_Score", "B", "PC2 score")
  figS7 <- pS7A | pS7B
  save_pub_figure(figS7, "Supplementary_Figure_S7", 10.2, 4.8, out_dir = DIR_SUPP_FIG)
}

# ---- Supplementary Figure S8: dynamic-reference longitudinal comparison -------
paired <- res$longitudinal$paired
dyn <- paired[paired$Progression_Status %in% c("Progressor", "Stable Severe"), , drop = FALSE]
dyn$Group <- factor(dyn$Progression_Status, levels = c("Stable Severe", "Progressor"),
                    labels = c("Stable severe", "Mild-to-severe progressor"))
make_delta_panel <- function(var, tag, ylab, pval) {
  ggplot2::ggplot(dyn, ggplot2::aes(Group, .data[[var]], fill = Group)) +
    ggplot2::geom_boxplot(width = .48, outlier.shape = NA, linewidth = .6, alpha = .72) +
    ggplot2::geom_jitter(width = .12, size = 1.35, alpha = .55, color = COL_BLACK) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, color = COL_GRAY, linewidth = .45) +
    ggplot2::scale_fill_manual(values = c("Stable severe" = "#B9C2CC", "Mild-to-severe progressor" = "#D88A8A")) +
    ggplot2::labs(tag = tag, x = NULL, y = ylab,
                  subtitle = paste0("Wilcoxon rank-sum P = ", format_p(pval))) +
    pub_theme() + ggplot2::theme(legend.position = "none", plot.subtitle = ggplot2::element_text(size = 8.8))
}
pS8A <- make_delta_panel("Delta_PC1", "A", "ΔPC1 (follow-up − baseline)",
                          res$longitudinal$tests$P[res$longitudinal$tests$Outcome == "Delta PC1: progressor vs stable severe"])
pS8B <- make_delta_panel("Delta_PC2", "B", "ΔPC2 (follow-up − baseline)",
                          res$longitudinal$tests$P[res$longitudinal$tests$Outcome == "Delta PC2: progressor vs stable severe"])
figS8 <- pS8A | pS8B
save_pub_figure(figS8, "Supplementary_Figure_S8", 9.6, 4.8, out_dir = DIR_SUPP_FIG)

# ---- Supplementary Figure S9: cross-platform PCA robustness -------------------
bl <- sup$nhanes_bootstrap_loadings
var_map <- c(
  "BMI" = "BMI", "VAT_area" = "VAT", "Trunk_pct" = "Trunk",
  "LA_pct" = "Left arm", "RA_pct" = "Right arm",
  "LL_pct" = "Left leg", "RL_pct" = "Right leg"
)
bl$Display <- unname(var_map[bl$Variable])
bl$Display <- factor(bl$Display, levels = rev(CANONICAL_VARS))
bl$Component <- factor(bl$Component, levels = c("PC1", "PC2"))
dodge_s9 <- ggplot2::position_dodge(width = .46)
pS9A <- ggplot2::ggplot(
  bl,
  ggplot2::aes(Estimate, Display, color = Component, group = Component)
) +
  ggplot2::geom_vline(
    xintercept = 0,
    color = COL_LIGHT_GRAY,
    linewidth = .45
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = Lower, xmax = Upper),
    height = .11,
    linewidth = .75,
    position = dodge_s9
  ) +
  ggplot2::geom_point(
    size = 2.5,
    position = dodge_s9
  ) +
  ggplot2::scale_color_manual(
    values = c(PC1 = COL_BLUE, PC2 = COL_RED)
  ) +
  ggplot2::labs(
    tag = "A",
    title = "Loading stability",
    x = "Loading (bootstrap 95% interval)",
    y = NULL
  ) +
  pub_theme() +
  ggplot2::theme(legend.position = "top")

phi_raw <- as.data.frame(res$nhanes$bootstrap_phi_raw)
phi_long <- tidyr::pivot_longer(phi_raw, c("PC1", "PC2"), names_to = "Component", values_to = "Tucker_phi")
pS9B <- ggplot2::ggplot(phi_long, ggplot2::aes(Tucker_phi, fill = Component, color = Component)) +
  ggplot2::geom_density(alpha = .16, linewidth = .9) +
  ggplot2::geom_vline(xintercept = .90, linetype = 3, color = COL_GRAY, linewidth = .55) +
  ggplot2::geom_vline(xintercept = .95, linetype = 2, color = COL_GRAY, linewidth = .55) +
  ggplot2::scale_fill_manual(values = c(PC1 = COL_BLUE, PC2 = COL_RED)) +
  ggplot2::scale_color_manual(values = c(PC1 = COL_BLUE, PC2 = COL_RED)) +
  ggplot2::labs(tag = "B", title = "Tucker congruence", x = "Tucker φ", y = "Density") +
  pub_theme() + ggplot2::theme(legend.position = "top")
figS9 <- pS9A | pS9B
save_pub_figure(figS9, "Supplementary_Figure_S9", 10.6, 5.0, out_dir = DIR_SUPP_FIG)

# ---- Supplementary Figure S10: external discrimination ------------------------
# v4: bar charts were visually heavy and compressed the small model differences.
# Dot plots preserve exact values without implying a meaningful zero baseline.

eroc <- sup$external_roc
eroc$Model <- factor(
  eroc$Model,
  levels = c("Baseline", "BMI", "VAT", "FDP")
)

pS10A <- ggplot2::ggplot(
  eroc,
  ggplot2::aes(FPR, TPR, color = Model)
) +
  ggplot2::geom_abline(
    slope = 1, intercept = 0,
    linetype = 2,
    color = COL_GRAY,
    linewidth = .45
  ) +
  ggplot2::geom_line(linewidth = .95) +
  ggplot2::scale_color_manual(
    values = model_cols[c("Baseline", "BMI", "VAT", "FDP")],
    labels = model_labs[c("Baseline", "BMI", "VAT", "FDP")]
  ) +
  ggplot2::coord_equal() +
  ggplot2::labs(
    tag = "A",
    title = "ROC curves",
    x = "1 − specificity",
    y = "Sensitivity"
  ) +
  pub_theme() +
  ggplot2::theme(legend.position = "top")

ep <- res$nhanes$performance
ep$Model <- factor(ep$Model, levels = c("Baseline", "BMI", "VAT", "FDP"))
ep$Display <- factor(
  model_labs[as.character(ep$Model)],
  levels = rev(model_labs[c("Baseline", "BMI", "VAT", "FDP")])
)

# Panels B and C use compact lollipop-style displays and are stacked on the
# right to avoid the cramped appearance of a three-column arrangement.
auroc_xlim <- c(
  min(ep$Weighted_AUROC, na.rm = TRUE) - .018,
  max(ep$Weighted_AUROC, na.rm = TRUE) + .030
)
auroc_xmin <- auroc_xlim[1]
pS10B <- ggplot2::ggplot(
  ep,
  ggplot2::aes(y = Display, x = Weighted_AUROC, color = Model)
) +
  ggplot2::geom_segment(
    ggplot2::aes(x = auroc_xmin, xend = Weighted_AUROC, yend = Display),
    linewidth = 2.2, alpha = .32, lineend = "round"
  ) +
  ggplot2::geom_point(size = 3.1) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.3f", Weighted_AUROC)),
    hjust = -0.15,
    family = PLOT_FONT,
    size = 2.8,
    color = COL_BLACK
  ) +
  ggplot2::scale_color_manual(values = model_cols) +
  ggplot2::coord_cartesian(xlim = auroc_xlim, clip = "off") +
  ggplot2::labs(
    tag = "B",
    title = "Weighted AUROC",
    x = "Weighted AUROC",
    y = NULL
  ) +
  pub_theme() +
  ggplot2::theme(legend.position = "none", aspect.ratio = .78)

brier_xlim <- c(
  min(ep$Weighted_Brier, na.rm = TRUE) - .0010,
  max(ep$Weighted_Brier, na.rm = TRUE) + .0020
)
brier_xmin <- brier_xlim[1]
pS10C <- ggplot2::ggplot(
  ep,
  ggplot2::aes(y = Display, x = Weighted_Brier, color = Model)
) +
  ggplot2::geom_segment(
    ggplot2::aes(x = brier_xmin, xend = Weighted_Brier, yend = Display),
    linewidth = 2.2, alpha = .32, lineend = "round"
  ) +
  ggplot2::geom_point(size = 3.1) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.4f", Weighted_Brier)),
    hjust = -0.15,
    family = PLOT_FONT,
    size = 2.8,
    color = COL_BLACK
  ) +
  ggplot2::scale_color_manual(values = model_cols) +
  ggplot2::coord_cartesian(xlim = brier_xlim, clip = "off") +
  ggplot2::labs(
    tag = "C",
    title = "Weighted Brier score",
    x = "Weighted Brier score (lower is better)",
    y = NULL
  ) +
  pub_theme() +
  ggplot2::theme(legend.position = "none", aspect.ratio = .78)

figS10 <- pS10A | (pS10B / pS10C) +
  patchwork::plot_layout(widths = c(1.28, .92))
save_pub_figure(
  figS10,
  "Supplementary_Figure_S10",
  11.6, 6.4,
  out_dir = DIR_SUPP_FIG
)

# ---- Supplementary Figure S11: external sensitivity ---------------------------
alc <- sup$alcohol_full_or
if (!is.null(alc) && nrow(alc)) {
  alc$Short <- dplyr::recode(
    alc$Analysis,
    "Fixed China loading: same alcohol-complete sample, no alcohol adjustment" = "Fixed score; no alcohol adjustment",
    "Fixed China loading: alcohol-adjusted" = "Fixed score; alcohol-adjusted",
    "NHANES-derived PC2: same alcohol-complete sample, no alcohol adjustment" = "NHANES PC2; no alcohol adjustment",
    "NHANES-derived PC2: alcohol-adjusted" = "NHANES PC2; alcohol-adjusted",
    "Fixed China loading: alcohol-adjusted Stage 2 vs Stage 3-4" = "Fixed score; alcohol-adjusted; Stage 2–4"
  )
  alc$Short <- factor(alc$Short, levels = rev(alc$Short))
  pS11A <- ggplot2::ggplot(alc, ggplot2::aes(OR, Short)) +
    ggplot2::geom_vline(xintercept = 1, linetype = 2, color = COL_GRAY, linewidth = .45) +
    ggplot2::geom_segment(ggplot2::aes(x = Lower, xend = Upper, yend = Short), color = COL_BLUE, linewidth = .85) +
    ggplot2::geom_point(color = COL_RED, size = 2.7) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(tag = "A", title = "Sensitivity analyses",
                  x = "Odds ratio per 1 weighted SD", y = NULL) +
    pub_theme()

  sg <- sup$stage_gradient
  pS11B <- ggplot2::ggplot(sg, ggplot2::aes(Stage, Mean_PC2)) +
    ggplot2::geom_line(color = COL_BLUE, linewidth = .9) +
    ggplot2::geom_point(color = COL_RED, size = 2.7) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = Lower, ymax = Upper), width = .08,
                           color = COL_BLUE, linewidth = .65) +
    ggplot2::scale_x_continuous(breaks = 0:4) +
    ggplot2::labs(tag = "B", title = "PC2 by CKM stage", x = "CKM stage",
                  y = "Survey-weighted mean NHANES-derived PC2") +
    pub_theme()
  figS11 <- pS11A | pS11B
  save_pub_figure(figS11, "Supplementary_Figure_S11", 10.8, 5.0, out_dir = DIR_SUPP_FIG)
}

# ---- Supplementary Figure S12: age sensitivity -------------------------------
# v4 fixes:
# - use geom_errorbarh instead of dodged geom_segment, preventing diagonal CI
#   lines;
# - provide explicit right-hand space for P-value labels in panel B;
# - use compact, aligned forest-plot styling.

ac <- sup$age_comparison
ac_long <- dplyr::bind_rows(
  data.frame(
    Label = ac$Label,
    Model = "Primary model",
    OR = ac$Primary_OR,
    Lower = ac$Primary_Lower,
    Upper = ac$Primary_Upper
  ),
  data.frame(
    Label = ac$Label,
    Model = "Age-adjusted sensitivity",
    OR = ac$Age_OR,
    Lower = ac$Age_Lower,
    Upper = ac$Age_Upper
  )
)

ac_long$Label <- factor(
  ac_long$Label,
  levels = rev(
    c(
      "China derivation",
      "NHANES-derived PC2",
      "Fixed China-loading PC2"
    )
  )
)
ac_long$Model <- factor(
  ac_long$Model,
  levels = c(
    "Primary model",
    "Age-adjusted sensitivity"
  )
)

dodge_s12 <- ggplot2::position_dodge(width = .48)

pS12A <- ggplot2::ggplot(
  ac_long,
  ggplot2::aes(
    OR,
    Label,
    color = Model,
    group = Model
  )
) +
  ggplot2::geom_vline(
    xintercept = 1,
    linetype = 2,
    color = COL_GRAY,
    linewidth = .45
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = Lower, xmax = Upper),
    height = .11,
    linewidth = .80,
    position = dodge_s12
  ) +
  ggplot2::geom_point(
    size = 2.7,
    position = dodge_s12
  ) +
  ggplot2::scale_x_log10(
    breaks = c(.4, .5, .7, 1)
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Primary model" = COL_BLUE,
      "Age-adjusted sensitivity" = COL_RED
    )
  ) +
  ggplot2::labs(
    tag = "A",
    title = "PC2 associations",
    x = "Odds ratio (95% CI)",
    y = NULL
  ) +
  pub_theme() +
  ggplot2::theme(legend.position = "top")

ai <- sup$age_interactions
ai$Label <- factor(ai$Label, levels = levels(ac_long$Label))
ai$Ptxt <- vapply(ai$P, format_p, character(1))

# Reserve explicit annotation space to the right of all confidence intervals.
x_text_s12 <- max(ai$Upper, na.rm = TRUE) * 1.10
x_max_s12 <- max(ai$Upper, na.rm = TRUE) * 1.34

pS12B <- ggplot2::ggplot(ai, ggplot2::aes(OR, Label)) +
  ggplot2::geom_vline(
    xintercept = 1,
    linetype = 2,
    color = COL_GRAY,
    linewidth = .45
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = Lower, xmax = Upper),
    height = .10,
    color = COL_BLUE,
    linewidth = .80
  ) +
  ggplot2::geom_point(
    color = COL_RED,
    size = 2.75
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      x = x_text_s12,
      label = paste0("P = ", Ptxt)
    ),
    hjust = 0,
    family = PLOT_FONT,
    size = 2.75,
    color = COL_BLACK
  ) +
  ggplot2::scale_x_log10(
    breaks = c(.8, .9, 1, 1.2, 1.5)
  ) +
  ggplot2::coord_cartesian(
    xlim = c(
      min(ai$Lower, na.rm = TRUE) * .94,
      x_max_s12
    ),
    clip = "off"
  ) +
  ggplot2::labs(
    tag = "B",
    title = "PC2 × age interaction",
    x = "Interaction OR per 10-year age increment",
    y = NULL
  ) +
  pub_theme() +
  ggplot2::theme(
    plot.margin = ggplot2::margin(7, 28, 7, 9)
  )

figS12 <- pS12A | pS12B
save_pub_figure(
  figS12,
  "Supplementary_Figure_S12",
  11.6, 5.2,
  out_dir = DIR_SUPP_FIG
)


# Compile supplementary figures into a Word file with captions.
if (requireNamespace("officer", quietly = TRUE) && requireNamespace("png", quietly = TRUE)) {
  figure_legend_map <- list(
    "Supplementary_Figure_S1" = paste(
      "Study flow diagram for cohort selection and derivation of the cross-sectional and repeated-measures cohorts.",
      "The diagram summarizes screening, exclusions, the final derivation cohort, and the repeated-measures subset used for longitudinal analyses."
    ),
    "Supplementary_Figure_S2" = paste(
      "Strict nested cross-validation benchmark of candidate machine-learning algorithms in the Chinese derivation cohort.",
      "(A) shows pooled outer-fold AUROC.",
      "(B) shows pooled outer-fold AUPRC.",
      "(C) shows Brier score.",
      "(D) shows the frequency with which each algorithm was selected as the inner-CV winner across outer folds."
    ),
    "Supplementary_Figure_S3" = paste(
      "Secondary internal model-performance analyses for the baseline, +BMI, +VFA, and FDP models.",
      "(A) shows pooled outer-fold AUROC with 95% confidence intervals and AUROC differences versus the FDP model.",
      "(B) shows Brier score.",
      "(C) shows the FDP calibration curve with bootstrap calibration intercept and slope.",
      "(D) shows exploratory decision-curve analysis across the threshold-probability range."
    ),
    "Supplementary_Figure_S4" = paste(
      "Reclassification and calibration analyses comparing the fat-distribution phenotype, FDP, model with reference models.",
      "(A) shows continuous net reclassification improvement, NRI.",
      "(B) shows integrated discrimination improvement, IDI.",
      "(C) shows the event and non-event NRI components.",
      "(D) shows flexible calibration curves for all models."
    ),
    "Supplementary_Figure_S5" = paste(
      "Incremental decision-curve analysis showing the net-benefit gain of the FDP model over each reference model across the threshold-probability range."
    ),
    "Supplementary_Figure_S6" = paste(
      "SHAP-based global interpretation of the selected internal model.",
      "(A) shows mean absolute SHAP importance.",
      "(B) shows the SHAP summary plot; color represents the within-feature percentile from low to high feature values."
    ),
    "Supplementary_Figure_S7" = paste(
      "SHAP dependence plots for the principal phenotype components.",
      "(A) shows PC1 score, and (B) shows PC2 score.",
      "Points represent participants and the loess curve summarizes the marginal pattern."
    ),
    "Supplementary_Figure_S8" = paste(
      "Dynamic-reference longitudinal comparison of phenotype change in the repeated-measures cohort.",
      "(A) shows ΔPC1, and (B) shows ΔPC2 for mild-to-severe progressors and participants who remained in the severe category.",
      "P values are from Wilcoxon rank-sum tests."
    ),
    "Supplementary_Figure_S9" = paste(
      "Cross-platform PCA robustness analyses in NHANES.",
      "(A) shows bootstrap loading intervals for PC1 and PC2.",
      "(B) shows the distribution of Tucker congruence coefficients, with reference lines at 0.90 and 0.95."
    ),
    "Supplementary_Figure_S10" = paste(
      "External discrimination analyses in NHANES.",
      "(A) shows sampling-weighted ROC curves.",
      "(B) shows weighted AUROC across models.",
      "(C) shows weighted Brier score, where lower values indicate better probabilistic accuracy."
    ),
    "Supplementary_Figure_S11" = paste(
      "External sensitivity analyses for transportability.",
      "(A) shows odds ratios from the alcohol-complete sensitivity analyses.",
      "(B) shows the survey-weighted mean NHANES-derived PC2 across CKM stages."
    ),
    "Supplementary_Figure_S12" = paste(
      "Age-sensitivity analyses.",
      "(A) compares the primary and age-adjusted associations of PC2 with CKM severity.",
      "(B) shows the exploratory PC2-by-age interaction estimates."
    )
  )

  preview_files <- list.files(
    DIR_SUPP_FIG,
    pattern = "^Supplementary_Figure_S[0-9]+_preview[.]png$",
    full.names = TRUE
  )

  if (length(preview_files) > 0) {
    get_snum <- function(x) {
      as.integer(gsub("[^0-9]", "", basename(x)))
    }
    preview_files <- preview_files[order(vapply(preview_files, get_snum, integer(1)))]

    doc <- officer::read_docx()
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          "Supplementary Figures",
          officer::fp_text(font.family = "Arial", font.size = 14, bold = TRUE)
        ),
        fp_p = officer::fp_par(text.align = "center", padding.bottom = 8)
      )
    )

    add_submission_legend <- function(doc, sn, legend_txt) {
      officer::body_add_fpar(
        doc,
        officer::fpar(
          officer::ftext(
            paste0("Supplementary Figure S", sn, ". "),
            officer::fp_text_lite(bold = TRUE)
          ),
          officer::ftext(
            legend_txt,
            officer::fp_text_lite(bold = FALSE)
          )
        ),
        style = "Normal"
      )
    }

    for (i in seq_along(preview_files)) {
      fig <- preview_files[i]
      sn <- get_snum(fig)
      key <- paste0("Supplementary_Figure_S", sn)
      legend_txt <- figure_legend_map[[key]]
      if (length(legend_txt) == 0 || is.na(legend_txt)) legend_txt <- "Legend not available."

      dims <- dim(png::readPNG(fig))
      h_px <- if (length(dims) >= 2) dims[1] else 1200
      w_px <- if (length(dims) >= 2) dims[2] else 1600
      aspect <- h_px / w_px
      width_in <- 6.65
      height_in <- min(8.6, width_in * aspect)

      if (i > 1) doc <- officer::body_add_break(doc)
      doc <- officer::body_add_img(doc, src = fig, width = width_in, height = height_in, style = "centered")
      doc <- officer::body_add_par(doc, "", style = "Normal")
      doc <- add_submission_legend(doc, sn, legend_txt)
    }

    supp_docx <- file.path(DIR_SUPP_FIG, "Supplementary_Figures_submission_ready_reconciled.docx")
    print(doc, target = supp_docx)
    message("Saved supplementary-figure Word file: ", supp_docx)
  } else {
    message("No supplementary figure preview PNG files found in ", DIR_SUPP_FIG,
            "; skipping Supplementary_Figures_compiled.docx generation.")
  }
} else {
  message("Packages 'officer' and/or 'png' are unavailable; skipping Supplementary_Figures_compiled.docx generation.")
}


# Save all figure-ready supplementary data in one workbook for audit and later edits.
supp_wb <- openxlsx::createWorkbook()
add_supp_sheet <- function(name, x) {
  if (is.null(x)) return(invisible(NULL))
  if (is.matrix(x)) x <- as.data.frame(x)
  if (!is.data.frame(x)) return(invisible(NULL))
  openxlsx::addWorksheet(supp_wb, substr(name, 1, 31))
  openxlsx::writeData(supp_wb, substr(name, 1, 31), x)
}
if (!is.null(sup$ml)) {
  add_supp_sheet("S2_algorithm_benchmark", sup$ml$algorithm_benchmark)
  add_supp_sheet("S2_selection_frequency", sup$ml$selection_frequency)
  add_supp_sheet("S3_comparator_performance", sup$ml$comparator_performance)
  add_supp_sheet("S3_auc_differences", sup$ml$comparator_auc_differences)
  add_supp_sheet("S3_fdp_deciles", sup$ml$fdp_deciles)
  add_supp_sheet("S3_calibration_boot", sup$ml$calibration_bootstrap)
  add_supp_sheet("S4_nri_bootstrap", sup$ml$nri_bootstrap)
  add_supp_sheet("S5_dca_difference", sup$ml$dca_difference)
}
add_supp_sheet("S9_bootstrap_loadings", sup$nhanes_bootstrap_loadings)
add_supp_sheet("S10_external_roc", sup$external_roc)
add_supp_sheet("S11_alcohol_sensitivity", sup$alcohol_full_or)
add_supp_sheet("S11_stage_gradient", sup$stage_gradient)
add_supp_sheet("S12_age_comparison", sup$age_comparison)
add_supp_sheet("S12_age_interactions", sup$age_interactions)
openxlsx::saveWorkbook(supp_wb, file.path(DIR_TABLE, "Supplementary_figure_source_data.xlsx"), overwrite = TRUE)

# ==============================================================================
# 8. Output audit
# ==============================================================================

files <- list.files(c(DIR_FIG, DIR_TABLE), full.names = TRUE, recursive = TRUE)
audit <- data.frame(File = basename(files), Path = normalizePath(files, winslash = "/", mustWork = FALSE),
                    Size_bytes = file.info(files)$size, stringsAsFactors = FALSE)
utils::write.csv(audit, file.path(DIR_AUDIT, "publication_output_manifest.csv"), row.names = FALSE)
writeLines(c(paste0("Created: ", Sys.time()), "", capture.output(sessionInfo())),
           file.path(DIR_AUDIT, "tables_figures_sessionInfo.txt"))

message(
  "Publication tables/figures generated without refitting the locked primary models.\nMain figures: ",
  DIR_MAIN_FIG,
  "\nSupplementary figures S1-S12: ",
  DIR_SUPP_FIG,
  "\nTables: ",
  DIR_TABLE
)
