# Stage 7.5 final figure and supplementary-display production
#
# This script redraws locked displays only. It does not refit inferential models.
# Set STAGE75_MASTER_RDS to the verified CKM_refactored_master_results.rds file.

options(stringsAsFactors = FALSE, scipen = 999)

script_arg <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_arg[grepl("^--file=", script_arg)])
if (!length(script_file)) stop("Run this file with Rscript.")
script_dir <- dirname(normalizePath(script_file[1], winslash = "/"))
prod_root <- file.path(normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE), "results", "stage7_5_production")
stage751a_only <- identical(Sys.getenv("STAGE751A_ONLY", unset = "0"), "1")
stage751b_only <- identical(Sys.getenv("STAGE751B_ONLY", unset = "0"), "1")

master_file <- Sys.getenv("STAGE75_MASTER_RDS", unset = "")
if (!nzchar(master_file)) {
  master_file <- file.path(dirname(script_dir), "results", "01_locked_analysis_objects", "CKM_refactored_master_results.rds")
}
if (!file.exists(master_file)) {
  stop("Verified locked master-results RDS not found. Set STAGE75_MASTER_RDS.")
}

main_dir <- file.path(prod_root, "main_figures")
supp_dir <- file.path(prod_root, "supplementary_figures")
qa_dir <- file.path(prod_root, "qa")
dir.create(main_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(supp_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

res <- readRDS(master_file)

stopifnot(
  nrow(res$china$raw_analysis) == 955L,
  nrow(res$longitudinal$paired) == 100L,
  nrow(res$longitudinal$progressors) == 37L,
  nrow(res$longitudinal$stable_severe) == 63L,
  nrow(res$nhanes$data) == 2254L,
  sum(res$nhanes$data$advanced_ckm, na.rm = TRUE) == 157L
)

COL_PC1 <- "#C44E52"
COL_PC2 <- "#4C72B0"
COL_CHINA <- "#C44E52"
COL_NHANES <- "#4C72B0"
COL_DARK <- "#222222"
COL_GRAY <- "#6B7280"
COL_LIGHT <- "#D1D5DB"
COL_PALE_BLUE <- "#DDE7F3"
COL_PALE_RED <- "#F2D9D8"

open_pdf <- function(path, width, height) {
  grDevices::cairo_pdf(path, width = width, height = height, family = "sans", onefile = TRUE)
}

open_png <- function(path, width, height, res_dpi = 240) {
  grDevices::png(path, width = width, height = height, units = "in", res = res_dpi,
                 type = "cairo-png", bg = "white")
}

open_tiff <- function(path, width, height, res_dpi = 600) {
  grDevices::tiff(path, width = width, height = height, units = "in", res = res_dpi,
                  compression = "lzw", type = "cairo", bg = "white")
}

base_par <- function() {
  par(family = "sans", fg = COL_DARK, col.axis = COL_DARK, col.lab = COL_DARK,
      cex.axis = 0.86, cex.lab = 0.92, las = 1, xaxs = "i", yaxs = "i")
}

panel_letter <- function(letter) {
  mtext(letter, side = 3, adj = 0, line = 0.35, font = 2, cex = 1.05)
}

fmt_p <- function(p) {
  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
}

save_panel <- function(draw_fun, stem, width, height) {
  pdf_path <- file.path(main_dir, paste0(stem, ".pdf"))
  png_path <- file.path(main_dir, paste0(stem, "_preview.png"))
  open_pdf(pdf_path, width, height); draw_fun(); dev.off()
  open_png(png_path, width, height, 240); draw_fun(); dev.off()
}

save_composite <- function(draw_fun, stem, width, height) {
  open_pdf(file.path(main_dir, paste0(stem, ".pdf")), width, height)
  draw_fun(); dev.off()
  open_png(file.path(main_dir, paste0(stem, "_preview.png")), width, height, 240)
  draw_fun(); dev.off()
  open_tiff(file.path(main_dir, paste0(stem, ".tiff")), width, height, 600)
  draw_fun(); dev.off()
}

draw_box <- function(x, y, w, h, label, fill = "white", border = COL_DARK,
                     cex = 0.82, font = 1) {
  rect(x - w / 2, y - h / 2, x + w / 2, y + h / 2,
       col = fill, border = border, lwd = 1.2)
  text(x, y, label, cex = cex, font = font)
}

draw_arrow <- function(x0, y0, x1, y1) {
  arrows(x0, y0, x1, y1, length = 0.08, lwd = 1.2, col = COL_GRAY)
}

draw_figure1a <- function(letter = "a") {
  base_par(); par(mar = c(0.6, 0.6, 1.0, 0.6))
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  panel_letter(letter)
  draw_box(0.24, 0.90, 0.34, 0.075, "1,676 screened", fill = "#F7F7F7", font = 2)
  draw_arrow(0.24, 0.86, 0.24, 0.79)
  draw_box(0.24, 0.73, 0.38, 0.10,
           "475 clinical exclusions\n+\n146 insufficient CKM-staging exclusions",
           fill = "#F7F7F7", cex = 0.72)
  draw_arrow(0.24, 0.68, 0.24, 0.59)
  draw_box(0.24, 0.54, 0.40, 0.11,
           "1,055 remaining Chinese participants\nAge eligibility for both analytic cohorts:\n30-79 years",
           fill = "#FFF4E6", font = 2, cex = 0.68)
  draw_arrow(0.18, 0.50, 0.08, 0.40)
  draw_arrow(0.30, 0.50, 0.42, 0.40)
  draw_box(0.08, 0.31, 0.15, 0.16,
           "DERIVATION\n\nn = 955\n451 severe CKM",
           fill = COL_PALE_RED, cex = 0.67, font = 2)
  draw_box(0.42, 0.31, 0.22, 0.18,
           "REPEATED-MEASURES\nCOHORT\n\nSeparate n = 100\n37 progressors\n63 stable-severe\nreference participants",
           fill = COL_PALE_BLUE, cex = 0.52, font = 2)
  draw_box(0.78, 0.54, 0.36, 0.24,
           "INDEPENDENT EXTERNAL COHORT\n\nNHANES 2011-2018\nn = 2,254\n157 advanced-CKM events",
           fill = "#E8F3EE", cex = 0.66, font = 2)
  text(0.50, 0.06,
       "The repeated-measures cohort is separate from the 955-person derivation cohort.",
       cex = 0.68, col = COL_GRAY)
}

draw_heatmap <- function(mat, row_labels, col_labels, letter = "b", annotations = NULL) {
  base_par(); par(mar = c(4.3, 5.7, 1.1, 2.3))
  nr <- nrow(mat); nc <- ncol(mat)
  plot.new(); plot.window(xlim = c(0.5, nc + 1.45), ylim = c(0.5, nr + 0.5))
  panel_letter(letter)
  pal <- colorRampPalette(c("#2B5C8A", "#F7F7F7", "#C65F5F"))(201)
  lim <- 0.80
  map_col <- function(v) pal[pmax(1, pmin(201, round((v + lim) / (2 * lim) * 200) + 1))]
  for (i in seq_len(nr)) {
    yy <- nr - i + 1
    for (j in seq_len(nc)) {
      v <- mat[i, j]
      rect(j - 0.5, yy - 0.5, j + 0.5, yy + 0.5,
           col = map_col(v), border = "white", lwd = 1.3)
      text(j, yy, sprintf("%.3f", v), cex = 0.74,
           col = ifelse(abs(v) > 0.42, "white", COL_DARK))
    }
  }
  axis(1, at = seq_len(nc), labels = col_labels, tick = FALSE, line = -0.2, cex.axis = 0.78)
  axis(2, at = nr:1, labels = row_labels, tick = FALSE, line = -0.3, cex.axis = 0.78)
  box(col = COL_LIGHT)
  yy <- seq(1, nr, length.out = 201)
  for (k in seq_len(200)) rect(nc + 0.56, yy[k], nc + 0.76, yy[k + 1], col = pal[k], border = NA)
  text(nc + 0.87, c(1, (nr + 1) / 2, nr), c("-0.80", "0", "0.80"), adj = 0, cex = 0.62)
  if (!is.null(annotations)) {
    mtext(annotations, side = 1, line = 3.1, cex = 0.68, col = COL_GRAY)
  }
}

china_load <- res$china$pca_lock$rotation[, c("PC1", "PC2"), drop = FALSE]
china_order <- res$china$pca_lock$variable_order
china_load <- china_load[china_order, , drop = FALSE]

draw_figure1b <- function(letter = "b") {
  draw_heatmap(china_load, china_order, c("PC1", "PC2"), letter,
               "PC1 explained 90.6%; PC2 explained 5.3%")
}

draw_figure1 <- function() {
  layout(matrix(c(1, 2), nrow = 1), widths = c(1.08, 0.92))
  draw_figure1a("a"); draw_figure1b("b")
}

if (!stage751b_only) {
  save_panel(function() draw_figure1a("a"), "Figure_1a_Study_architecture", 6.4, 5.3)
  save_panel(function() draw_figure1b("b"), "Figure_1b_Chinese_PCA_loadings", 5.8, 5.3)
  save_composite(draw_figure1, "Figure_1_Study_architecture_and_Chinese_PCA_loading_contrast", 11.8, 5.6)

  write.csv(
    data.frame(Variable = rownames(china_load), PC1 = china_load[, 1], PC2 = china_load[, 2]),
    file.path(qa_dir, "Figure1b_loading_values.csv"), row.names = FALSE
  )
}

if (stage751a_only) {
  message("Stage 7.5.1A Figure 1 targeted production completed: ", prod_root)
  quit(save = "no", status = 0)
}

draw_forest <- function(dat, labels, letter, xlab, colors, p_values,
                        xlim = NULL, log_axis = FALSE) {
  base_par(); par(mar = c(4.2, 9.3, 1.2, 5.1))
  y <- rev(seq_len(nrow(dat)))
  if (is.null(xlim)) xlim <- range(c(dat$Lower, dat$Upper), finite = TRUE)
  right_room <- if (log_axis) xlim[2] * 2.0 else xlim[2] + diff(xlim) * 0.55
  plot.new(); plot.window(xlim = c(xlim[1], right_room), ylim = c(0.4, nrow(dat) + 0.6), log = if (log_axis) "x" else "")
  panel_letter(letter)
  abline(v = 1, lty = 2, col = COL_GRAY)
  for (i in seq_len(nrow(dat))) {
    segments(dat$Lower[i], y[i], dat$Upper[i], y[i], col = colors[i], lwd = 2.1)
    points(dat$OR[i], y[i], pch = ifelse(i %% 2 == 0, 15, 16), col = colors[i], cex = 1.05)
    text(right_room * if (log_axis) 0.70 else 0.78, y[i],
         sprintf("%.2f (%.2f-%.2f)", dat$OR[i], dat$Lower[i], dat$Upper[i]),
         adj = 0, cex = 0.70)
    text(right_room * if (log_axis) 0.93 else 0.94, y[i], fmt_p(p_values[i]), adj = 0.5, cex = 0.70)
  }
  axis(1, cex.axis = 0.78)
  axis(2, at = y, labels = labels, tick = FALSE, line = -0.3, cex.axis = 0.74)
  mtext(xlab, side = 1, line = 2.5, cex = 0.88)
  text(right_room * if (log_axis) 0.70 else 0.78, nrow(dat) + 0.52, "OR (95% CI)", adj = 0, font = 2, cex = 0.72)
  text(right_room * if (log_axis) 0.93 else 0.94, nrow(dat) + 0.52, "P value", font = 2, cex = 0.72)
  box(bty = "l", col = COL_DARK)
}

or2 <- res$china$primary_or
or2 <- or2[match(c("PC1.Crude", "PC1.Adjusted", "PC2.Crude", "PC2.Adjusted"),
                 paste(or2$Exposure, or2$Model, sep = ".")), ]
draw_figure2a <- function(letter = "a") {
  draw_forest(or2, c("PC1 crude", "PC1 adjusted", "PC2 crude", "PC2 adjusted"), letter,
              "Odds ratio per score-unit increase", c(COL_GRAY, COL_PC1, COL_GRAY, COL_PC2),
              or2$P, xlim = c(0.35, 1.35), log_axis = TRUE)
}

draw_spline <- function(curve, score_col, color, overall, nonlinear, letter) {
  base_par(); par(mar = c(4.2, 4.4, 1.2, 1.0))
  x <- curve[[score_col]]
  ylim <- range(c(curve$lower, curve$upper), finite = TRUE)
  plot(x, curve$yhat, type = "n", ylim = ylim, xlab = paste0(sub("_Score", "", score_col), " score"),
       ylab = "Adjusted odds ratio")
  polygon(c(x, rev(x)), c(curve$lower, rev(curve$upper)),
          col = grDevices::adjustcolor(color, alpha.f = 0.18), border = NA)
  lines(x, curve$yhat, col = color, lwd = 2.2)
  abline(h = 1, lty = 2, col = COL_GRAY)
  panel_letter(letter)
  usr <- par("usr")
  text(usr[1] + 0.05 * diff(usr[1:2]), usr[4] - 0.09 * diff(usr[3:4]),
       paste0("Overall P ", overall, "\nNonlinear P = ", nonlinear),
       adj = c(0, 1), cex = 0.74)
  box(bty = "l")
}

draw_figure2b <- function(letter = "b") draw_spline(res$china$rcs$pc1_curve, "PC1_Score", COL_PC1, "<0.001", "0.061", letter)
draw_figure2c <- function(letter = "c") draw_spline(res$china$rcs$pc2_curve, "PC2_Score", COL_PC2, "= 0.002", "0.150", letter)
draw_figure2 <- function() {
  layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE), heights = c(0.92, 1.08))
  draw_figure2a("a"); draw_figure2b("b"); draw_figure2c("c")
}

if (!stage751b_only) {
  save_panel(function() draw_figure2a("a"), "Figure_2a_Chinese_concurrent_association_forest", 8.8, 4.1)
  save_panel(function() draw_figure2b("b"), "Figure_2b_PC1_restricted_cubic_spline", 5.2, 4.6)
  save_panel(function() draw_figure2c("c"), "Figure_2c_PC2_restricted_cubic_spline", 5.2, 4.6)
  save_composite(draw_figure2, "Figure_2_Chinese_concurrent_CKM_associations", 10.8, 8.2)
  write.csv(or2, file.path(qa_dir, "Figure2a_values.csv"), row.names = FALSE)
  write.csv(res$china$rcs$pc1_curve, file.path(qa_dir, "Figure2b_spline_values.csv"), row.names = FALSE)
  write.csv(res$china$rcs$pc2_curve, file.path(qa_dir, "Figure2c_spline_values.csv"), row.names = FALSE)
}

load3 <- res$nhanes$loading_comparison
load3 <- load3[match(c("BMI", "VAT", "Trunk", "Left arm", "Right arm", "Left leg", "Right leg"), load3$Variable), ]
load3_mat <- as.matrix(load3[, c("China_PC1", "NHANES_PC1", "China_PC2", "NHANES_PC2")])
rownames(load3_mat) <- load3$Variable
load3_display_labels <- rownames(load3_mat)
load3_display_labels[load3_display_labels == "VAT"] <- "VFA / VAT"

draw_figure3a <- function(letter = "a") {
  draw_heatmap(load3_mat, load3_display_labels, c("China PC1", "NHANES PC1", "China PC2", "NHANES PC2"), letter,
               "Tucker: PC1 0.975 (0.971-0.979); PC2 0.952 (0.946-0.958)")
}

nh <- res$nhanes$data
draw_figure3b <- function(letter = "b") {
  base_par(); par(mar = c(4.2, 4.6, 1.2, 1.0))
  advanced <- as.integer(nh$advanced_ckm) == 1L
  plot(nh$PC2_z, nh$China_loading_PC2_z, pch = 16, cex = 0.43,
       col = ifelse(advanced, grDevices::adjustcolor(COL_CHINA, 0.50), grDevices::adjustcolor(COL_NHANES, 0.28)),
       xlab = "NHANES-derived PC2, weighted-SD units",
       ylab = "Fixed China-loading PC2, weighted-SD units")
  abline(lm(nh$China_loading_PC2_z ~ nh$PC2_z), col = COL_DARK, lwd = 1.8)
  panel_letter(letter)
  usr <- par("usr")
  text(usr[1] + 0.04 * diff(usr[1:2]), usr[4] - 0.05 * diff(usr[3:4]),
       "Pearson r = 0.926", adj = c(0, 1), cex = 0.82, font = 2)
  legend("bottomright", legend = c("Stages 0-2", "Advanced CKM"),
         pch = 16, col = c(COL_NHANES, COL_CHINA), bty = "n", cex = 0.70)
  box(bty = "l")
}

or3 <- res$nhanes$external_or
or3 <- or3[match(c("NHANES-derived PC1", "NHANES-derived PC2", "Fixed China-loading PC1", "Fixed China-loading PC2"), or3$Analysis), ]
draw_figure3c <- function(letter = "c") {
  draw_forest(or3, or3$Analysis, letter,
              "Survey-weighted odds ratio per 1 weighted SD",
              c(COL_PC1, COL_PC2, COL_PC1, COL_PC2), or3$P,
              xlim = c(0.35, 2.7), log_axis = TRUE)
}

draw_figure3 <- function() {
  layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE), heights = c(1.12, 0.88))
  draw_figure3a("a"); draw_figure3b("b"); draw_figure3c("c")
}

if (stage751b_only) {
  save_panel(function() draw_figure3a("a"), "Figure_3a_Loading_structure", 6.3, 5.4)
  save_composite(draw_figure3, "Figure_3_NHANES_structural_replication_and_association", 11.8, 9.2)
} else {
  save_panel(function() draw_figure3a("a"), "Figure_3a_Loading_structure", 6.3, 5.4)
  save_panel(function() draw_figure3b("b"), "Figure_3b_Fixed_loading_score_correlation", 5.8, 5.4)
  save_panel(function() draw_figure3c("c"), "Figure_3c_NHANES_concurrent_association", 9.2, 4.1)
  save_composite(draw_figure3, "Figure_3_NHANES_structural_replication_and_association", 11.8, 9.2)
  write.csv(load3, file.path(qa_dir, "Figure3a_loading_values.csv"), row.names = FALSE)
  write.csv(data.frame(SEQN = nh$SEQN, NHANES_PC2 = nh$PC2_z,
                       Fixed_China_loading_PC2 = nh$China_loading_PC2_z,
                       Advanced_CKM = as.integer(nh$advanced_ckm)),
            file.path(qa_dir, "Figure3b_score_values.csv"), row.names = FALSE)
  write.csv(or3, file.path(qa_dir, "Figure3c_OR_values.csv"), row.names = FALSE)
}

prog <- res$longitudinal$progressors
stable <- res$longitudinal$stable_severe

draw_figure4a <- function(letter = "a") {
  base_par(); par(mar = c(4.1, 4.4, 1.2, 0.8))
  y0 <- prog$PC2_Score_0; y1 <- prog$PC2_Score_1
  ylim <- range(c(y0, y1), finite = TRUE)
  boxplot(list(Baseline = y0, `Follow-up` = y1), ylim = ylim, ylab = "PC2 score",
          border = c(COL_NHANES, COL_CHINA), col = c(COL_PALE_BLUE, COL_PALE_RED),
          outline = FALSE, whisklty = 1)
  set.seed(75041)
  x0 <- jitter(rep(1, length(y0)), amount = 0.06)
  x1 <- jitter(rep(2, length(y1)), amount = 0.06)
  for (i in seq_along(y0)) segments(x0[i], y0[i], x1[i], y1[i], col = grDevices::adjustcolor(COL_GRAY, 0.28))
  points(x0, y0, pch = 16, cex = 0.56, col = grDevices::adjustcolor(COL_NHANES, 0.75))
  points(x1, y1, pch = 16, cex = 0.56, col = grDevices::adjustcolor(COL_CHINA, 0.75))
  panel_letter(letter)
  mtext("Paired Wilcoxon P = 0.013; paired t-test sensitivity P = 0.016", side = 3, line = 0.25, cex = 0.68)
  box(bty = "l")
}

draw_figure4b <- function(letter = "b") {
  base_par(); par(mar = c(4.1, 4.4, 1.2, 0.8))
  y0 <- prog$PC2_Score_0; y1 <- prog$PC2_Score_1
  dec <- y1 < y0
  plot(c(1, 2), range(c(y0, y1)), type = "n", xaxt = "n", xlim = c(0.85, 2.15),
       xlab = "Assessment", ylab = "PC2 score")
  axis(1, at = c(1, 2), labels = c("Baseline", "Follow-up"))
  for (i in seq_along(y0)) {
    segments(1, y0[i], 2, y1[i], col = grDevices::adjustcolor(ifelse(dec[i], COL_PC2, COL_GRAY), 0.64), lwd = 1.2)
    points(c(1, 2), c(y0[i], y1[i]), pch = 16, cex = 0.48,
           col = ifelse(dec[i], COL_PC2, COL_GRAY))
  }
  panel_letter(letter)
  usr <- par("usr")
  text(usr[1] + 0.05 * diff(usr[1:2]), usr[4] - 0.05 * diff(usr[3:4]),
       "73% had lower PC2 at follow-up", adj = c(0, 1), cex = 0.78, font = 2)
  legend("topright", inset = c(0.01, 0.08),
         legend = c("Lower at follow-up", "Not lower at follow-up"),
         col = c(COL_PC2, COL_GRAY), lwd = 1.4, pch = 16,
         bty = "n", cex = 0.66)
  box(bty = "l")
}

draw_figure4c <- function(letter = "c") {
  base_par(); par(mar = c(4.4, 5.0, 1.2, 0.8))
  d1 <- prog$Delta_PC2; d2 <- stable$Delta_PC2
  ylim <- range(c(d1, d2), finite = TRUE)
  boxplot(list(Progressors = d1, `Stable-severe` = d2), ylim = ylim,
          ylab = "Delta PC2 (follow-up - baseline)", outline = FALSE,
          border = c(COL_PC2, COL_GRAY), col = c(COL_PALE_BLUE, "#EEEEEE"))
  set.seed(75042)
  points(jitter(rep(1, length(d1)), amount = 0.12), d1, pch = 16, cex = 0.55,
         col = grDevices::adjustcolor(COL_PC2, 0.62))
  points(jitter(rep(2, length(d2)), amount = 0.12), d2, pch = 16, cex = 0.55,
         col = grDevices::adjustcolor(COL_GRAY, 0.55))
  abline(h = 0, lty = 2, col = COL_LIGHT)
  panel_letter(letter)
  mtext("Wilcoxon rank-sum P = 0.002", side = 3, line = 0.25, cex = 0.72)
  box(bty = "l")
}

draw_figure4 <- function() {
  layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE), heights = c(1.02, 0.98))
  draw_figure4a("a"); draw_figure4b("b"); draw_figure4c("c")
}

if (stage751b_only) {
  save_panel(function() draw_figure4b("b"), "Figure_4b_Individual_PC2_trajectories", 5.7, 4.7)
  save_composite(draw_figure4, "Figure_4_Complementary_repeated_measures_evidence", 11.4, 8.5)
} else {
  save_panel(function() draw_figure4a("a"), "Figure_4a_Paired_PC2_progressors", 5.7, 4.7)
  save_panel(function() draw_figure4b("b"), "Figure_4b_Individual_PC2_trajectories", 5.7, 4.7)
  save_panel(function() draw_figure4c("c"), "Figure_4c_Delta_PC2_group_comparison", 8.8, 4.2)
  save_composite(draw_figure4, "Figure_4_Complementary_repeated_measures_evidence", 11.4, 8.5)

  write.csv(data.frame(Participant = seq_len(nrow(prog)), Baseline_PC2 = prog$PC2_Score_0,
                       Followup_PC2 = prog$PC2_Score_1, Delta_PC2 = prog$Delta_PC2,
                       Lower_at_followup = prog$PC2_Score_1 < prog$PC2_Score_0),
            file.path(qa_dir, "Figure4a_4b_paired_values.csv"), row.names = FALSE)
  write.csv(rbind(
    data.frame(Group = "Progressor", Participant = seq_len(nrow(prog)), Delta_PC2 = prog$Delta_PC2),
    data.frame(Group = "Stable-severe", Participant = seq_len(nrow(stable)), Delta_PC2 = stable$Delta_PC2)
  ), file.path(qa_dir, "Figure4c_delta_values.csv"), row.names = FALSE)

  write.csv(data.frame(
    Metric = c("Cohort n", "Progressors", "Stable-severe", "Baseline median", "Baseline Q1", "Baseline Q3",
               "Follow-up median", "Follow-up Q1", "Follow-up Q3", "Delta median", "Delta Q1", "Delta Q3",
               "Paired Wilcoxon P", "Paired t sensitivity P", "Lower at follow-up percent",
               "Stable-severe Delta median", "Between-group rank-sum P", "Median follow-up days",
               "Follow-up Q1 days", "Follow-up Q3 days", "Follow-up minimum days", "Follow-up maximum days"),
    Value = c(100, 37, 63, 0.31, -0.09, 0.47, 0.13, -0.26, 0.46, -0.08, -0.22, 0.01,
              0.013, 0.016, 73, 0.09, 0.002, 379.5, 322.75, 667.5, 92, 968)
  ), file.path(qa_dir, "Figure4_summary_values.csv"), row.names = FALSE)
}

if (stage751b_only) {
  message("Stage 7.5.1B Figure 3/4 targeted production completed: ", prod_root)
  quit(save = "no", status = 0)
}

draw_supp1 <- function() {
  base_par(); par(mar = c(0.7, 0.7, 0.9, 0.7))
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  draw_box(0.50, 0.90, 0.42, 0.08, "1,676 screened", fill = "#F7F7F7", font = 2)
  draw_arrow(0.50, 0.86, 0.50, 0.77)
  draw_box(0.50, 0.70, 0.50, 0.11,
           "475 clinical exclusions\n+ 146 insufficient CKM-staging exclusions",
           fill = "#F7F7F7", cex = 0.78)
  draw_arrow(0.50, 0.64, 0.50, 0.55)
  draw_box(0.50, 0.49, 0.50, 0.09,
           "1,055 eligible participants\nAge 30-79 years for both Chinese cohorts",
           fill = "#FFF4E6", font = 2, cex = 0.76)
  draw_arrow(0.42, 0.44, 0.27, 0.32)
  draw_arrow(0.58, 0.44, 0.73, 0.32)
  draw_box(0.27, 0.22, 0.34, 0.16, "Derivation cohort\nn = 955\n451 severe CKM",
           fill = COL_PALE_RED, cex = 0.80, font = 2)
  draw_box(0.73, 0.22, 0.38, 0.18,
           "Separate repeated-measures cohort\nn = 100\n37 progressors; 63 stable-severe",
           fill = COL_PALE_BLUE, cex = 0.76, font = 2)
  text(0.50, 0.06, "The 100-person cohort was not a subset of the 955-person cohort.",
       cex = 0.78, col = COL_GRAY)
}

draw_supp13 <- function() {
  base_par(); par(mar = c(0.7, 0.7, 0.9, 0.7))
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
  draw_box(0.50, 0.91, 0.48, 0.08, "33,580 NHANES 2011-2018 participants", fill = "#F7F7F7", font = 2)
  draw_arrow(0.50, 0.87, 0.50, 0.79)
  draw_box(0.50, 0.74, 0.36, 0.075, "9,520 aged 30-59 years", fill = "#FFF4E6", font = 2)
  draw_arrow(0.44, 0.70, 0.28, 0.60)
  draw_arrow(0.56, 0.70, 0.72, 0.60)
  draw_box(0.25, 0.52, 0.37, 0.11, "3,963 with positive fasting\nsubsample weight", fill = COL_PALE_BLUE, cex = 0.78)
  draw_box(0.75, 0.52, 0.37, 0.11, "3,368 with complete revised\nCKM staging", fill = COL_PALE_RED, cex = 0.78)
  draw_arrow(0.32, 0.46, 0.44, 0.38)
  draw_arrow(0.68, 0.46, 0.56, 0.38)
  draw_box(0.50, 0.33, 0.48, 0.09,
           "2,993 meeting both requirements", fill = "#E8F3EE", font = 2)
  draw_arrow(0.50, 0.28, 0.50, 0.20)
  draw_box(0.50, 0.15, 0.53, 0.09,
           "739 lacking at least 1 of 7 adiposity inputs excluded", fill = "#F7F7F7", cex = 0.75)
  draw_arrow(0.50, 0.10, 0.50, 0.085)
  draw_box(0.50, 0.05, 0.40, 0.07, "2,254 final analytic participants", fill = "#E8F3EE", font = 2)
}

save_supp <- function(draw_fun, stem, width, height) {
  open_pdf(file.path(supp_dir, paste0(stem, ".pdf")), width, height); draw_fun(); dev.off()
  open_png(file.path(supp_dir, paste0(stem, "_preview.png")), width, height, 240); draw_fun(); dev.off()
  open_tiff(file.path(supp_dir, paste0(stem, ".tiff")), width, height, 600); draw_fun(); dev.off()
}

save_supp(draw_supp1, "Supplementary_Figure_S1", 8.0, 6.4)
save_supp(draw_supp13, "Supplementary_Figure_S13_NHANES_attrition", 8.2, 6.5)
write.csv(data.frame(
  Step = c("All NHANES 2011-2018", "Age 30-59", "Positive fasting subsample weight",
           "Complete revised CKM staging", "Both staging and weight", "Missing one or more adiposity inputs", "Final analytic sample"),
  N = c(33580, 9520, 3963, 3368, 2993, 739, 2254),
  Relation = c("start", "subset", "parallel condition", "parallel condition", "intersection", "excluded", "final")
), file.path(qa_dir, "Supplementary_Figure_S13_attrition_values.csv"), row.names = FALSE)

message("Stage 7.5 locked-display production completed: ", prod_root)
