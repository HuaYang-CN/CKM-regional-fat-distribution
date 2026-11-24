# Export only the four aggregate plotting tables and an aggregate outcome count.
# Usage: Rscript scripts/export_s10_aggregates.R AUTHORIZED_LOCKED_MASTER NEW_DIR
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Expected a restricted master object and a new output directory.")
if (file.exists(args[2])) stop("Use a new output directory.")
res <- readRDS(args[1])
ml <- res$supplementary$ml
schemas <- list(
  calibration_curves = c("Model", "Predicted", "Observed"),
  fdp_deciles = c("Decile", "Predicted", "Observed", "N"),
  calibration_bootstrap = c("Metric", "Estimate", "Lower", "Upper"),
  dca = c("threshold", "net_benefit", "Model")
)
tables <- lapply(names(schemas), function(name) {
  dat <- ml[[name]]
  stopifnot(is.data.frame(dat), all(schemas[[name]] %in% names(dat)))
  dat[, schemas[[name]], drop = FALSE]
})
names(tables) <- names(schemas)
outcome <- res$ml$oof_comparators$Outcome
stopifnot(length(outcome) > 0, !anyNA(outcome), all(outcome %in% c("Mild", "Severe")))
tables$event_counts <- data.frame(total = length(outcome), advanced = sum(outcome == "Severe"))
if (!dir.create(args[2], recursive = TRUE)) stop("Cannot create aggregate export directory.")
for (name in names(tables)) {
  write.csv(tables[[name]], file.path(args[2], paste0("s10_", name, ".csv")), row.names = FALSE)
}
