# Read-only export of already fitted, locked Chinese plotting objects.
# The supplied code defaults to the authorized user's corrected master.
# Only saved Chinese objects are read; NHANES objects are not accessed.
# The paired-score CSV remains restricted participant data despite lacking IDs.
root <- normalizePath(Sys.getenv("CKM_CODE_ROOT", unset = getwd()), winslash = "/", mustWork = TRUE)
results <- Sys.getenv("CKM_OUTPUT_ROOT", unset = file.path(root, "results"))
publication <- Sys.getenv("CKM_PUBLICATION_ROOT", unset = file.path(results, "IJO_Publication"))
master <- Sys.getenv("CKM_MASTER_FILE", unset = file.path(results, "01_locked_analysis_objects", "CKM_refactored_master_results.rds"))
x <- readRDS(master)
out <- Sys.getenv("CKM_CHINA_PLOT_DATA_DIR", unset = file.path(publication, "work", "locked_china_plot_data"))
if (file.exists(out) && (!dir.exists(out) || length(list.files(out, all.files = TRUE, no.. = TRUE)))) stop("Use a new or empty directory for restricted plotting data.")
dir.create(out, recursive=TRUE, showWarnings=FALSE)
write.csv(x$longitudinal$paired[,c('PC1_Score_0','PC1_Score_1','PC2_Score_0','PC2_Score_1','Progression_Status','Delta_PC1','Delta_PC2')],file.path(out,'paired_scores_no_identifiers.csv'),row.names=FALSE)
write.csv(x$china$rcs$pc1_curve,file.path(out,'pc1_curve.csv'),row.names=FALSE)
write.csv(x$china$rcs$pc2_curve,file.path(out,'pc2_curve.csv'),row.names=FALSE)
write.csv(x$supplementary$ml$calibration_curves,file.path(out,'calibration_curves.csv'),row.names=FALSE)
cat('Exported locked China-only plotting objects, without identifiers or model fitting.\n')
