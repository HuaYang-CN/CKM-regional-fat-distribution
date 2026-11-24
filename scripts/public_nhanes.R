# Run from any directory: Rscript scripts/public_nhanes.R CACHE NEW_OUTPUT [--offline]
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 2:3 || (length(args) == 3 && args[3] != "--offline"))
  stop("Usage: Rscript scripts/public_nhanes.R CACHE NEW_OUTPUT [--offline]")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
repo <- dirname(dirname(normalizePath(script, mustWork = TRUE)))
# Resolve existing ancestors too, including symlinks. Output and input trees must be disjoint.
canonical_directory <- function(path) {
  if (!nzchar(path)) stop("Empty directory path")
  path <- path.expand(path)
  if (file.exists(path)) return(normalizePath(path, winslash="/", mustWork=TRUE))
  parent <- dirname(path)
  if (identical(parent,path)) stop("Cannot resolve directory path: ",path)
  leaf <- basename(path)
  parent <- canonical_directory(parent)
  if (leaf == ".") return(parent)
  if (leaf == "..") return(dirname(parent))
  file.path(parent,leaf)
}
cache <- canonical_directory(args[1])
out <- canonical_directory(args[2])
paths <- c(cache,out)
if (.Platform$OS.type == "windows") paths <- tolower(paths)
prefix <- paste0(sub("/+$","",paths),"/")
if (startsWith(prefix[1],prefix[2]) || startsWith(prefix[2],prefix[1]))
  stop("CACHE and NEW_OUTPUT must be separate directories outside one another.")
if (file.exists(out) || !dir.create(out, recursive = TRUE))
  stop("Select a new output directory; existing runs are preserved.")
if (!dir.exists(cache) && !dir.create(cache, recursive = TRUE)) stop("Cannot create cache")
offline <- length(args) == 3
source(file.path(repo,"R/public/locked_reconstruction.R"), encoding="UTF-8")
source(file.path(repo,"R/public/score_helpers.R"), encoding="UTF-8")
source(file.path(repo,"R/public/association.R"), encoding="UTF-8")
for (package in c("haven", "dplyr", "tidyr", "purrr", "stringr", "tibble", "readr", "survey", "preventr"))
  suppressPackageStartupMessages(library(package, character.only=TRUE))
stopifnot(packageVersion("preventr")=="0.12.0", packageVersion("dplyr") >= "1.1.4")
options(timeout=max(300, getOption("timeout")), survey.lonely.psu="adjust", survey.adjust.domain.lonely=TRUE)
inputs <- read.delim(file.path(repo,"manifests/public_cdc_inputs.tsv"), stringsAsFactors=FALSE)
verified <- function(path, row) {
  isTRUE(file.info(path)$size == row$bytes) && identical(unname(tools::md5sum(path)),row$md5)
}
fetch_one <- function(row) {
  path <- file.path(cache,paste0(row$dataset, ".XPT"))
  if (file.exists(path)) {
    if (!verified(path,row)) stop("Unexpected snapshot in cache: ", row$dataset)
    return(invisible(TRUE))
  }
  if (offline) stop("Missing offline input: ",row$dataset)
  tmp <- tempfile(tmpdir=cache)
  on.exit(unlink(tmp))
  utils::download.file(row$url, tmp, mode="wb", method="libcurl")
  if (!verified(tmp,row)) stop("Downloaded snapshot mismatch: ",row$dataset)
  if (!file.copy(tmp,path,overwrite=FALSE) && !verified(path,row)) stop("Could not save cache input")
}
for (i in seq_len(nrow(inputs))) fetch_one(inputs[i,,drop=FALSE])
state <- new.env(parent=emptyenv()); state$joins <- list()
parts <- lapply(seq_len(nrow(CYCLES)),function(i) {
  cy <- CYCLES[i,,drop=FALSE]
  for (stub in names(REQUIRED_VARS))
    validate_xpt(file.path(cache,paste0(stub,"_",cy$suffix,".XPT")),stub,cy)
  harmonize_cycle(cy$cycle, cy$suffix, cache, state)
})
merged <- dplyr::bind_rows(parts)
stopifnot(!anyDuplicated(merged$SEQN))
derived <- derive_revised_staging(derive_harmonized_variables(merged))
counts <- flow_counts(nrow(merged), derived$age_eligible)
write.table(data.frame(step=names(counts),N=as.integer(counts)),file.path(out,"flow.tsv"),
            sep="\t",row.names=FALSE,quote=FALSE)
run_associations(derived$final, cache, out, file.path(repo,"manifests/public_fixed_loadings.csv"))
writeLines(capture.output(sessionInfo()),file.path(out,"sessionInfo.txt"))
