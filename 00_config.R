# Configuration consumed only by the authoritative 07a/07b public acquisition scripts.
REPO_ROOT <- normalizePath(Sys.getenv("CKM_REPO_ROOT", unset = getwd()), winslash = "/", mustWork = TRUE)
PUBLIC_NHANES_DIR <- file.path(REPO_ROOT, "data", "public", "nhanes")
