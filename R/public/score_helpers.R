# Statistical helpers from the locked analytic configuration.
assert_columns <- function(dat, vars, label = "data") {
  miss <- setdiff(vars, names(dat))
  if (length(miss)) stop(label, " missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# The 2017–2018 questionnaire skips ALQ121 after a lifetime abstention response.
classify_alq_2017_18 <- function(x, label) {
  assert_columns(x, c("SEQN", "ALQ111", "ALQ121"), label)
  frequency <- as.numeric(x$ALQ121)
  dplyr::case_when(
    frequency == 0 ~ FALSE,
    frequency >= 1 & frequency <= 10 ~ TRUE,
    is.na(frequency) & as.numeric(x$ALQ111) == 2 ~ FALSE,
    TRUE ~ NA
  )
}


weighted_mean_sd <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  mu <- sum(w * x) / sum(w)
  s <- sqrt(sum(w * (x - mu)^2) / sum(w))
  c(mean = mu, sd = s)
}


orient_pca <- function(fit, vat_name, leg_names) {
  if (mean(fit$rotation[, "PC1"], na.rm = TRUE) < 0) {
    fit$rotation[, "PC1"] <- -fit$rotation[, "PC1"]
    fit$x[, "PC1"] <- -fit$x[, "PC1"]
  }
  direction <- mean(fit$rotation[leg_names, "PC2"], na.rm = TRUE) - fit$rotation[vat_name, "PC2"]
  if (is.finite(direction) && direction < 0) {
    fit$rotation[, "PC2"] <- -fit$rotation[, "PC2"]
    fit$x[, "PC2"] <- -fit$x[, "PC2"]
  }
  fit
}


extract_svy_or <- function(fit, design, term) {
  cm <- stats::coef(summary(fit))
  if (!term %in% rownames(cm)) return(data.frame(term = term, OR = NA, Lower = NA, Upper = NA, P = NA))
  df <- survey::degf(design)
  crit <- if (is.finite(df) && df > 0) stats::qt(0.975, df) else stats::qnorm(0.975)
  b <- cm[term, 1]; se <- cm[term, 2]
  data.frame(term = term, OR = exp(b), Lower = exp(b - crit * se), Upper = exp(b + crit * se), P = cm[term, 4])
}

