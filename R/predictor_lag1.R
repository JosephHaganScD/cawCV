# Lag-1 autocorrelation of a single numeric series. Returns NA when the series
# is too short or has no variance.
.lag1_series <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3L || sd(x) == 0) {
    return(NA_real_)
  }
  cor(x[-length(x)], x[-1])
}

#' Per-predictor within-subject lag-1 autocorrelation
#'
#' Computes, for each predictor, the lag-1 autocorrelation within each subject
#' and then summarises those values across subjects by their median and
#' interquartile range. Lag-1 autocorrelation measures the temporal
#' predictability of a predictor within a subject, the mechanism through which
#' observation-level cross-validation leaks information directly between
#' consecutive records of the same subject.
#'
#' @param data A data frame containing the predictor columns.
#' @param predictors A character vector of column names in `data` for which the
#'   lag-1 autocorrelation is required.
#' @param cluster A vector of subject or cluster identifiers, of length equal
#'   to `nrow(data)`.
#' @param time Optional vector of length equal to `nrow(data)` giving the
#'   measurement time within subject. If supplied, observations are ordered by
#'   `time` within each subject before the autocorrelation is computed. If
#'   `NULL`, the existing row order within each subject is assumed to be time
#'   order.
#'
#' @return A data frame with one row per predictor and four columns:
#'   `predictor`, `lag1_median`, `lag1_q25` and `lag1_q75`. Subjects with fewer
#'   than three non-missing observations contribute `NA` and are excluded from
#'   the summary.
#'
#' @seealso [predictor_icc()]
#' @export
#'
#' @examples
#' set.seed(1)
#' n_subj <- 20
#' n_obs <- 30
#' subject <- rep(seq_len(n_subj), each = n_obs)
#' # x1 is an autocorrelated within-subject series; x2 is white noise
#' x1 <- as.vector(sapply(seq_len(n_subj), function(i) {
#'   as.numeric(arima.sim(list(ar = 0.7), n = n_obs))
#' }))
#' x2 <- rnorm(n_subj * n_obs)
#' dat <- data.frame(subject = subject, x1 = x1, x2 = x2)
#' predictor_lag1(dat, c("x1", "x2"), dat$subject)
predictor_lag1 <- function(data, predictors, cluster, time = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.character(predictors) || length(predictors) < 1L) {
    stop("`predictors` must be a character vector of column names.",
         call. = FALSE)
  }
  not_found <- setdiff(predictors, names(data))
  if (length(not_found) > 0L) {
    stop("Columns not found in `data`: ", paste(not_found, collapse = ", "),
         call. = FALSE)
  }
  if (length(cluster) != nrow(data)) {
    stop("`cluster` must have length equal to nrow(data).", call. = FALSE)
  }
  if (!is.null(time) && length(time) != nrow(data)) {
    stop("`time` must have length equal to nrow(data).", call. = FALSE)
  }
  clusters <- unique(cluster)
  summarise_one <- function(p) {
    x <- data[[p]]
    per_cluster <- vapply(clusters, function(g) {
      idx <- which(cluster == g)
      if (!is.null(time)) {
        idx <- idx[order(time[idx])]
      }
      .lag1_series(x[idx])
    }, numeric(1))
    if (all(is.na(per_cluster))) {
      return(c(lag1_median = NA_real_, lag1_q25 = NA_real_,
               lag1_q75 = NA_real_))
    }
    q <- quantile(per_cluster, c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
    c(lag1_median = q[2], lag1_q25 = q[1], lag1_q75 = q[3])
  }
  res <- do.call(rbind, lapply(predictors, summarise_one))
  data.frame(
    predictor   = predictors,
    lag1_median = res[, "lag1_median"],
    lag1_q25    = res[, "lag1_q25"],
    lag1_q75    = res[, "lag1_q75"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
