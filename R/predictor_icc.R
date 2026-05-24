# Unbalanced one-way random-effects ANOVA intraclass correlation for a single
# numeric variable. Returns NA when the ICC is not estimable.
.icc_oneway <- function(x, group) {
  ok <- !is.na(x) & !is.na(group)
  x <- x[ok]
  group <- factor(group[ok])
  total_n <- length(x)
  n_groups <- nlevels(group)
  if (n_groups < 2L || total_n <= n_groups) {
    return(NA_real_)
  }
  group_n <- as.numeric(table(group))
  group_mean <- tapply(x, group, mean)
  grand_mean <- mean(x)
  ss_between <- sum(group_n * (group_mean - grand_mean)^2)
  ss_within <- sum((x - group_mean[as.character(group)])^2)
  ms_between <- ss_between / (n_groups - 1)
  ms_within <- ss_within / (total_n - n_groups)
  if (ms_within <= 0) {
    return(NA_real_)
  }
  # Average cluster size adjustment for unbalanced designs.
  n0 <- (total_n - sum(group_n^2) / total_n) / (n_groups - 1)
  (ms_between - ms_within) / (ms_between + (n0 - 1) * ms_within)
}

#' Per-predictor intraclass correlation
#'
#' Computes the intraclass correlation (ICC) of each predictor across subjects,
#' using a one-way random-effects analysis of variance with an adjustment for
#' unbalanced cluster sizes. The ICC quantifies the share of a predictor's
#' total variance that lies between subjects rather than within them.
#' Predictors with a high ICC carry strong within-subject dependence and are
#' the predictors through which observation-level cross-validation leaks
#' information.
#'
#' @param data A data frame containing the predictor columns.
#' @param predictors A character vector of column names in `data` for which the
#'   ICC is required.
#' @param cluster A vector of subject or cluster identifiers, of length equal
#'   to `nrow(data)`.
#'
#' @return A data frame with one row per predictor and two columns,
#'   `predictor` and `icc`. The ICC can be slightly negative when
#'   between-subject variance is negligible, and is `NA` when it cannot be
#'   estimated.
#'
#' @seealso [predictor_lag1()]
#' @export
#'
#' @examples
#' set.seed(1)
#' n_subj <- 20
#' n_obs <- 6
#' subject <- rep(seq_len(n_subj), each = n_obs)
#' b <- rnorm(n_subj)[subject]
#' dat <- data.frame(
#'   subject = subject,
#'   x1 = b + rnorm(n_subj * n_obs),   # carries subject-level structure
#'   x2 = rnorm(n_subj * n_obs)        # no subject-level structure
#' )
#' predictor_icc(dat, c("x1", "x2"), dat$subject)
predictor_icc <- function(data, predictors, cluster) {
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
  icc <- vapply(
    predictors,
    function(p) .icc_oneway(data[[p]], cluster),
    numeric(1)
  )
  data.frame(
    predictor = predictors,
    icc = unname(icc),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
