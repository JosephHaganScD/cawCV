#' Naive observation-level k-fold partition
#'
#' Assigns each observation to one of `k` folds at random, ignoring subject
#' clustering. Observations from the same subject may therefore be split across
#' the training and test sets. This is the leakage-prone baseline strategy and
#' is provided for comparison with the cluster-aware strategies.
#'
#' @param cluster A vector of subject or cluster identifiers, one element per
#'   observation. Only its length is used. The grouping is intentionally
#'   ignored, which is what makes this strategy leakage-prone.
#' @param k Number of folds. A single integer of at least 2. Defaults to 10.
#' @param seed Optional integer. If supplied, the global random number
#'   generator state is set with [set.seed()] before fold assignment so the
#'   partition is reproducible.
#'
#' @return A `caw_partition` object.
#'
#' @seealso [partition_subject_kfold()], [partition_loco()]
#' @export
#'
#' @examples
#' subject <- rep(1:6, each = 5)
#' p <- partition_naive_kfold(subject, k = 5, seed = 1)
#' p
#' get_fold(p, 1)
partition_naive_kfold <- function(cluster, k = 10, seed = NULL) {
  cluster <- as.character(cluster)
  n <- length(cluster)
  if (n < 2L) {
    stop("`cluster` must contain at least 2 observations.", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1L || k != round(k) || k < 2) {
    stop("`k` must be a single integer of at least 2.", call. = FALSE)
  }
  if (k > n) {
    stop("`k` (", k, ") cannot exceed the number of observations (", n, ").",
         call. = FALSE)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  fold_id <- sample(rep_len(seq_len(k), n))
  new_caw_partition(
    fold_id    = fold_id,
    strategy   = "naive_kfold",
    k          = k,
    n_obs      = n,
    n_clusters = length(unique(cluster))
  )
}
