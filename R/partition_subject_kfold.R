#' Subject-level k-fold partition
#'
#' Assigns whole subjects (clusters) to folds at random, so that every
#' observation from a given subject is placed in the same fold. This prevents
#' the within-subject information leakage that arises under observation-level
#' partitioning.
#'
#' @param cluster A vector of subject or cluster identifiers, one element per
#'   observation.
#' @param k Number of folds. A single integer of at least 2, and not greater
#'   than the number of distinct clusters. Defaults to 10.
#' @param seed Optional integer. If supplied, the global random number
#'   generator state is set with [set.seed()] before fold assignment so the
#'   partition is reproducible.
#'
#' @return A `caw_partition` object.
#'
#' @seealso [partition_naive_kfold()], [partition_loco()]
#' @export
#'
#' @examples
#' subject <- rep(1:12, each = 5)
#' p <- partition_subject_kfold(subject, k = 4, seed = 1)
#' p
#' # whole subjects are kept within a single fold
#' table(subject, p$fold_id)
partition_subject_kfold <- function(cluster, k = 10, seed = NULL) {
  cluster <- as.character(cluster)
  n <- length(cluster)
  if (n < 2L) {
    stop("`cluster` must contain at least 2 observations.", call. = FALSE)
  }
  clusters <- unique(cluster)
  n_clusters <- length(clusters)
  if (!is.numeric(k) || length(k) != 1L || k != round(k) || k < 2) {
    stop("`k` must be a single integer of at least 2.", call. = FALSE)
  }
  if (k > n_clusters) {
    stop("`k` (", k, ") cannot exceed the number of clusters (", n_clusters,
         "). Use a smaller `k`, or partition_loco() for one fold per cluster.",
         call. = FALSE)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  cluster_fold <- rep_len(seq_len(k), n_clusters)
  names(cluster_fold) <- sample(clusters)
  fold_id <- unname(cluster_fold[cluster])
  new_caw_partition(
    fold_id    = fold_id,
    strategy   = "subject_kfold",
    k          = k,
    n_obs      = n,
    n_clusters = n_clusters
  )
}
