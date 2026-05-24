#' Leave-one-cluster-out partition
#'
#' Creates one fold per subject (cluster). Each fold holds out all observations
#' from a single subject, and the remaining subjects form the training set.
#' This is the exhaustive cluster-aware strategy. It is deterministic, so no
#' random seed is required.
#'
#' @param cluster A vector of subject or cluster identifiers, one element per
#'   observation.
#'
#' @return A `caw_partition` object with as many folds as there are distinct
#'   clusters.
#'
#' @seealso [partition_naive_kfold()], [partition_subject_kfold()]
#' @export
#'
#' @examples
#' subject <- rep(1:8, each = 4)
#' p <- partition_loco(subject)
#' p
#' n_folds(p)
partition_loco <- function(cluster) {
  cluster <- as.character(cluster)
  n <- length(cluster)
  clusters <- unique(cluster)
  n_clusters <- length(clusters)
  if (n_clusters < 2L) {
    stop("`cluster` must contain at least 2 distinct clusters.", call. = FALSE)
  }
  fold_id <- match(cluster, clusters)
  new_caw_partition(
    fold_id    = fold_id,
    strategy   = "loco",
    k          = n_clusters,
    n_obs      = n,
    n_clusters = n_clusters
  )
}
