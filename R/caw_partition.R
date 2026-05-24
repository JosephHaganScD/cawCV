# Internal constructor and S3 methods for the 'caw_partition' object returned
# by the partition_* functions.

# Internal constructor. Not exported.
new_caw_partition <- function(fold_id, strategy, k, n_obs, n_clusters) {
  structure(
    list(
      fold_id    = as.integer(fold_id),
      strategy   = strategy,
      k          = as.integer(k),
      n_obs      = as.integer(n_obs),
      n_clusters = as.integer(n_clusters)
    ),
    class = "caw_partition"
  )
}

#' Print a cross-validation partition
#'
#' @param x A `caw_partition` object.
#' @param ... Ignored. Present for S3 method consistency.
#'
#' @return `x`, invisibly.
#' @export
print.caw_partition <- function(x, ...) {
  label <- c(
    naive_kfold   = "naive observation-level k-fold",
    subject_kfold = "subject-level k-fold",
    loco          = "leave-one-cluster-out"
  )[x$strategy]
  cat("<caw_partition>\n")
  cat("  strategy     :", label, "\n")
  cat("  folds        :", x$k, "\n")
  cat("  observations :", x$n_obs, "\n")
  cat("  clusters     :", x$n_clusters, "\n")
  invisible(x)
}

#' Number of folds in a partition
#'
#' @param partition A `caw_partition` object.
#'
#' @return An integer giving the number of folds.
#' @export
#'
#' @examples
#' p <- partition_subject_kfold(rep(1:12, each = 5), k = 4, seed = 1)
#' n_folds(p)
n_folds <- function(partition) {
  if (!inherits(partition, "caw_partition")) {
    stop("`partition` must be a 'caw_partition' object.", call. = FALSE)
  }
  partition$k
}

#' Extract the training and test rows for one fold
#'
#' @param partition A `caw_partition` object.
#' @param fold A single integer identifying the fold, between 1 and
#'   `n_folds(partition)`.
#'
#' @return A list with two integer vectors, `train` and `test`, giving the row
#'   positions assigned to the training and test sets for the requested fold.
#' @export
#'
#' @examples
#' p <- partition_subject_kfold(rep(1:12, each = 5), k = 4, seed = 1)
#' fold1 <- get_fold(p, 1)
#' str(fold1)
get_fold <- function(partition, fold) {
  if (!inherits(partition, "caw_partition")) {
    stop("`partition` must be a 'caw_partition' object.", call. = FALSE)
  }
  if (!is.numeric(fold) || length(fold) != 1L ||
      fold != round(fold) || fold < 1 || fold > partition$k) {
    stop("`fold` must be a single integer between 1 and ", partition$k, ".",
         call. = FALSE)
  }
  list(
    train = which(partition$fold_id != fold),
    test  = which(partition$fold_id == fold)
  )
}
