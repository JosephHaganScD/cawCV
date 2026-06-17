test_that("n_folds returns k for partition_naive_kfold and partition_subject_kfold", {
  p_naive <- partition_naive_kfold(rep(1:10, each = 5), k = 5, seed = 1)
  p_subj  <- partition_subject_kfold(rep(1:12, each = 5), k = 4, seed = 1)
  expect_equal(n_folds(p_naive), 5L)
  expect_equal(n_folds(p_subj), 4L)
})

test_that("n_folds returns the number of distinct clusters for partition_loco", {
  cluster <- rep(1:8, each = 4)
  p <- partition_loco(cluster)
  expect_equal(n_folds(p), length(unique(cluster)))
})

test_that("n_folds errors on input that is not a caw_partition", {
  expect_error(n_folds(list(k = 5)))
  expect_error(n_folds(5))
  expect_error(n_folds(NULL))
})
