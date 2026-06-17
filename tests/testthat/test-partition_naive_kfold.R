test_that("partition_naive_kfold returns a caw_partition object", {
  p <- partition_naive_kfold(rep(1:10, each = 5), k = 5, seed = 1)
  expect_s3_class(p, "caw_partition")
})

test_that("partition_naive_kfold assigns every observation a fold ID in 1:k", {
  cluster <- rep(1:10, each = 5)
  p <- partition_naive_kfold(cluster, k = 5, seed = 1)
  expect_length(p$fold_id, length(cluster))
  expect_true(all(p$fold_id %in% seq_len(5)))
})

test_that("partition_naive_kfold produces fold sizes balanced to within one observation", {
  cluster <- rep(1:10, each = 5)
  p <- partition_naive_kfold(cluster, k = 7, seed = 1)
  sizes <- as.integer(table(p$fold_id))
  expect_lte(max(sizes) - min(sizes), 1L)
})

test_that("partition_naive_kfold is reproducible under the same seed", {
  cluster <- rep(1:10, each = 5)
  p1 <- partition_naive_kfold(cluster, k = 5, seed = 42)
  p2 <- partition_naive_kfold(cluster, k = 5, seed = 42)
  expect_identical(p1$fold_id, p2$fold_id)
})

test_that("partition_naive_kfold errors on invalid k", {
  cluster <- rep(1:10, each = 5)
  expect_error(partition_naive_kfold(cluster, k = 1))
  expect_error(partition_naive_kfold(cluster, k = length(cluster) + 1))
  expect_error(partition_naive_kfold(cluster, k = 2.5))
})
