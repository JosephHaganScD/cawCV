test_that("partition_subject_kfold returns a caw_partition object", {
  p <- partition_subject_kfold(rep(1:12, each = 5), k = 4, seed = 1)
  expect_s3_class(p, "caw_partition")
})

test_that("partition_subject_kfold keeps each subject's observations in exactly one fold", {
  cluster <- rep(1:12, each = 5)
  p <- partition_subject_kfold(cluster, k = 4, seed = 1)
  per_subject_folds <- tapply(p$fold_id, cluster, function(x) length(unique(x)))
  expect_true(all(per_subject_folds == 1L))
})

test_that("partition_subject_kfold reports fold count equal to k", {
  cluster <- rep(1:12, each = 5)
  p <- partition_subject_kfold(cluster, k = 4, seed = 1)
  expect_equal(p$k, 4L)
  expect_equal(length(unique(p$fold_id)), 4L)
})

test_that("partition_subject_kfold is reproducible under the same seed", {
  cluster <- rep(1:12, each = 5)
  p1 <- partition_subject_kfold(cluster, k = 4, seed = 99)
  p2 <- partition_subject_kfold(cluster, k = 4, seed = 99)
  expect_identical(p1$fold_id, p2$fold_id)
})

test_that("partition_subject_kfold errors when k exceeds the number of clusters and names partition_loco", {
  cluster <- rep(1:5, each = 4)
  expect_error(
    partition_subject_kfold(cluster, k = 10),
    "partition_loco"
  )
})

test_that("partition_subject_kfold accepts only minimum-length inputs", {
  expect_error(partition_subject_kfold(integer(0), k = 2))
  expect_error(partition_subject_kfold(1L, k = 2))
})
