test_that("partition_loco returns a caw_partition object", {
  p <- partition_loco(rep(1:8, each = 4))
  expect_s3_class(p, "caw_partition")
})

test_that("partition_loco produces one fold per distinct cluster", {
  cluster <- rep(1:8, each = 4)
  p <- partition_loco(cluster)
  expect_equal(p$k, length(unique(cluster)))
  expect_equal(length(unique(p$fold_id)), length(unique(cluster)))
})

test_that("partition_loco places each cluster's observations in exactly one fold", {
  cluster <- rep(1:8, each = 4)
  p <- partition_loco(cluster)
  # The test set for each fold should contain observations from exactly one cluster.
  for (f in seq_len(p$k)) {
    test_rows <- get_fold(p, f)$test
    expect_equal(length(unique(cluster[test_rows])), 1L)
  }
})

test_that("partition_loco errors when fewer than two distinct clusters are present", {
  expect_error(partition_loco(rep(1, 10)))
  expect_error(partition_loco(integer(0)))
})
