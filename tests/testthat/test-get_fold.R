test_that("get_fold returns a list of named integer vectors", {
  p <- partition_subject_kfold(rep(1:12, each = 5), k = 4, seed = 1)
  out <- get_fold(p, 1)
  expect_type(out, "list")
  expect_named(out, c("train", "test"))
  expect_type(out$train, "integer")
  expect_type(out$test, "integer")
})

test_that("get_fold partitions rows into disjoint train and test that cover all observations", {
  p <- partition_subject_kfold(rep(1:12, each = 5), k = 4, seed = 1)
  for (f in seq_len(p$k)) {
    out <- get_fold(p, f)
    expect_length(intersect(out$train, out$test), 0L)
    expect_setequal(c(out$train, out$test), seq_along(p$fold_id))
  }
})

test_that("get_fold test indices correspond to observations whose fold ID matches", {
  p <- partition_naive_kfold(rep(1:10, each = 5), k = 5, seed = 1)
  for (f in seq_len(p$k)) {
    out <- get_fold(p, f)
    expect_true(all(p$fold_id[out$test] == f))
    expect_true(all(p$fold_id[out$train] != f))
  }
})

test_that("get_fold errors on a fold number out of range", {
  p <- partition_subject_kfold(rep(1:12, each = 5), k = 4, seed = 1)
  expect_error(get_fold(p, 0))
  expect_error(get_fold(p, 5))
  expect_error(get_fold(p, 2.5))
})
