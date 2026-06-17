make_dat <- function(n_subj = 30, n_obs = 8, sd_b = 1, seed = 1) {
  set.seed(seed)
  subject <- rep(seq_len(n_subj), each = n_obs)
  b <- rnorm(n_subj, sd = sd_b)[subject]
  data.frame(
    subject = subject,
    x_clustered = b + rnorm(n_subj * n_obs),
    x_noise = rnorm(n_subj * n_obs)
  )
}

test_that("predictor_icc returns the documented data frame structure", {
  dat <- make_dat()
  out <- predictor_icc(dat, c("x_clustered", "x_noise"), dat$subject)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("predictor", "icc"))
  expect_equal(nrow(out), 2L)
  expect_type(out$icc, "double")
})

test_that("predictor_icc recovers high ICC for a strong subject-level random intercept", {
  dat <- make_dat(sd_b = 2)
  out <- predictor_icc(dat, "x_clustered", dat$subject)
  expect_gt(out$icc[out$predictor == "x_clustered"], 0.4)
})

test_that("predictor_icc returns near-zero ICC for pure white noise", {
  dat <- make_dat()
  out <- predictor_icc(dat, "x_noise", dat$subject)
  expect_lt(abs(out$icc[out$predictor == "x_noise"]), 0.1)
})

test_that("predictor_icc handles missing values in predictors without erroring", {
  dat <- make_dat()
  dat$x_noise[c(3, 17, 42)] <- NA
  expect_no_error(predictor_icc(dat, "x_noise", dat$subject))
})

test_that("predictor_icc errors when a named predictor is absent from the data", {
  dat <- make_dat()
  expect_error(
    predictor_icc(dat, c("x_clustered", "x_missing"), dat$subject),
    "x_missing"
  )
})
