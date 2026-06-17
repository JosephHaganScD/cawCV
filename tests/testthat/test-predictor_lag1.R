make_ar_dat <- function(n_subj = 25, n_obs = 50, ar_param = 0.7, seed = 1) {
  set.seed(seed)
  subject <- rep(seq_len(n_subj), each = n_obs)
  x_ar <- as.vector(sapply(
    seq_len(n_subj),
    function(i) as.numeric(arima.sim(list(ar = ar_param), n = n_obs))
  ))
  x_noise <- rnorm(n_subj * n_obs)
  data.frame(subject = subject, x_ar = x_ar, x_noise = x_noise)
}

test_that("predictor_lag1 returns the documented data frame structure", {
  dat <- make_ar_dat()
  out <- predictor_lag1(dat, c("x_ar", "x_noise"), dat$subject)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("predictor", "lag1_median", "lag1_q25", "lag1_q75"))
  expect_equal(nrow(out), 2L)
})

test_that("predictor_lag1 recovers a high lag-1 for an AR(1) series with phi = 0.7", {
  dat <- make_ar_dat(ar_param = 0.7)
  out <- predictor_lag1(dat, "x_ar", dat$subject)
  expect_gt(out$lag1_median[out$predictor == "x_ar"], 0.4)
})

test_that("predictor_lag1 yields near-zero lag-1 for white noise", {
  dat <- make_ar_dat()
  out <- predictor_lag1(dat, "x_noise", dat$subject)
  expect_lt(abs(out$lag1_median[out$predictor == "x_noise"]), 0.2)
})

test_that("predictor_lag1 honours an explicit time argument", {
  dat <- make_ar_dat()
  # Add a sequential time column, then shuffle row order within each subject.
  dat$time <- ave(seq_len(nrow(dat)), dat$subject, FUN = seq_along)
  out_ordered <- predictor_lag1(dat, "x_ar", dat$subject, time = dat$time)
  set.seed(123)
  shuffled_idx <- unlist(tapply(seq_len(nrow(dat)), dat$subject, sample),
                        use.names = FALSE)
  dat_shuf <- dat[shuffled_idx, ]
  out_shuffled <- predictor_lag1(dat_shuf, "x_ar", dat_shuf$subject,
                                 time = dat_shuf$time)
  expect_equal(out_ordered$lag1_median, out_shuffled$lag1_median,
               tolerance = 1e-8)
})

test_that("predictor_lag1 errors when a named predictor is absent", {
  dat <- make_ar_dat()
  expect_error(
    predictor_lag1(dat, c("x_ar", "x_missing"), dat$subject),
    "x_missing"
  )
})
