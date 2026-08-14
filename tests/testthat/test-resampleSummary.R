# Tests for resampleSummary(), which turns a matrix of per-resample predictions
# into mean/sd performance metrics (and, optionally, the stacked obs/pred data).

test_that("resampleSummary summarises regression resamples", {
  obs <- c(1, 2, 3, 4, 5)
  resampled <- data.frame(
    R1 = c(1.1, 2.1, 2.9, 4.2, 5.1),
    R2 = c(0.9, 2.0, 3.1, 3.8, 5.0)
  )

  out <- resampleSummary(obs, resampled)
  expect_identical(names(out), c("metrics", "data"))
  # regression metrics: RMSE / Rsquared / MAE, as mean then sd (six values)
  expect_length(out$metrics, 6L)

  # the mean RMSE equals the mean of the per-resample RMSEs
  rmses <- vapply(
    resampled,
    function(p) postResample(p, obs)["RMSE"],
    numeric(1)
  )
  expect_equal(unname(out$metrics["RMSE"][1]), mean(rmses))

  # kept data is the predictions stacked with their resample label
  expect_s3_class(out$data, "data.frame")
  expect_identical(colnames(out$data), c("obs", "pred", "group"))
  expect_identical(nrow(out$data), 10L)
})

test_that("resampleSummary can drop the stacked data", {
  obs <- c(1, 2, 3, 4, 5)
  resampled <- data.frame(R1 = c(1, 2, 3, 4, 5), R2 = c(1, 2, 3, 4, 6))
  out <- resampleSummary(obs, resampled, keepData = FALSE)
  expect_null(out$data)
})

test_that("resampleSummary handles the leave-one-out case", {
  # in LOOCV each resample holds out a single observation, so every column has
  # one prediction; the metrics are computed once and their sd is 0
  obs <- c(1, 2, 3, 4, 5)
  m <- matrix(NA_real_, 5, 5)
  diag(m) <- c(1.1, 2.1, 2.9, 4.2, 5.1)

  out <- resampleSummary(obs, as.data.frame(m))
  expect_length(out$metrics, 6L)
  # the second half (the standard deviations) is all zero
  expect_true(all(out$metrics[4:6] == 0))
  expect_identical(nrow(out$data), 5L)
})

test_that("resampleSummary summarises classification resamples", {
  obs <- factor(c("a", "b", "a", "b", "a"), levels = c("a", "b"))
  resampled <- data.frame(
    R1 = factor(c("a", "b", "a", "a", "a"), levels = c("a", "b")),
    R2 = factor(c("a", "b", "b", "b", "a"), levels = c("a", "b"))
  )

  out <- resampleSummary(obs, resampled)
  # classification metrics are Accuracy / Kappa, as mean then sd
  expect_true(all(c("Accuracy", "Kappa") %in% names(out$metrics)))
  # the stacked predictions stay a factor with the original levels
  expect_s3_class(out$data$pred, "factor")
  expect_identical(levels(out$data$pred), c("a", "b"))
})
