# Tests for the tuning-parameter selection functions oneSE and tolerance
# (R/selection.R). Both take the resampled performance table that train()
# builds, so the tests use small hand-written tables where the intended pick
# is obvious.

test_that("oneSE picks the simplest model within one standard error", {
  # RMSE is minimised by row 3, and one standard error above it
  # (0.5 + 0.8/sqrt(4) = 0.9) also covers row 2, so the simpler row 2 wins
  perf <- data.frame(
    k = 1:4,
    RMSE = c(1.5, 0.8, 0.5, 0.6),
    RMSESD = c(0.2, 0.8, 0.8, 0.8)
  )
  expect_identical(oneSE(perf, "RMSE", num = 4, maximize = FALSE), 2L)

  # accuracy is maximised by row 3; one standard error below it (0.9 - 0.2/2)
  # still covers row 2
  perf_max <- data.frame(
    k = 1:4,
    Accuracy = c(0.5, 0.82, 0.9, 0.88),
    AccuracySD = c(0.1, 0.2, 0.2, 0.2)
  )
  expect_identical(oneSE(perf_max, "Accuracy", num = 4, maximize = TRUE), 2L)
})

test_that("oneSE returns the best model when nothing else is within range", {
  # a tiny standard error leaves only the best row in the candidate set
  perf <- data.frame(
    k = 1:3,
    RMSE = c(3, 2, 1),
    RMSESD = c(0.001, 0.001, 0.001)
  )
  expect_identical(oneSE(perf, "RMSE", num = 10, maximize = FALSE), 3L)
})

test_that("oneSE tightens its window as the number of resamples grows", {
  perf <- data.frame(
    k = 1:3,
    RMSE = c(1.2, 0.9, 0.5),
    RMSESD = c(0.5, 0.5, 0.5)
  )
  # with few resamples the standard error is wide enough to reach row 2
  expect_identical(oneSE(perf, "RMSE", num = 1, maximize = FALSE), 2L)
  # with many resamples only the best row qualifies
  expect_identical(oneSE(perf, "RMSE", num = 500, maximize = FALSE), 3L)
})

test_that("tolerance picks the simplest model within a percentage of the best", {
  # the best RMSE is 1.0; row 2 is 3% worse, so a 5% tolerance accepts it
  perf <- data.frame(
    k = 1:3,
    RMSE = c(2, 1.03, 1)
  )
  expect_identical(tolerance(perf, "RMSE", tol = 5, maximize = FALSE), 2L)
  # a tighter tolerance falls back to the best model
  expect_identical(tolerance(perf, "RMSE", tol = 1, maximize = FALSE), 3L)

  # the same logic when larger is better
  perf_max <- data.frame(
    k = 1:3,
    Accuracy = c(0.5, 0.97, 1)
  )
  expect_identical(
    tolerance(perf_max, "Accuracy", tol = 5, maximize = TRUE),
    2L
  )
  expect_identical(
    tolerance(perf_max, "Accuracy", tol = 1, maximize = TRUE),
    3L
  )
})

test_that("the selection functions can be used by train", {
  skip_on_cran()

  set.seed(4416)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = c(1, 5, 9, 13)),
    trControl = trainControl(
      method = "cv",
      number = 3,
      selectionFunction = "oneSE"
    )
  )
  expect_contains(fit$results$k, fit$bestTune$k)

  set.seed(4416)
  fit2 <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = c(1, 5, 9, 13)),
    trControl = trainControl(
      method = "cv",
      number = 3,
      selectionFunction = "tolerance"
    )
  )
  expect_contains(fit2$results$k, fit2$bestTune$k)
})
