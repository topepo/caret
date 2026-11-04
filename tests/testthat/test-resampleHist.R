# Tests for resampleHist (R/resampleHist.R), which plots the distribution of a
# train object's resampled performance.

test_that("resampleHist draws a density plot and a histogram", {
  skip_on_cran()

  set.seed(6427)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "cv", number = 3)
  )

  # the default is a density plot of the best tuning parameter's resamples
  dens <- resampleHist(fit)
  expect_s3_class(dens, "trellis")
  draw_trellis(dens)

  hist <- resampleHist(fit, type = "histogram")
  expect_s3_class(hist, "trellis")
  draw_trellis(hist)
})

test_that("resampleHist works when there is nothing to tune", {
  skip_on_cran()

  # with a single parameter value the results need no merging against bestTune
  set.seed(9313)
  fit <- train(
    mpg ~ hp + wt,
    data = mtcars,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_s3_class(resampleHist(fit), "trellis")
})

test_that("resampleHist needs resampled results", {
  skip_on_cran()

  set.seed(2115)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3)
  )

  # out-of-bag error rates are a single number, not a distribution
  oob <- fit
  oob$control$method <- "oob"
  expect_snapshot(resampleHist(oob), error = TRUE)

  # the check indexes exactly, so removing the resampled results is noticed
  # even on a classification fit (whose `resampledCM` element would otherwise
  # partially match `$resample`)
  no_resamples <- fit
  no_resamples$resample <- NULL
  expect_false(is.null(no_resamples$resample))
  expect_snapshot(resampleHist(no_resamples), error = TRUE)
})
