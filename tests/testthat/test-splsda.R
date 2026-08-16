# Tests for sparse PLS discriminant analysis (splsda, R/splsda.R). The dense
# variant plsda is tested in test-plsda.R. Fits need the spls package, and the
# predictions are floats, so the tests assert on structure.

test_that("splsda fits a sparse PLS discriminant model", {
  skip_on_cran()
  # the skip loads the spls namespace, which prints a note about re-registering
  # caret's splsda S3 methods
  suppressMessages(skip_if_not_installed("spls"))

  set.seed(1)
  fit <- caret:::splsda(as.matrix(iris[, 1:4]), iris$Species, K = 2, eta = 0.5)
  expect_s3_class(fit, "splsda")
  expect_length(
    caret:::predict.splsda(fit, as.matrix(iris[, 1:4])),
    nrow(iris)
  )
})
