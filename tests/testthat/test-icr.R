# Tests for independent component regression (icr). Needs fastICA. Fits are
# RNG-dependent, so the tests assert on structure/prediction shape and snapshot
# only the deterministic error output.

test_that("icr fits from the default and formula interfaces", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  set.seed(1)
  fit <- icr(mtcars[, -1], mtcars$mpg, n.comp = 3)
  expect_s3_class(fit, "icr")
  expect_length(predict(fit, mtcars[, -1]), nrow(mtcars))

  set.seed(1)
  fit_form <- icr(mpg ~ ., data = mtcars, n.comp = 3)
  expect_s3_class(fit_form, "icr.formula")
  expect_length(predict(fit_form, mtcars), nrow(mtcars))
})

test_that("icr handles a single predictor and single-row prediction", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  # with one predictor, n.comp cannot exceed 1, and preProcess warns that the
  # ICA step is dropped
  set.seed(1)
  expect_snapshot_warning(
    fit <- icr(mtcars[, "hp", drop = FALSE], mtcars$mpg, n.comp = 1)
  )
  expect_length(predict(fit, mtcars[, "hp", drop = FALSE]), nrow(mtcars))

  # a single new row returns a single prediction
  set.seed(1)
  full <- icr(mtcars[, -1], mtcars$mpg, n.comp = 3)
  expect_length(predict(full, mtcars[1, -1, drop = FALSE]), 1)
})

test_that("icr requires a numeric outcome", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  set.seed(1)
  expect_snapshot(icr(iris[, 1:4], iris$Species, n.comp = 2), error = TRUE)
})

test_that("print.icr identifies the model", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  set.seed(1)
  fit <- icr(mtcars[, -1], mtcars$mpg, n.comp = 3)
  # the coefficients are RNG-/BLAS-dependent floats, so match the stable header
  # rather than snapshotting the whole print
  expect_output(print(fit), "Independent Component Regression")
})
