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

test_that("predict.icr can use the stored fitted values", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  set.seed(5088)
  fit <- icr(mtcars[, -1], mtcars$mpg, n.comp = 3)
  expect_length(predict(fit), nrow(mtcars))
})

test_that("predict.icr rejects missing data in newdata", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  set.seed(3721)
  fit <- icr(mtcars[, -1], mtcars$mpg, n.comp = 3)

  na_row <- as.matrix(mtcars[1, -1])
  na_row[1, 1] <- NA
  expect_snapshot(predict(fit, na_row), error = TRUE)
})

test_that("predict.icr checks the object's class", {
  expect_snapshot(
    caret:::predict.icr(structure(list(), class = "nope")),
    error = TRUE
  )
})

test_that("icr.formula accepts a matrix as the data argument", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  set.seed(2647)
  fit <- icr(mpg ~ ., data = as.matrix(mtcars), n.comp = 2)
  expect_s3_class(fit, "icr")
})

test_that("print.icr reports when there are no coefficients", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  set.seed(4903)
  fit <- icr(mtcars[, -1], mtcars$mpg, n.comp = 2)
  # the print method reads the stored model, so clear its coefficients
  fit$model$coefficients <- NULL
  expect_snapshot(print(fit))
})
