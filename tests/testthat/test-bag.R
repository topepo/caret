# Tests for the general bagging function. bagControl and the argument-validation
# errors are unit-tested directly; bag() itself is fit with the ldaBag base
# learner and its predict/print methods are exercised.

# ------------------------------------------------------------------------------
# bagControl

test_that("bagControl fills in sensible defaults", {
  ctrl <- bagControl()
  expect_true(ctrl$oob)
  expect_false(ctrl$downSample)
  expect_true(ctrl$allowParallel)
  # the model functions have no default and must be supplied
  expect_null(ctrl$fit)
  expect_null(ctrl$predict)
  expect_null(ctrl$aggregate)
})

# ------------------------------------------------------------------------------
# argument validation

test_that("bag validates its control and arguments", {
  # bagControl is required
  expect_snapshot(bag(iris[, 1:4], iris$Species), error = TRUE)

  full_ctrl <- bagControl(
    fit = ldaBag$fit,
    predict = ldaBag$pred,
    aggregate = ldaBag$aggregate
  )
  # vars must be a positive integer
  expect_snapshot(
    bag(iris[, 1:4], iris$Species, vars = 0, bagControl = full_ctrl),
    error = TRUE
  )
  # fit/predict/aggregate must all be supplied
  expect_snapshot(
    bag(iris[, 1:4], iris$Species, bagControl = bagControl()),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------
# bag() workflow + methods

test_that("bag fits an ensemble and predicts", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  set.seed(1)
  fit <- suppressWarnings(bag(
    iris[, 1:4],
    iris$Species,
    B = 5,
    bagControl = bagControl(
      fit = ldaBag$fit,
      predict = ldaBag$pred,
      aggregate = ldaBag$aggregate
    )
  ))

  expect_s3_class(fit, "bag")
  expect_length(predict(fit, iris[, 1:4]), nrow(iris))
  # the print output has no fitted metrics, so it is deterministic
  expect_snapshot(print(fit))

  # summary reports out-of-bag performance (seeded, so reproducible in CI)
  expect_snapshot(summary(fit))
})

test_that("bag works with the formula interface", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  set.seed(1)
  fit <- suppressWarnings(bag(
    Species ~ .,
    data = iris,
    B = 5,
    bagControl = bagControl(
      fit = ldaBag$fit,
      predict = ldaBag$pred,
      aggregate = ldaBag$aggregate
    )
  ))

  expect_s3_class(fit, "bag")
  expect_length(predict(fit, iris), nrow(iris))
})
