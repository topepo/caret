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

# ------------------------------------------------------------------------------
# the other built-in base learners (each behind its Suggests package);
# fit_iris_bag() lives in helper-bag.R

test_that("bag works with plsBag", {
  skip_on_cran()
  skip_if_not_installed("pls")
  fit <- fit_iris_bag(plsBag)
  expect_s3_class(fit, "bag")
  expect_length(predict(fit, iris[, 1:4]), nrow(iris))
})

test_that("bag works with nnetBag", {
  skip_on_cran()
  skip_if_not_installed("nnet")
  fit <- fit_iris_bag(nnetBag, size = 3)
  expect_s3_class(fit, "bag")
  expect_length(predict(fit, iris[, 1:4]), nrow(iris))
})

test_that("bag works with svmBag", {
  skip_on_cran()
  skip_if_not_installed("kernlab")
  fit <- fit_iris_bag(svmBag)
  expect_s3_class(fit, "bag")
  expect_length(predict(fit, iris[, 1:4]), nrow(iris))
})

test_that("bag works with ctreeBag", {
  skip_on_cran()
  skip_if_not_installed("party")
  fit <- fit_iris_bag(ctreeBag)
  expect_s3_class(fit, "bag")
  expect_length(predict(fit, iris[, 1:4]), nrow(iris))
})

test_that("bag works with nbBag", {
  skip_on_cran()
  skip_if_not_installed("klaR")
  fit <- fit_iris_bag(nbBag)
  expect_s3_class(fit, "bag")
  expect_length(predict(fit, iris[, 1:4]), nrow(iris))
})

# ------------------------------------------------------------------------------
# bagControl options

test_that("bag can sample a subset of the predictors", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  # `vars` larger than the data is capped at the number of columns
  set.seed(8004)
  fit <- bag(
    iris[, 1:4],
    iris$Species,
    B = 3,
    vars = 10,
    bagControl = bagControl(
      fit = ctreeBag$fit,
      predict = ctreeBag$pred,
      aggregate = ctreeBag$aggregate
    )
  )
  expect_s3_class(fit, "bag")
})

test_that("bag can down-sample the classes and says so when printing", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  # an unbalanced outcome, so down-sampling has something to do
  set.seed(1637)
  unbalanced <- iris[c(1:10, 51:100, 101:150), ]
  fit <- bag(
    unbalanced[, 1:4],
    unbalanced$Species,
    B = 3,
    bagControl = bagControl(
      fit = ldaBag$fit,
      predict = ldaBag$pred,
      aggregate = ldaBag$aggregate,
      downSample = TRUE
    )
  )
  expect_true(fit$control$downSample)
  expect_snapshot(print(fit))
})

test_that("bag refuses to down-sample a numeric outcome", {
  skip_on_cran()
  skip_if_not_installed("party")

  set.seed(2896)
  expect_snapshot_warning(
    fit <- bag(
      mtcars[, -1],
      mtcars$mpg,
      B = 2,
      bagControl = bagControl(
        fit = ctreeBag$fit,
        predict = ctreeBag$pred,
        aggregate = ctreeBag$aggregate,
        downSample = TRUE
      )
    )
  )
  expect_false(fit$control$downSample)
})

test_that("bag rejects a non-positive number of variables", {
  skip_on_cran()
  expect_snapshot(
    bag(
      iris[, 1:4],
      iris$Species,
      B = 2,
      vars = 0,
      bagControl = bagControl(
        fit = ldaBag$fit,
        predict = ldaBag$pred,
        aggregate = ldaBag$aggregate
      )
    ),
    error = TRUE
  )
})

test_that("summary.bag reports out-of-bag statistics or their absence", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  set.seed(4130)
  with_oob <- bag(
    iris[, 1:4],
    iris$Species,
    B = 3,
    bagControl = bagControl(
      fit = ldaBag$fit,
      predict = ldaBag$pred,
      aggregate = ldaBag$aggregate,
      oob = TRUE
    )
  )
  smry <- summary(with_oob)
  expect_s3_class(smry, "summary.bag")
  # the accuracy statistics are resampled floats, so mask the numbers
  expect_snapshot(print(smry), transform = mask_decimals)

  set.seed(4130)
  without_oob <- bag(
    iris[, 1:4],
    iris$Species,
    B = 3,
    bagControl = bagControl(
      fit = ldaBag$fit,
      predict = ldaBag$pred,
      aggregate = ldaBag$aggregate,
      oob = FALSE
    )
  )
  no_stats <- summary(without_oob)
  expect_null(no_stats$oobStat)
  expect_snapshot(print(no_stats))
})
