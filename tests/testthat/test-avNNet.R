# Tests for model-averaged neural networks (avNNet). The fits are RNG-dependent,
# so the integration tests assert on structure (class, dimensions) and snapshot
# only the deterministic print output; predictions are checked for shape.

test_that("avNNet fits an averaged classifier and predicts each type", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  set.seed(1)
  fit <- avNNet(
    iris[, 1:4],
    iris$Species,
    size = 3,
    repeats = 3,
    trace = FALSE,
    allowParallel = FALSE
  )

  expect_s3_class(fit, "avNNet")
  expect_identical(fit$repeats, 3)
  # class predictions: one per row
  expect_length(predict(fit, iris[, 1:4], type = "class"), nrow(iris))
  # probabilities: one column per class, one row per sample
  probs <- predict(fit, iris[, 1:4], type = "prob")
  expect_identical(colnames(probs), levels(iris$Species))
  expect_identical(dim(predict(fit, iris[, 1:4], type = "raw")), c(150L, 3L))
  # the print shows the network architecture, which is deterministic
  expect_snapshot(print(fit))
})

test_that("avNNet fits an averaged regression model", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  set.seed(1)
  dat <- SLC14_1(80)
  fit <- avNNet(
    dat[, 1:5],
    dat$y,
    size = 3,
    repeats = 3,
    linout = TRUE,
    trace = FALSE,
    allowParallel = FALSE
  )

  expect_s3_class(fit, "avNNet")
  expect_length(predict(fit, dat[, 1:5]), nrow(dat))
})

test_that("avNNet works with the formula interface", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  set.seed(1)
  fit <- avNNet(
    Species ~ .,
    data = iris,
    size = 3,
    repeats = 3,
    trace = FALSE,
    allowParallel = FALSE
  )

  expect_s3_class(fit, "avNNet.formula")
  expect_length(predict(fit, iris, type = "class"), nrow(iris))
})

test_that("predict.avNNet rejects objects of the wrong class", {
  expect_snapshot(
    caret:::predict.avNNet(structure(list(), class = "notAvNNet"), iris),
    error = TRUE
  )
})
