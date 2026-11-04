# Tests for pcaNNet (PCA pre-processing followed by a neural network). Fits are
# RNG-dependent, so the tests assert on structure and prediction shape and
# snapshot only the deterministic print output.

test_that("pcaNNet fits a classifier and predicts each type", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  set.seed(1)
  fit <- pcaNNet(iris[, 1:4], iris$Species, size = 3, trace = FALSE)

  expect_s3_class(fit, "pcaNNet")
  expect_length(predict(fit, iris[, 1:4], type = "class"), nrow(iris))
  probs <- predict(fit, iris[, 1:4], type = "prob")
  expect_identical(colnames(probs), levels(iris$Species))
  expect_identical(dim(predict(fit, iris[, 1:4], type = "raw")), c(150L, 3L))
})

test_that("pcaNNet works with the formula interface", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  set.seed(1)
  fit <- pcaNNet(Species ~ ., data = iris, size = 3, trace = FALSE)
  expect_s3_class(fit, "pcaNNet.formula")
  expect_length(predict(fit, iris, type = "class"), nrow(iris))
})

test_that("pcaNNet fits a regression model", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  set.seed(1)
  dat <- SLC14_1(80)
  fit <- pcaNNet(dat[, 1:5], dat$y, size = 3, linout = TRUE, trace = FALSE)
  expect_s3_class(fit, "pcaNNet")
  expect_length(predict(fit, dat[, 1:5]), nrow(dat))
})

test_that("print.pcaNNet reports the PCA and network structure", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  set.seed(1)
  fit <- pcaNNet(iris[, 1:4], iris$Species, size = 3, trace = FALSE)
  expect_snapshot(print(fit))
})

test_that("predict.pcaNNet rejects objects of the wrong class", {
  expect_snapshot(
    caret:::predict.pcaNNet(structure(list(), class = "notPcaNNet"), iris),
    error = TRUE
  )
})

test_that("pcaNNet handles a single predictor and single-row prediction", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  # a single predictor drops the PCA step (a group transform needs >= 2 cols),
  # but the network still fits and predicts
  set.seed(1)
  fit <- suppressWarnings(
    pcaNNet(iris[, 1, drop = FALSE], iris$Species, size = 3, trace = FALSE)
  )
  expect_length(
    predict(fit, iris[, 1, drop = FALSE], type = "class"),
    nrow(iris)
  )

  # a single new row returns a single set of class probabilities
  set.seed(1)
  full <- pcaNNet(iris[, 1:4], iris$Species, size = 3, trace = FALSE)
  expect_identical(
    nrow(predict(full, iris[1, 1:4, drop = FALSE], type = "prob")),
    1L
  )
})

test_that("predict.pcaNNet can use the stored fitted values", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  set.seed(7205)
  reg <- suppressWarnings(pcaNNet(
    mtcars[, -1],
    mtcars$mpg,
    size = 2,
    linout = TRUE,
    trace = FALSE,
    maxit = 20
  ))
  expect_length(predict(reg), nrow(mtcars))

  set.seed(7205)
  cls <- suppressWarnings(pcaNNet(
    iris[, 1:4],
    iris$Species,
    size = 2,
    trace = FALSE,
    maxit = 20
  ))
  # the default type is "raw", giving the class scores
  expect_identical(dim(predict(cls)), c(150L, 3L))
  expect_s3_class(predict(cls, type = "class"), "factor")
})

test_that("predict.pcaNNet checks the object's class", {
  expect_snapshot(
    caret:::predict.pcaNNet(structure(list(), class = "nope")),
    error = TRUE
  )
})

test_that("pcaNNet drops zero-variance predictors before the PCA", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  dat <- iris[, 1:4]
  dat$flat <- 1

  set.seed(8846)
  fit <- suppressWarnings(pcaNNet(
    dat,
    iris$Species,
    size = 2,
    trace = FALSE,
    maxit = 20
  ))
  # the constant column is excluded, and the retained names are stored so
  # predict() can subset newdata the same way
  expect_identical(fit$names, colnames(iris)[1:4])
  expect_identical(nrow(fit$pc$rotation), 4L)
  # with newdata the class predictions come back as characters
  expect_type(predict(fit, dat, type = "class"), "character")
  expect_identical(dim(predict(fit, dat, type = "prob")), c(150L, 3L))
})
