# Tests for the k-nearest-neighbour regression wrapper knnreg (R/knnreg.R):
# the formula/matrix/data.frame interfaces, print, predict, and the low-level
# knnregTrain engine. Its classification twin knn3 is tested in test-knn3.R.

test_that("knnreg fits from a formula, matrix and data frame", {
  expect_s3_class(knnreg(mpg ~ ., data = mtcars, k = 5), "knnreg")
  expect_s3_class(knnreg(as.matrix(mtcars[, -1]), mtcars$mpg, k = 5), "knnreg")
  expect_s3_class(knnreg(mtcars[, -1], mtcars$mpg, k = 5), "knnreg")

  # the default method needs a matrix/data frame/formula
  expect_snapshot(knnreg(mtcars$mpg), error = TRUE)
})

test_that("predict.knnreg returns one numeric prediction per row", {
  fit <- knnreg(mpg ~ ., data = mtcars, k = 5)
  expect_identical(fit$k, 5)

  preds <- predict(fit, mtcars[, -1])
  expect_length(preds, nrow(mtcars))
  expect_type(preds, "double")
})

test_that("knnregTrain predicts held-out cases directly", {
  train_x <- as.matrix(mtcars[1:20, -1])
  test_x <- as.matrix(mtcars[21:32, -1])
  out <- caret:::knnregTrain(train_x, test_x, mtcars$mpg[1:20], k = 5)
  expect_length(out, 12)
  expect_type(out, "double")
})

test_that("print.knnreg identifies the model", {
  expect_snapshot(print(knnreg(mpg ~ ., data = mtcars, k = 5)))
})

test_that("knnreg handles a single predictor and single-row prediction", {
  fit <- knnreg(mtcars[, "hp", drop = FALSE], mtcars$mpg, k = 5)
  # one predictor: still one prediction per row
  expect_length(predict(fit, mtcars[, "hp", drop = FALSE]), nrow(mtcars))
  # a single new row returns a single prediction
  expect_length(predict(fit, mtcars[1, "hp", drop = FALSE]), 1)
})

# ------------------------------------------------------------------------------
# The C engine (src/caret.c). knnregTrain() reaches the regression half of the
# neighbour search, whose tie handling mirrors the classification code.

test_that("knnregTrain averages every tied neighbour when use.all is TRUE", {
  # the origin is 1 away from rows 2 and 3 and 5 away from row 4
  train <- matrix(c(0, 0, 1, 0, -1, 0, 0, 5), ncol = 2, byrow = TRUE)
  y <- c(1, 2, 3, 4)
  test <- matrix(c(0, 0), ncol = 2)

  # rows 1, 2 and 3 are all within the third neighbour's distance
  expect_equal(caret:::knnregTrain(train, test, y, k = 3, use.all = TRUE), 2)
})

test_that("knnregTrain samples among tied neighbours when use.all is FALSE", {
  train <- matrix(c(0, 0, 1, 0, -1, 0, 0, 5), ncol = 2, byrow = TRUE)
  y <- c(1, 2, 3, 4)
  test <- matrix(c(0, 0), ncol = 2)

  set.seed(6)
  out <- caret:::knnregTrain(train, test, y, k = 3, use.all = FALSE)
  expect_length(out, 1)
  expect_true(is.finite(out))
})

test_that("knnregTrain handles distinct distances with use.all FALSE", {
  train <- matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE)
  y <- c(1, 2, 3, 4)
  test <- matrix(c(0, 0), ncol = 2)

  # the two nearest are rows 1 and 2
  expect_equal(caret:::knnregTrain(train, test, y, k = 2, use.all = FALSE), 1.5)
})

test_that("knnregTrain stops when there are too many tied neighbours", {
  train <- matrix(rep(0, 2002), ncol = 2)
  y <- seq_len(1001)
  test <- matrix(c(0, 0), ncol = 2)

  expect_snapshot(caret:::knnregTrain(train, test, y, k = 1), error = TRUE)
})

test_that("knnregTrain validates its arguments", {
  train <- matrix(c(0, 0, 1, 1, 2, 2), ncol = 2, byrow = TRUE)
  y <- c(1, 2, 3)
  test <- matrix(c(0, 0), ncol = 2)

  expect_snapshot_warning(caret:::knnregTrain(train, test, y, k = 10))
  expect_snapshot(caret:::knnregTrain(train, test, y, k = 0), error = TRUE)
  expect_snapshot(caret:::knnregTrain(train, test, y[1:2], k = 1), error = TRUE)
  expect_snapshot(
    caret:::knnregTrain(train, test[, 1, drop = FALSE], y, k = 1),
    error = TRUE
  )
  expect_snapshot(
    caret:::knnregTrain(train, test, c(1, 2, NA), k = 1),
    error = TRUE
  )
  # a bare vector of test values is treated as a single row
  expect_length(caret:::knnregTrain(train, c(0, 0), y, k = 1), 1)
})

test_that("the C regression engine can leave each training point out", {
  # knnregTrain always passes cv = FALSE, so this branch needs a direct call
  train <- matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE)
  y <- c(1, 2, 3, 4)

  Z <- .C(
    "knn3reg",
    as.integer(1),
    as.integer(nrow(train)),
    as.integer(nrow(train)),
    as.integer(ncol(train)),
    as.double(train),
    as.double(y),
    as.double(train),
    double(nrow(train)),
    as.integer(TRUE),
    as.integer(TRUE),
    PACKAGE = "caret"
  )
  # each point is predicted from its neighbours only: the end points take
  # their single neighbour's value, the middle two average the pair that ties
  expect_equal(Z[[8]], c(2, 2, 3, 3))
})

test_that("knnregTrain subsamples when more neighbours tie than k allows", {
  # all four rows sit exactly one unit from the test point, so the tie list
  # grows past k and the extras are subsampled down to k
  train <- matrix(c(1, 0, -1, 0, 0, 1, 0, -1), ncol = 2, byrow = TRUE)
  y <- c(1, 2, 3, 4)
  test <- matrix(c(0, 0), ncol = 2)

  set.seed(9)
  out <- caret:::knnregTrain(train, test, y, k = 2, use.all = FALSE)
  expect_length(out, 1)
  # the result is the mean of two of the four tied outcomes
  expect_gte(out, 1.5)
  expect_lte(out, 3.5)
})

# ------------------------------------------------------------------------------
# formula, matrix and predict branches

test_that("knnreg.formula rejects a malformed formula", {
  expect_snapshot(knnreg(~., data = mtcars), error = TRUE)
})

test_that("knnreg.formula accepts a matrix as the data argument", {
  fit <- knnreg(mpg ~ ., data = as.matrix(mtcars), k = 5)
  expect_s3_class(fit, "knnreg")
})

test_that("knnreg.formula records the na.action that dropped rows", {
  with_na <- mtcars
  with_na$hp[c(2, 5)] <- NA
  fit <- knnreg(mpg ~ ., data = with_na, k = 5, na.action = na.omit)
  expect_false(is.null(fit$na.action))
})

test_that("knnreg needs a numeric outcome", {
  expect_snapshot(
    knnreg(as.matrix(iris[, 1:4]), iris$Species),
    error = TRUE
  )
  expect_snapshot(knnreg(iris[, 1:4], iris$Species), error = TRUE)
})

test_that("predict.knnreg checks the object and can reuse the model frame", {
  expect_snapshot(
    caret:::predict.knnreg(structure(list(), class = "nope")),
    error = TRUE
  )

  # with no newdata the model frame is rebuilt from the formula, so its
  # variables have to be reachable from the formula's environment
  dat <- mtcars[, c("mpg", "hp", "wt")]
  mpg <- dat$mpg
  hp <- dat$hp
  wt <- dat$wt
  fit <- knnreg(mpg ~ hp + wt, data = dat, k = 5)
  expect_length(predict(fit), nrow(mtcars))
})
