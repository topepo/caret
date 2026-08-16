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
