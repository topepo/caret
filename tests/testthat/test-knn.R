# Tests for the k-nearest-neighbour wrappers: knn3 (classification, R/knn3.R)
# and its regression twin knnreg (R/knnreg.R). They share the same structure
# (formula/matrix/data.frame interfaces, print, predict, and a low-level
# *Train engine), so they are tested together here.

# --- knn3 (classification) --------------------------------------------------

test_that("knn3 fits from a formula, matrix and data frame", {
  expect_s3_class(knn3(Species ~ ., data = iris, k = 5), "knn3")
  expect_s3_class(knn3(as.matrix(iris[, 1:4]), iris$Species, k = 5), "knn3")
  expect_s3_class(knn3(iris[, 1:4], iris$Species, k = 5), "knn3")

  # the default method requires a matrix/data frame/formula
  expect_snapshot(knn3(iris$Sepal.Length), error = TRUE)
})

test_that("predict.knn3 returns probabilities and classes", {
  fit <- knn3(Species ~ ., data = iris, k = 5)
  expect_identical(fit$k, 5)

  probs <- predict(fit, iris[, 1:4], type = "prob")
  expect_identical(dim(probs), c(150L, 3L))
  expect_identical(colnames(probs), levels(iris$Species))
  # each row of probabilities sums to exactly 1 (neighbour-count fractions)
  expect_identical(unname(rowSums(probs)), rep(1, nrow(iris)))

  cls <- predict(fit, iris[, 1:4], type = "class")
  expect_length(cls, nrow(iris))
  expect_identical(levels(cls), levels(iris$Species))
})

test_that("knn3Train predicts held-out cases directly", {
  # a clean split of iris: train on 100, predict the other 50
  train_x <- iris[1:100, 1:4]
  test_x <- iris[101:150, 1:4]
  train_y <- iris$Species[1:100]
  out <- knn3Train(train_x, test_x, train_y, k = 5)
  expect_length(out, 50)
})

test_that("print.knn3 shows the neighbour count and class distribution", {
  expect_snapshot(print(knn3(Species ~ ., data = iris, k = 5)))
})

# --- knnreg (regression) ----------------------------------------------------

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
