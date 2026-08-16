# Tests for the k-nearest-neighbour classification wrapper knn3 (R/knn3.R):
# the formula/matrix/data.frame interfaces, print, predict, and the low-level
# knn3Train engine. Its regression twin knnreg is tested in test-knnreg.R.

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

test_that("knn3 handles a single predictor and single-row prediction", {
  fit <- knn3(iris[, 1, drop = FALSE], iris$Species, k = 5)
  # one predictor: still one prediction per row
  expect_length(
    predict(fit, iris[, 1, drop = FALSE], type = "class"),
    nrow(iris)
  )
  # a single new row returns a single set of class probabilities
  expect_identical(
    nrow(predict(fit, iris[1, 1, drop = FALSE], type = "prob")),
    1L
  )
})
