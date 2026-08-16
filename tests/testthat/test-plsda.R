# Tests for PLS discriminant analysis (plsda, R/plsda.R); its sparse variant
# splsda is tested in test-splsda.R. Fits need the pls package and the Bayes
# probability method needs klaR, so those paths are guarded. Fits are
# RNG-independent here, but the predictions are floats, so the tests assert on
# structure and snapshot only the deterministic print and error output.

test_that("plsda fits and predicts classes, probabilities and raw scores", {
  skip_on_cran()
  skip_if_not_installed("pls")

  set.seed(1)
  fit <- plsda(iris[, 1:4], iris$Species, ncomp = 2)
  expect_s3_class(fit, "plsda")
  expect_identical(fit$ncomp, 2)

  expect_length(predict(fit, iris[, 1:4]), nrow(iris))
  probs <- predict(fit, iris[, 1:4], type = "prob")
  expect_identical(dim(probs), c(150L, 3L, 1L))
  expect_identical(
    dim(predict(fit, iris[, 1:4], type = "raw")),
    c(150L, 3L, 1L)
  )

  # a single new row still returns one prediction
  expect_length(predict(fit, iris[1, 1:4, drop = FALSE]), 1)
})

test_that("plsda handles a single predictor and single-row prediction", {
  skip_on_cran()
  skip_if_not_installed("pls")

  # with one predictor, ncomp cannot exceed 1
  set.seed(1)
  fit <- plsda(iris[, 1, drop = FALSE], iris$Species, ncomp = 1)
  expect_length(predict(fit, iris[, 1, drop = FALSE]), nrow(iris))
  # a single new row returns a single prediction
  expect_length(predict(fit, iris[1, 1, drop = FALSE]), 1)
})

test_that("predict.plsda needs a number of components", {
  skip_on_cran()
  skip_if_not_installed("pls")

  set.seed(1)
  fit <- plsda(iris[, 1:4], iris$Species, ncomp = 2)
  fit$ncomp <- NULL
  expect_snapshot(predict(fit, iris[, 1:4]), error = TRUE)
})

test_that("plsda can use the Bayes probability method", {
  skip_on_cran()
  skip_if_not_installed("pls")
  skip_if_not_installed("klaR")

  set.seed(1)
  fit <- plsda(iris[, 1:4], iris$Species, ncomp = 2, probMethod = "Bayes")
  expect_length(predict(fit, iris[, 1:4], type = "class"), nrow(iris))
  expect_identical(
    dim(predict(fit, iris[, 1:4], type = "prob")),
    c(150L, 3L, 1L)
  )
})

test_that("print.plsda describes the model", {
  skip_on_cran()
  skip_if_not_installed("pls")

  set.seed(1)
  fit <- plsda(iris[, 1:4], iris$Species, ncomp = 2)
  expect_snapshot(print(fit))
})
