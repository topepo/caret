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

# ------------------------------------------------------------------------------
# plsda.default input handling

test_that("plsda takes an indicator matrix and validates the outcome", {
  skip_on_cran()
  skip_if_not_installed("pls")

  x <- as.matrix(iris[, 1:4])
  ind <- class2ind(iris$Species)

  fit <- plsda(x, ind, ncomp = 2)
  expect_identical(fit$obsLevels, colnames(ind))

  # a numeric vector is neither a factor nor an indicator matrix
  expect_snapshot(plsda(x, as.numeric(iris$Species), ncomp = 2), error = TRUE)

  unnamed <- ind
  colnames(unnamed) <- NULL
  expect_snapshot(plsda(x, unnamed, ncomp = 2), error = TRUE)

  not_one <- ind
  not_one[1, ] <- 0
  expect_snapshot(plsda(x, not_one, ncomp = 2), error = TRUE)
})

test_that("plsda warns about several ncomp values and unused priors", {
  skip_on_cran()
  skip_if_not_installed("pls")

  x <- as.matrix(iris[, 1:4])
  expect_snapshot_warning(fit <- plsda(x, iris$Species, ncomp = c(2, 3)))
  expect_equal(fit$ncomp, 3)

  expect_snapshot_warning(
    plsda(x, iris$Species, ncomp = 2, prior = c(0.2, 0.3, 0.5))
  )
})

test_that("print.plsda names the fitting algorithm and the probability rule", {
  skip_on_cran()
  skip_if_not_installed("pls")

  x <- as.matrix(iris[, 1:4])

  # each pls algorithm prints its own description
  for (m in c("kernelpls", "simpls", "oscorespls")) {
    fit <- plsda(x, iris$Species, ncomp = 2, method = m)
    expect_snapshot(print(fit))
  }

  # "svdpc" belongs to pls::pcr rather than pls::plsr, so plsda cannot fit it;
  # the print method only reads the field, so set it directly
  pcr_like <- plsda(x, iris$Species, ncomp = 2)
  pcr_like$method <- "svdpc"
  expect_snapshot(print(pcr_like))

  # an unknown algorithm is rejected
  bogus <- plsda(x, iris$Species, ncomp = 2)
  bogus$method <- "nope"
  expect_snapshot(print(bogus), error = TRUE)
})

test_that("print.plsda reports cross-validation and the Bayes rule", {
  skip_on_cran()
  skip_if_not_installed("pls")
  skip_if_not_installed("klaR")

  x <- as.matrix(iris[, 1:4])
  # pls's own cross-validation adds a validation element to the object
  cv_fit <- plsda(x, iris$Species, ncomp = 2, validation = "CV")
  expect_snapshot(print(cv_fit))

  bayes <- plsda(x, iris$Species, ncomp = 2, probMethod = "Bayes")
  expect_snapshot(print(bayes))
})

test_that("predict.plsda handles several components at once", {
  skip_on_cran()
  skip_if_not_installed("pls")

  x <- as.matrix(iris[, 1:4])
  fit <- plsda(x, iris$Species, ncomp = 3)

  # asking for more than one component returns a column per component
  cls <- predict(fit, x, ncomp = 1:3, type = "class")
  expect_identical(dim(cls), c(150L, 3L))

  probs <- predict(fit, x, ncomp = 1:3, type = "prob")
  expect_identical(dim(probs)[3], 3L)
})
