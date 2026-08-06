# Tests for bagged FDA (bagFDA). The model needs earth + mda; a small number of
# bootstrap samples (B) keeps the fits fast. Fits are RNG-dependent, so the
# tests assert on structure and snapshot only the deterministic print output.

test_that("bagFDA fits a classifier and predicts classes and probabilities", {
  skip_on_cran()
  skip_if_not_installed("earth")
  skip_if_not_installed("mda")

  set.seed(1)
  fit <- bagFDA(iris[, 1:4], iris$Species, B = 3)

  expect_s3_class(fit, "bagFDA")
  expect_identical(fit$B, 3)
  expect_identical(fit$levels, levels(iris$Species))

  # class predictions: one per row
  expect_length(predict(fit, iris[, 1:4]), nrow(iris))
  # probabilities: one column per class
  probs <- predict(fit, iris[, 1:4], type = "probs")
  expect_s3_class(probs, "data.frame")
  expect_identical(dim(probs), c(150L, 3L))
})

test_that("bagFDA works with the formula interface", {
  skip_on_cran()
  skip_if_not_installed("earth")
  skip_if_not_installed("mda")

  set.seed(1)
  fit <- bagFDA(Species ~ ., data = iris, B = 3)

  expect_s3_class(fit, "bagFDA")
  expect_length(predict(fit, iris), nrow(iris))
})

test_that("print.bagFDA summarises the model", {
  skip_on_cran()
  skip_if_not_installed("earth")
  skip_if_not_installed("mda")

  set.seed(1)
  fit <- bagFDA(iris[, 1:4], iris$Species, B = 3)
  # the print reports only structural counts, so it is deterministic
  expect_snapshot(print(fit))
})

test_that("summary.bagFDA reports out-of-bag and model statistics", {
  skip_on_cran()
  skip_if_not_installed("earth")
  skip_if_not_installed("mda")

  set.seed(1)
  fit <- bagFDA(iris[, 1:4], iris$Species, B = 3)
  sm <- summary(fit)
  expect_s3_class(sm, "summary.bagFDA")
  # seeded, so the summary print is reproducible in CI
  expect_snapshot(print(sm))
})

test_that("bagFDA accepts case weights and can drop the stored x", {
  skip_on_cran()
  skip_if_not_installed("earth")
  skip_if_not_installed("mda")

  set.seed(1)
  fit <- bagFDA(
    iris[, 1:4],
    iris$Species,
    B = 3,
    weights = runif(nrow(iris)),
    keepX = FALSE
  )
  expect_s3_class(fit, "bagFDA")
  # keepX = FALSE drops the training predictors from the object
  expect_null(fit$x)
})
