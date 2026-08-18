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

test_that("bagFDA coerces a matrix outcome and accepts a formula", {
  skip_on_cran()
  skip_if_not_installed("mda")
  skip_if_not_installed("earth")

  x <- as.matrix(iris[, 1:4])

  # a one-column data frame outcome is reduced to its column and refactored
  set.seed(6274)
  fit <- suppressWarnings(bagFDA(x, iris[, "Species", drop = FALSE], B = 2))
  expect_s3_class(fit, "bagFDA")

  set.seed(6274)
  form <- suppressWarnings(bagFDA(Species ~ ., data = iris, B = 2))
  expect_s3_class(form, "bagFDA")
})

test_that("predict.bagFDA can use the out-of-bag training predictions", {
  skip_on_cran()
  skip_if_not_installed("mda")
  skip_if_not_installed("earth")

  set.seed(3959)
  fit <- suppressWarnings(bagFDA(iris[, 1:4], iris$Species, B = 3))

  # with no newdata the stored predictors are used
  oob <- suppressWarnings(predict(fit))
  expect_length(oob, nrow(iris))

  probs <- suppressWarnings(predict(fit, iris[, 1:4], type = "probs"))
  expect_identical(ncol(probs), 3L)
})

test_that("bagFDA.formula needs a formula", {
  skip_on_cran()
  skip_if_not_installed("mda")
  expect_snapshot(caret:::bagFDA.formula(iris[, 1:4]), error = TRUE)
})

test_that("predict.bagFDA pools out-of-bag predictions without stored x", {
  skip_on_cran()
  skip_if_not_installed("mda")
  skip_if_not_installed("earth")

  set.seed(1590)
  fit <- suppressWarnings(
    bagFDA(iris[, 1:4], iris$Species, B = 5, keepX = FALSE)
  )
  expect_null(fit$x)
  oob <- suppressWarnings(predict(fit))
  expect_true(length(oob) > 0)
})

test_that("predict.bagFDA tolerates repeated row names in newdata", {
  skip_on_cran()
  skip_if_not_installed("earth")
  skip_if_not_installed("mda")
  withr::local_package("plyr")

  dat <- engine_three_class()
  set.seed(4188)
  fit <- bagFDA(dat[, 1:4], dat$Species, B = 2)

  # as for plsda, predicting a bootstrap sample repeats rows
  boot <- dat[c(1, 1, 2, 16, 16, 31), 1:4]
  probs <- predict(fit, boot, type = "probs")
  expect_identical(nrow(probs), 6L)
  expect_length(predict(fit, boot, type = "class"), 6L)
})
