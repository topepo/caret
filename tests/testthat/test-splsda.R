# Tests for sparse PLS discriminant analysis (splsda, R/splsda.R). The dense
# variant plsda is tested in test-plsda.R.
#
# The spls package exports its own splsda(), predict.splsda() and
# print.splsda(), and because caret's splsda() requires spls, spls is always
# loaded by the time one of these objects exists - so spls's S3 methods win the
# registration and shadow caret's. That is why the tests below call
# `caret:::predict.splsda()` and `caret:::print.splsda()` explicitly rather
# than through predict()/print(): the generics would dispatch to spls.
#
# Fits are deterministic for a fixed seed but the coefficients are floats, so
# the tests assert on structure and snapshot only the printed metadata.

test_that("splsda fits a sparse PLS discriminant model", {
  skip_on_cran()
  # the skip loads the spls namespace, which prints a note about re-registering
  # caret's splsda S3 methods
  suppressMessages(skip_if_not_installed("spls"))

  set.seed(1)
  fit <- caret:::splsda(as.matrix(iris[, 1:4]), iris$Species, K = 2, eta = 0.5)
  expect_s3_class(fit, "splsda")
  expect_length(
    caret:::predict.splsda(fit, as.matrix(iris[, 1:4])),
    nrow(iris)
  )
})

test_that("splsda accepts a data frame and records the class levels", {
  skip_on_cran()
  suppressMessages(skip_if_not_installed("spls"))

  set.seed(6903)
  fit <- caret:::splsda(iris[, 1:4], iris$Species, K = 2, eta = 0.5)
  expect_s3_class(fit, "splsda")
  expect_identical(fit$obsLevels, levels(iris$Species))
  expect_identical(fit$probMethod, "softmax")
  # softmax needs no auxiliary probability model
  expect_null(fit$probModel)
})

test_that("splsda takes an indicator matrix as the outcome", {
  skip_on_cran()
  suppressMessages(skip_if_not_installed("spls"))

  ind <- class2ind(iris$Species)
  set.seed(4477)
  fit <- caret:::splsda(as.matrix(iris[, 1:4]), ind, K = 2, eta = 0.5)
  # the levels come from the indicator column names
  expect_identical(fit$obsLevels, colnames(ind))
})

test_that("splsda validates its outcome", {
  skip_on_cran()
  suppressMessages(skip_if_not_installed("spls"))

  x <- as.matrix(iris[, 1:4])

  # a numeric vector is neither a factor nor an indicator matrix
  expect_snapshot(
    caret:::splsda(x, as.numeric(iris$Species), K = 2, eta = 0.5),
    error = TRUE
  )

  # an indicator matrix has to say which column is which class
  unnamed <- class2ind(iris$Species)
  colnames(unnamed) <- NULL
  expect_snapshot(caret:::splsda(x, unnamed, K = 2, eta = 0.5), error = TRUE)

  # and every row has to belong to exactly one class
  not_one <- class2ind(iris$Species)
  not_one[1, ] <- 0
  expect_snapshot(caret:::splsda(x, not_one, K = 2, eta = 0.5), error = TRUE)
})

test_that("splsda ignores priors unless the Bayes method is used", {
  skip_on_cran()
  suppressMessages(skip_if_not_installed("spls"))

  set.seed(2274)
  expect_snapshot_warning(
    fit <- caret:::splsda(
      as.matrix(iris[, 1:4]),
      iris$Species,
      K = 2,
      eta = 0.5,
      prior = c(0.2, 0.3, 0.5)
    )
  )
  expect_s3_class(fit, "splsda")
})

test_that("splsda can compute class probabilities with Bayes rule", {
  skip_on_cran()
  suppressMessages(skip_if_not_installed("spls"))
  skip_if_not_installed("klaR")

  set.seed(8158)
  fit <- caret:::splsda(
    as.matrix(iris[, 1:4]),
    iris$Species,
    K = 2,
    eta = 0.5,
    probMethod = "Bayes"
  )
  expect_identical(fit$probMethod, "Bayes")
  expect_s3_class(fit$probModel, "NaiveBayes")
})

# --- predict -------------------------------------------------------------------

test_that("predict.splsda returns classes, probabilities and raw scores", {
  skip_on_cran()
  suppressMessages(skip_if_not_installed("spls"))

  x <- as.matrix(iris[, 1:4])
  set.seed(9236)
  fit <- caret:::splsda(x, iris$Species, K = 2, eta = 0.5)

  cls <- caret:::predict.splsda(fit, x)
  expect_s3_class(cls, "factor")
  expect_identical(levels(cls), levels(iris$Species))

  # softmax probabilities sum to one for every row
  probs <- caret:::predict.splsda(fit, x, type = "prob")
  expect_identical(dim(probs), c(150L, 3L))
  expect_equal(unname(rowSums(probs)), rep(1, 150))

  # the raw scores are the underlying spls predictions
  raw <- caret:::predict.splsda(fit, x, type = "raw")
  expect_identical(dim(raw), c(150L, 3L))
})

test_that("predict.splsda uses the Bayes model when one was fit", {
  skip_on_cran()
  suppressMessages(skip_if_not_installed("spls"))
  skip_if_not_installed("klaR")

  x <- as.matrix(iris[, 1:4])
  set.seed(3520)
  fit <- caret:::splsda(x, iris$Species, K = 2, eta = 0.5, probMethod = "Bayes")

  cls <- suppressWarnings(caret:::predict.splsda(fit, x))
  expect_s3_class(cls, "factor")

  probs <- suppressWarnings(caret:::predict.splsda(fit, x, type = "prob"))
  expect_identical(ncol(probs), 3L)
})

# --- print ---------------------------------------------------------------------

test_that("print.splsda reports the parameters and selected variables", {
  skip_on_cran()
  suppressMessages(skip_if_not_installed("spls"))

  set.seed(1)
  fit <- caret:::splsda(as.matrix(iris[, 1:4]), iris$Species, K = 2, eta = 0.5)
  expect_snapshot(caret:::print.splsda(fit))
})

test_that("print.splsda names the Bayes probability method", {
  skip_on_cran()
  suppressMessages(skip_if_not_installed("spls"))
  skip_if_not_installed("klaR")

  set.seed(1)
  fit <- caret:::splsda(
    as.matrix(iris[, 1:4]),
    iris$Species,
    K = 2,
    eta = 0.5,
    probMethod = "Bayes"
  )
  expect_snapshot(caret:::print.splsda(fit))
})

test_that("print.splsda handles a single outcome column", {
  skip_on_cran()
  suppressMessages(skip_if_not_installed("spls"))

  # spls cannot fit a one-column outcome, so shrink a fitted object instead;
  # the print method only reads these fields. With one column it leaves out
  # the kappa parameter.
  set.seed(1)
  fit <- caret:::splsda(as.matrix(iris[, 1:4]), iris$Species, K = 2, eta = 0.5)
  fit$y <- fit$y[, 1, drop = FALSE]
  expect_snapshot(caret:::print.splsda(fit))
})

test_that("print.splsda falls back to variable positions without names", {
  skip_on_cran()
  suppressMessages(skip_if_not_installed("spls"))

  # spls always labels its columns, so drop the names on a fitted object to
  # reach the positional branch
  set.seed(1)
  fit <- caret:::splsda(as.matrix(iris[, 1:4]), iris$Species, K = 2, eta = 0.5)
  colnames(fit$x) <- NULL
  expect_snapshot(caret:::print.splsda(fit))
})
