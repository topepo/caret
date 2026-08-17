# Tests for filterVarImp (R/filterVarImp.R): ROC-based importances for
# classification and lm/loess-based importances for regression.

test_that("filterVarImp scores two-class data with ROC curves", {
  x <- iris[51:150, 1:4]
  y <- factor(iris$Species[51:150])
  vi <- filterVarImp(x, y)
  expect_s3_class(vi, "data.frame")
  expect_named(vi, levels(y))
  # for two classes, both columns carry the same AUC
  expect_identical(vi[[1]], vi[[2]])
  expect_all_true(vi[[1]] >= 0.5)
})

test_that("filterVarImp scores multi-class data with pairwise ROC curves", {
  vi <- filterVarImp(iris[, 1:4], iris$Species)
  expect_s3_class(vi, "data.frame")
  expect_named(vi, levels(iris$Species))
  expect_identical(rownames(vi), colnames(iris[, 1:4]))
})

test_that("filterVarImp converts factor predictors to numeric", {
  x <- data.frame(a = iris$Sepal.Length, f = factor(rep(letters[1:5], 30)))
  vi <- filterVarImp(x, iris$Species)
  expect_identical(rownames(vi), c("a", "f"))
})

test_that("filterVarImp uses absolute t-statistics for parametric regression", {
  set.seed(6018)
  y <- rnorm(30)
  x <- data.frame(a = y + rnorm(30, sd = 0.2), b = rnorm(30))
  vi <- filterVarImp(x, y, nonpara = FALSE)
  expect_named(vi, "Overall")
  expect_identical(
    vi["a", "Overall"],
    abs(coef(summary(lm(y ~ x$a)))[2, "t value"])
  )
})

test_that("filterVarImp uses loess R-squared for nonparametric regression", {
  set.seed(2795)
  y <- rnorm(40)
  x <- data.frame(
    a = y + rnorm(40, sd = 0.2), # smooth relationship, loess path
    zv = rep(1, 40), # zero variance -> NA
    few = rep(1:3, length.out = 40) # < 20% unique values -> lm path
  )
  vi <- filterVarImp(x, y, nonpara = TRUE)
  expect_named(vi, "Overall")
  expect_gt(vi["a", "Overall"], 0.5)
  expect_true(is.na(vi["zv", "Overall"]))
  expect_gte(vi["few", "Overall"], 0)
})

test_that("filterVarImp falls back to lm when loess fails", {
  set.seed(1148)
  y <- rnorm(30)
  x <- data.frame(a = rnorm(30))
  # degree = 5 is rejected by loess but discarded by lm.fit, so the fallback
  # model provides the importance
  vi <- suppressWarnings(filterVarImp(x, y, nonpara = TRUE, degree = 5))
  expect_named(vi, "Overall")
  expect_false(is.na(vi["a", "Overall"]))
})

test_that("filterVarImp returns NA when both smoothers fail", {
  set.seed(9873)
  y <- rnorm(30)
  x <- data.frame(a = rnorm(30))
  # a character weights vector is rejected by both loess and lm
  vi <- filterVarImp(x, y, nonpara = TRUE, weights = "abc")
  expect_true(is.na(vi["a", "Overall"]))
})

test_that("filterVarImp clamps negative pseudo R-squared values to zero", {
  # the few-unique-values path fits a weighted lm; huge weights on two
  # reversed extremes force the unweighted residuals past the total sum of
  # squares, which would give a negative R-squared
  y <- c(rep(c(10, 0, -10), 9), -100, 0, 100)
  x <- data.frame(a = rep(1:3, 10))
  w <- c(rep(0.001, 27), 1000, 0.001, 1000)
  vi <- filterVarImp(x, y, nonpara = TRUE, weights = w)
  expect_identical(vi["a", "Overall"], 0)
})
