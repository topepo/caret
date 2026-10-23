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

test_that("filterVarImp ROC values are unchanged by the move to yardstick", {
  # expected values were computed with ModelMetrics::auc() before it was
  # replaced by yardstick::roc_auc_vec()
  set.seed(8361)
  n <- 150
  x <- data.frame(
    a = rnorm(n),
    b = runif(n),
    c = factor(sample(letters[1:3], n, TRUE))
  )
  y2 <- factor(ifelse(x$a + rnorm(n) > 0, "yes", "no"))
  y3 <- factor(sample(c("u", "v", "w"), n, TRUE))

  vi2 <- filterVarImp(x, y2)
  expect_equal(
    vi2$no,
    c(0.855318372559752, 0.569968983762087, 0.536763364349571)
  )

  vi3 <- filterVarImp(x, y3)
  expect_equal(
    vi3$u,
    c(0.556818181818182, 0.573181818181818, 0.611818181818182)
  )
  expect_equal(
    vi3$v,
    c(0.541487603305785, 0.551363636363636, 0.603181818181818)
  )
})

test_that("filterVarImp gives NA for a class pair with an empty class", {
  y <- factor(rep(c("a", "b"), each = 10), levels = c("a", "b", "c"))
  x <- data.frame(p = seq_len(20), q = rev(seq_len(20)))
  expect_no_warning(vi <- filterVarImp(x, y))
  expect_all_true(is.na(vi$c))
})

test_that("filterVarImp works with one predictor and more than two classes", {
  vi <- filterVarImp(iris[, 1, drop = FALSE], iris$Species)
  expect_named(vi, levels(iris$Species))
  expect_identical(rownames(vi), "Sepal.Length")
  expect_equal(vi, filterVarImp(iris[, 1:2], iris$Species)[1, ])
})

test_that("filterVarImp uses every class pair for four or more classes", {
  set.seed(6431)
  cl <- letters[1:4]
  y <- factor(rep(cl, each = 25))
  x <- data.frame(p = rnorm(100) + as.integer(y), q = rnorm(100))
  vi <- filterVarImp(x, y)

  pair_auc <- function(pred, a, b) {
    keep <- y %in% c(a, b)
    auc <- yardstick::roc_auc_vec(factor(y[keep], levels = c(a, b)), pred[keep])
    max(auc, 1 - auc)
  }
  for (k in cl) {
    others <- setdiff(cl, k)
    expected <- max(vapply(others, function(o) pair_auc(x$p, k, o), numeric(1)))
    expect_equal(vi["p", k], expected)
  }
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
