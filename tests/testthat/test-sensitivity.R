# Fixture data (sens_lvs, sens_truth, sens_pred, sens_xtab, sens_mtab) lives in
# helper-2x2-tables.R

# --- sensitivity ------------------------------------------------------------

test_that("sensitivity works for factors, tables and matrices", {
  expect_equal(sensitivity(sens_pred, sens_truth), 231 / 258)
  expect_equal(sensitivity(sens_xtab), 231 / 258)
  expect_equal(sensitivity(sens_mtab), 231 / 258)
  # the positive class can be chosen explicitly
  expect_equal(sensitivity(sens_pred, sens_truth, positive = "normal"), 54 / 86)
})

test_that("sensitivity errors on bad input", {
  expect_snapshot(
    sensitivity(as.character(sens_pred), sens_truth),
    error = TRUE
  )
  expect_snapshot(
    sensitivity(factor(letters[1:3]), factor(letters[1:3])),
    error = TRUE
  )
  # a non-square table is rejected
  expect_snapshot(sensitivity(as.table(matrix(1:6, nrow = 2))), error = TRUE)
})

test_that("sensitivity handles NAs and empty denominators", {
  p <- sens_pred
  p[1:5] <- NA
  # na.rm = TRUE (default) drops incomplete cases without error
  expect_false(is.na(sensitivity(p, sens_truth)))
  # no positive cases -> NA
  no_pos <- factor(rep("normal", 10), levels = rev(sens_lvs))
  expect_true(is.na(sensitivity(no_pos, no_pos, positive = "abnormal")))
})

test_that("sensitivity.table collapses multiclass tables", {
  data(iris)
  irisTabs <- table(iris$Species, iris$Species)
  # a perfect confusion table has sensitivity 1 for any class
  expect_equal(sensitivity(irisTabs, "versicolor"), 1)
})

# --- specificity ------------------------------------------------------------

test_that("specificity works for factors, tables and matrices", {
  expect_equal(specificity(sens_pred, sens_truth), 54 / 86)
  expect_equal(specificity(sens_xtab), 54 / 86)
  expect_equal(specificity(sens_mtab), 54 / 86)
})

test_that("specificity errors on bad input", {
  expect_snapshot(
    specificity(as.character(sens_pred), sens_truth),
    error = TRUE
  )
  expect_snapshot(
    specificity(factor(letters[1:3]), factor(letters[1:3])),
    error = TRUE
  )
  expect_snapshot(specificity(as.table(matrix(1:6, nrow = 2))), error = TRUE)
})

test_that("specificity handles empty denominators and multiclass tables", {
  no_neg <- factor(rep("abnormal", 10), levels = rev(sens_lvs))
  expect_true(is.na(specificity(no_neg, no_neg, negative = "normal")))
  irisTabs <- table(iris$Species, iris$Species)
  expect_equal(specificity(irisTabs, c("setosa", "virginica")), 1)
})

# --- posPredValue -----------------------------------------------------------

test_that("posPredValue matches the sensitivity/specificity identity", {
  sens <- sensitivity(sens_pred, sens_truth)
  spec <- specificity(sens_pred, sens_truth)
  prev <- mean(sens_truth == "abnormal")
  expected <- (sens * prev) / ((sens * prev) + ((1 - spec) * (1 - prev)))
  expect_equal(posPredValue(sens_pred, sens_truth), expected)
  expect_equal(posPredValue(sens_xtab), expected)
  expect_equal(posPredValue(sens_mtab), expected)
})

test_that("posPredValue respects a supplied prevalence", {
  ppv_25 <- posPredValue(sens_pred, sens_truth, prevalence = 0.25)
  expect_gte(ppv_25, 0)
  expect_lte(ppv_25, 1)
  # table method also accepts prevalence
  expect_equal(
    posPredValue(sens_xtab, prevalence = 0.25),
    posPredValue(sens_pred, sens_truth, prevalence = 0.25)
  )
})

test_that("posPredValue collapses multiclass tables", {
  irisTabs <- table(iris$Species, iris$Species)
  expect_equal(posPredValue(irisTabs, "versicolor"), 1)
})

test_that("posPredValue errors on bad input", {
  expect_snapshot(
    posPredValue(as.character(sens_pred), sens_truth),
    error = TRUE
  )
  expect_snapshot(
    posPredValue(factor(letters[1:3]), factor(letters[1:3])),
    error = TRUE
  )
})

# --- negPredValue -----------------------------------------------------------

test_that("negPredValue matches the sensitivity/specificity identity", {
  sens <- sensitivity(sens_pred, sens_truth)
  spec <- specificity(sens_pred, sens_truth)
  prev <- mean(sens_truth == "abnormal")
  expected <- (spec * (1 - prev)) /
    (((1 - sens) * prev) + (spec * (1 - prev)))
  expect_equal(negPredValue(sens_pred, sens_truth), expected)
  expect_equal(negPredValue(sens_xtab), expected)
  expect_equal(negPredValue(sens_mtab), expected)
})

test_that("negPredValue respects a supplied prevalence", {
  npv_25 <- negPredValue(sens_pred, sens_truth, prevalence = 0.25)
  expect_gte(npv_25, 0)
  expect_lte(npv_25, 1)
  expect_equal(
    negPredValue(sens_xtab, prevalence = 0.25),
    negPredValue(sens_pred, sens_truth, prevalence = 0.25)
  )
})

test_that("negPredValue collapses multiclass tables", {
  irisTabs <- table(iris$Species, iris$Species)
  expect_equal(negPredValue(irisTabs, negative = "versicolor"), 1)
})

test_that("negPredValue errors on bad input", {
  expect_snapshot(
    negPredValue(as.character(sens_pred), sens_truth),
    error = TRUE
  )
  expect_snapshot(
    negPredValue(factor(letters[1:3]), factor(letters[1:3])),
    error = TRUE
  )
})

# --- table-shape validation (shared by all four metric families) --------------

test_that("the table methods reject malformed tables with clear errors", {
  bad_names <- matrix(1:4, 2, dimnames = list(c("a", "b"), c("b", "a")))

  expect_snapshot(sensitivity(as.table(bad_names)), error = TRUE)
  expect_snapshot(specificity(as.table(bad_names)), error = TRUE)
  expect_snapshot(posPredValue(as.table(bad_names)), error = TRUE)
  expect_snapshot(negPredValue(as.table(bad_names)), error = TRUE)
  expect_snapshot(
    posPredValue(as.table(matrix(1:6, nrow = 2))),
    error = TRUE
  )
  expect_snapshot(
    negPredValue(as.table(matrix(1:6, nrow = 2))),
    error = TRUE
  )
})

test_that("specificity drops incomplete cases when na.rm = TRUE", {
  p <- sens_pred
  p[1:5] <- NA
  expect_false(is.na(specificity(p, sens_truth)))
})

test_that("missing values give NA when na.rm = FALSE", {
  p <- sens_pred
  p[1] <- NA
  expect_true(is.na(sensitivity(p, sens_truth, na.rm = FALSE)))
  expect_true(is.na(specificity(p, sens_truth, na.rm = FALSE)))
})

test_that("the matrix methods use the positive and negative arguments", {
  m <- matrix(c(10, 3, 4, 20), 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_equal(sensitivity(m, positive = "b"), 20 / 24)
  expect_equal(specificity(m, negative = "a"), 10 / 13)
  expect_equal(
    posPredValue(m, positive = "b"),
    posPredValue(as.table(m), positive = "b")
  )
  expect_equal(
    negPredValue(m, negative = "a", prevalence = 0.4),
    negPredValue(as.table(m), negative = "a", prevalence = 0.4)
  )
})

test_that("the metrics agree with yardstick", {
  set.seed(4472)
  lv <- c("x", "y")
  truth <- factor(sample(lv, 50, replace = TRUE), levels = lv)
  est <- factor(sample(lv, 50, replace = TRUE), levels = lv)
  expect_equal(sensitivity(est, truth), yardstick::sens_vec(truth, est))
  expect_equal(specificity(est, truth), yardstick::spec_vec(truth, est))
  expect_equal(
    posPredValue(est, truth, prevalence = 0.3),
    yardstick::ppv_vec(truth, est, prevalence = 0.3)
  )
  expect_equal(
    negPredValue(est, truth, negative = "x"),
    yardstick::npv_vec(truth, est, event_level = "second")
  )
})
