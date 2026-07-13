# Fixture data (sens_lvs, sens_truth, sens_pred, sens_xtab, sens_mtab) lives in
# helper-sensitivity.R

# --- sensitivity ------------------------------------------------------------

test_that("sensitivity works for factors, tables and matrices", {
  expect_equal(sensitivity(sens_pred, sens_truth), 231 / 258)
  expect_equal(sensitivity(sens_xtab), 231 / 258)
  expect_equal(sensitivity(sens_mtab), 231 / 258)
  # the positive class can be chosen explicitly
  expect_equal(sensitivity(sens_pred, sens_truth, positive = "normal"), 54 / 86)
})

test_that("sensitivity errors on bad input", {
  expect_error(
    sensitivity(as.character(sens_pred), sens_truth),
    "must be factors"
  )
  expect_error(
    sensitivity(factor(letters[1:3]), factor(letters[1:3])),
    "same two levels"
  )
  # a non-square table is rejected
  expect_error(sensitivity(as.table(matrix(1:6, nrow = 2))))
})

test_that("sensitivity handles NAs and empty denominators", {
  p <- sens_pred
  p[1:5] <- NA
  # na.rm = TRUE (default) drops incomplete cases without error
  expect_true(!is.na(sensitivity(p, sens_truth)))
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
  expect_error(
    specificity(as.character(sens_pred), sens_truth),
    "must be a factor"
  )
  expect_error(
    specificity(factor(letters[1:3]), factor(letters[1:3])),
    "same two levels"
  )
  expect_error(specificity(as.table(matrix(1:6, nrow = 2))))
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
  expect_true(ppv_25 >= 0 && ppv_25 <= 1)
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
  expect_error(
    posPredValue(as.character(sens_pred), sens_truth),
    "must be factors"
  )
  expect_error(
    posPredValue(factor(letters[1:3]), factor(letters[1:3])),
    "same two levels"
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
  expect_true(npv_25 >= 0 && npv_25 <= 1)
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
  expect_error(
    negPredValue(as.character(sens_pred), sens_truth),
    "must be a factor"
  )
  expect_error(
    negPredValue(factor(letters[1:3]), factor(letters[1:3])),
    "same two levels"
  )
})
