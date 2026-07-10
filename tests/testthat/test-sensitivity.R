# Reference data from the sensitivity() documentation example
lvs <- c("normal", "abnormal")
truth <- factor(rep(lvs, times = c(86, 258)), levels = rev(lvs))
pred <- factor(
  c(rep(lvs, times = c(54, 32)), rep(lvs, times = c(27, 231))),
  levels = rev(lvs)
)
xtab <- table(pred, truth)
# a plain matrix (class "matrix") to exercise the `.matrix` methods, since
# as.matrix() on a table keeps the "table" class
mtab <- matrix(as.vector(xtab), nrow = 2, dimnames = dimnames(xtab))

# --- sensitivity ------------------------------------------------------------

test_that("sensitivity works for factors, tables and matrices", {
  expect_equal(sensitivity(pred, truth), 231 / 258)
  expect_equal(sensitivity(xtab), 231 / 258)
  expect_equal(sensitivity(mtab), 231 / 258)
  # the positive class can be chosen explicitly
  expect_equal(sensitivity(pred, truth, positive = "normal"), 54 / 86)
})

test_that("sensitivity errors on bad input", {
  expect_error(sensitivity(as.character(pred), truth), "must be factors")
  expect_error(
    sensitivity(factor(letters[1:3]), factor(letters[1:3])),
    "same two levels"
  )
  # a non-square table is rejected
  expect_error(sensitivity(as.table(matrix(1:6, nrow = 2))))
})

test_that("sensitivity handles NAs and empty denominators", {
  p <- pred
  p[1:5] <- NA
  # na.rm = TRUE (default) drops incomplete cases without error
  expect_true(!is.na(sensitivity(p, truth)))
  # no positive cases -> NA
  no_pos <- factor(rep("normal", 10), levels = rev(lvs))
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
  expect_equal(specificity(pred, truth), 54 / 86)
  expect_equal(specificity(xtab), 54 / 86)
  expect_equal(specificity(mtab), 54 / 86)
})

test_that("specificity errors on bad input", {
  expect_error(specificity(as.character(pred), truth), "must be a factor")
  expect_error(
    specificity(factor(letters[1:3]), factor(letters[1:3])),
    "same two levels"
  )
  expect_error(specificity(as.table(matrix(1:6, nrow = 2))))
})

test_that("specificity handles empty denominators and multiclass tables", {
  no_neg <- factor(rep("abnormal", 10), levels = rev(lvs))
  expect_true(is.na(specificity(no_neg, no_neg, negative = "normal")))
  irisTabs <- table(iris$Species, iris$Species)
  expect_equal(specificity(irisTabs, c("setosa", "virginica")), 1)
})

# --- posPredValue -----------------------------------------------------------

test_that("posPredValue matches the sensitivity/specificity identity", {
  sens <- sensitivity(pred, truth)
  spec <- specificity(pred, truth)
  prev <- mean(truth == "abnormal")
  expected <- (sens * prev) / ((sens * prev) + ((1 - spec) * (1 - prev)))
  expect_equal(posPredValue(pred, truth), expected)
  expect_equal(posPredValue(xtab), expected)
  expect_equal(posPredValue(mtab), expected)
})

test_that("posPredValue respects a supplied prevalence", {
  ppv_25 <- posPredValue(pred, truth, prevalence = 0.25)
  expect_true(ppv_25 >= 0 && ppv_25 <= 1)
  # table method also accepts prevalence
  expect_equal(
    posPredValue(xtab, prevalence = 0.25),
    posPredValue(pred, truth, prevalence = 0.25)
  )
})

test_that("posPredValue collapses multiclass tables", {
  irisTabs <- table(iris$Species, iris$Species)
  expect_equal(posPredValue(irisTabs, "versicolor"), 1)
})

test_that("posPredValue errors on bad input", {
  expect_error(posPredValue(as.character(pred), truth), "must be factors")
  expect_error(
    posPredValue(factor(letters[1:3]), factor(letters[1:3])),
    "same two levels"
  )
})

# --- negPredValue -----------------------------------------------------------

test_that("negPredValue matches the sensitivity/specificity identity", {
  sens <- sensitivity(pred, truth)
  spec <- specificity(pred, truth)
  prev <- mean(truth == "abnormal")
  expected <- (spec * (1 - prev)) /
    (((1 - sens) * prev) + (spec * (1 - prev)))
  expect_equal(negPredValue(pred, truth), expected)
  expect_equal(negPredValue(xtab), expected)
  expect_equal(negPredValue(mtab), expected)
})

test_that("negPredValue respects a supplied prevalence", {
  npv_25 <- negPredValue(pred, truth, prevalence = 0.25)
  expect_true(npv_25 >= 0 && npv_25 <= 1)
  expect_equal(
    negPredValue(xtab, prevalence = 0.25),
    negPredValue(pred, truth, prevalence = 0.25)
  )
})

test_that("negPredValue collapses multiclass tables", {
  irisTabs <- table(iris$Species, iris$Species)
  expect_equal(negPredValue(irisTabs, negative = "versicolor"), 1)
})

test_that("negPredValue errors on bad input", {
  expect_error(negPredValue(as.character(pred), truth), "must be a factor")
  expect_error(
    negPredValue(factor(letters[1:3]), factor(letters[1:3])),
    "same two levels"
  )
})
