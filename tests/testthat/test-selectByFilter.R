# Tests for filter-based feature selection (sbf). The pure helpers (sbfControl,
# anovaScores) and the standalone nullModel are unit-tested directly; the sbf()
# workflow and its methods are exercised by small integration fits at the bottom.

# ------------------------------------------------------------------------------
# sbfControl

test_that("sbfControl fills in sensible defaults", {
  ctrl <- sbfControl()
  expect_identical(ctrl$method, "boot")
  # with no functions supplied it defaults to caretSBF
  expect_identical(ctrl$functions, caretSBF)
})

# ------------------------------------------------------------------------------
# univariate filter scores

test_that("anovaScores returns a small p-value for an informative predictor", {
  set.seed(1)
  grp <- factor(rep(c("a", "b"), each = 20))
  signal <- c(rnorm(20, 0), rnorm(20, 5))
  expect_lt(caret:::anovaScores(signal, grp), 0.05)
})

test_that("anovaScores rejects factor predictors", {
  expect_snapshot(
    caret:::anovaScores(factor(c("a", "b")), factor(c("a", "b"))),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------
# nullModel (a baseline that ignores the predictors)

test_that("nullModel predicts the majority class for a factor outcome", {
  nm <- caret:::nullModel(y = factor(c("a", "a", "b")))
  expect_s3_class(nm, "nullModel")
  expect_identical(nm$value, "a")
  expect_identical(nm$levels, c("a", "b"))

  # class predictions: the majority class, one per new row
  preds <- predict(nm, newdata = data.frame(x = 1:4))
  expect_identical(as.character(preds), rep("a", 4))
  # probability predictions: the class proportions
  probs <- predict(nm, type = "prob")
  expect_identical(colnames(probs), c("a", "b"))
})

test_that("nullModel predicts the mean for a numeric outcome", {
  nm <- caret:::nullModel(y = c(1, 2, 3, 4))
  expect_null(nm$levels)
  # mean(1:4) is exactly 2.5, so this round-trips bit-for-bit
  expect_identical(nm$value, 2.5)
  expect_identical(predict(nm, newdata = data.frame(x = 1:3)), rep(2.5, 3))
  # class/prob predictions make no sense for regression
  expect_snapshot(predict(nm, type = "prob"), error = TRUE)
})

test_that("print.nullModel labels the model type correctly", {
  # regression tests would depend on formatted numbers, so snapshot the
  # deterministic type label via the classification model
  expect_output(
    print(caret:::nullModel(y = factor(c("a", "a", "b")))),
    "Null Classification Model"
  )
  expect_output(
    print(caret:::nullModel(y = c(1, 2, 3))),
    "Null Regression Model"
  )
})

# ------------------------------------------------------------------------------
# sbf() workflow + methods

test_that("sbf runs and its methods behave (default interface)", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  set.seed(1)
  dat <- twoClassSim(150)
  set.seed(1)
  sf <- sbf(
    dat[, 1:8],
    dat$Class,
    sbfControl = sbfControl(functions = ldaSBF, method = "cv", number = 3)
  )

  expect_s3_class(sf, "sbf")
  expect_identical(predictors(sf), sf$optVariables)
  expect_identical(nrow(predict(sf, dat[, 1:8])), nrow(dat))
  expect_output(print(sf), "Selection By Filter")
})

test_that("sbf works with the formula and recipe interfaces", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  skip_if_not_installed("recipes")

  set.seed(1)
  full <- twoClassSim(150)
  dat <- full[, c(names(full)[1:8], "Class")]

  set.seed(1)
  sf_form <- sbf(
    Class ~ .,
    data = dat,
    sbfControl = sbfControl(functions = ldaSBF, method = "cv", number = 3)
  )
  expect_s3_class(sf_form, "sbf")

  set.seed(1)
  rec <- recipes::recipe(Class ~ ., data = dat)
  sf_rec <- sbf(
    rec,
    data = dat,
    sbfControl = sbfControl(functions = ldaSBF, method = "cv", number = 3)
  )
  expect_s3_class(sf_rec, "sbf")
})
