# Tests for learning_curve_dat(), which fits a model at increasing training-set
# sizes and returns the resampled (and optionally training/testing) performance
# at each size. The performance values are RNG-dependent, so the tests assert on
# structure and snapshot only the deterministic error. The fixture builder lives
# in helper-learning-curve.R.

test_that("learning_curve_dat returns performance at each training size", {
  skip_on_cran()

  lc <- learning_curve_fixture(proportion = c(0.5, 0.75, 1))

  expect_s3_class(lc, "data.frame")
  expect_in(c("Training_Size", "Data", "ROC"), colnames(lc))
  # without a test set there are resampling and training estimates
  expect_setequal(unique(lc$Data), c("Resampling", "Training"))
  # one training size per requested proportion
  expect_length(unique(lc$Training_Size), 3)
})

test_that("learning_curve_dat adds a testing set when test_prop > 0", {
  skip_on_cran()

  lc <- learning_curve_fixture(proportion = c(0.5, 1), test_prop = 0.3)
  expect_setequal(unique(lc$Data), c("Resampling", "Training", "Testing"))
})

test_that("learning_curve_dat prints progress when verbose", {
  skip_on_cran()

  # assign inside the snapshot so only the progress text is captured, not the
  # RNG-dependent metric values
  expect_snapshot(
    lc <- learning_curve_fixture(proportion = c(0.5, 1), verbose = TRUE)
  )
})

test_that("learning_curve_dat requires an outcome column name", {
  skip_on_cran()

  set.seed(1)
  dat <- twoClassSim(50)
  expect_snapshot(learning_curve_dat(dat), error = TRUE)
})

test_that("learning_curve_dat rejects method = 'none'", {
  skip_on_cran()

  set.seed(1)
  dat <- twoClassSim(120)
  expect_snapshot(
    learning_curve_dat(
      dat,
      outcome = "Class",
      proportion = c(0.5, 1),
      verbose = FALSE,
      method = "knn",
      tuneGrid = data.frame(k = 5),
      trControl = trainControl(method = "none", classProbs = TRUE)
    ),
    error = TRUE
  )
})

test_that("learning_curve_dat works without class probabilities", {
  skip_on_cran()

  # with classProbs turned off the predictions come from extractPrediction()
  # rather than extractProb(), for both the testing and the training estimates
  reg <- engine_regression(80)

  set.seed(2688)
  lc <- learning_curve_dat(
    reg,
    outcome = "y",
    proportion = c(0.5, 1),
    test_prop = 0.3,
    verbose = FALSE,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_setequal(unique(lc$Data), c("Resampling", "Training", "Testing"))
  expect_in(c("RMSE", "Rsquared", "Training_Size"), colnames(lc))
})
