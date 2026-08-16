# Tests for thresholder(), which sweeps probability cut-offs for a two-class
# train object and reports performance at each. The stat values are RNG-
# dependent, so the tests assert on structure and snapshot only the deterministic
# validation errors. The threshold_fit() fixture builder lives in
# helper-thresholder.R.

test_that("thresholder returns performance for each probability cut-off", {
  skip_on_cran()

  fit <- threshold_fit(classProbs = TRUE, savePredictions = "all")
  th <- thresholder(fit, threshold = seq(0.3, 0.7, by = 0.1))

  expect_s3_class(th, "data.frame")
  # one row per threshold (the final tuning parameter is used by default)
  expect_identical(nrow(th), 5L)
  expect_in(c("prob_threshold", "Sensitivity", "Specificity"), colnames(th))
  # sensitivity/specificity are proportions
  expect_all_true(th$Sensitivity >= 0 & th$Sensitivity <= 1)
})

test_that("thresholder can report every tuning parameter and a stats subset", {
  skip_on_cran()

  fit <- threshold_fit(classProbs = TRUE, savePredictions = "all")

  # final = FALSE keeps a row per tuning parameter value (two k's here)
  all_tunes <- thresholder(fit, threshold = 0.5, final = FALSE)
  expect_identical(nrow(all_tunes), 2L)

  # a statistics subset limits the reported columns
  subset <- thresholder(
    fit,
    threshold = 0.5,
    statistics = c("Sensitivity", "Specificity")
  )
  expect_in(c("Sensitivity", "Specificity"), colnames(subset))
})

test_that("thresholder validates its inputs", {
  skip_on_cran()

  # not a train object
  expect_snapshot(thresholder(list(), threshold = 0.5), error = TRUE)

  fit <- threshold_fit(classProbs = TRUE, savePredictions = "all")
  # thresholds must be within [0, 1]
  expect_snapshot(thresholder(fit, threshold = 1.5), error = TRUE)

  # classProbs must have been on
  no_probs <- threshold_fit(savePredictions = "all")
  expect_snapshot(thresholder(no_probs, threshold = 0.5), error = TRUE)

  # predictions must have been saved
  no_save <- threshold_fit(classProbs = TRUE)
  expect_snapshot(thresholder(no_save, threshold = 0.5), error = TRUE)
})

test_that("thresholder only supports two-class problems", {
  skip_on_cran()

  fit <- threshold_fit(
    classProbs = TRUE,
    savePredictions = "all",
    formula = Species ~ .,
    data = iris
  )
  expect_snapshot(thresholder(fit, threshold = 0.5), error = TRUE)
})
