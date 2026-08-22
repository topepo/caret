# Tests for the small helpers in R/workflows.R. The resampling workflows
# themselves are driven through train() in test-train.default.R; these are the
# pieces that are easier to pin down directly.

test_that("getOper and getTrainOper choose the foreach operator", {
  # TRUE asks for the parallel operator, FALSE the sequential one
  expect_identical(caret:::getOper(TRUE), foreach::`%dopar%`)
  expect_identical(caret:::getOper(FALSE), foreach::`%do%`)

  # getTrainOper is the same choice, kept separate for the parallel backends
  # that cannot see caret's internals
  expect_identical(caret:::getTrainOper(TRUE), foreach::`%dopar%`)
  expect_identical(caret:::getTrainOper(FALSE), foreach::`%do%`)
})

test_that("progress reports the parameters of the current resample", {
  params <- data.frame(k = 5)
  expect_snapshot(caret:::progress(
    params,
    names = c("Fold1", "Fold2"),
    iter = 1
  ))
  # start = FALSE marks the end of a resample instead of its beginning
  expect_snapshot(
    caret:::progress(
      params,
      names = c("Fold1", "Fold2"),
      iter = 2,
      start = FALSE
    )
  )
})

test_that("MeanSD averages columns and names the standard deviations", {
  x <- data.frame(RMSE = c(1, 3), Rsquared = c(0.4, 0.6))
  out <- caret:::MeanSD(x)
  expect_named(out, c("RMSE", "Rsquared", "RMSESD", "RsquaredSD"))
  expect_equal(unname(out["RMSE"]), 2)
  expect_equal(unname(out["RMSESD"]), sd(c(1, 3)))

  # columns can be left out of the summary
  dropped <- caret:::MeanSD(x, exclude = "Rsquared")
  expect_named(dropped, c("RMSE", "RMSESD"))
})

test_that("expandParameters combines fixed and varying parameters", {
  fixed <- data.frame(shift = 1, scale = 2)

  # with nothing to vary the fixed row is returned as is
  expect_identical(caret:::expandParameters(fixed, NULL), fixed)

  varying <- data.frame(shift = c(5, 6))
  out <- caret:::expandParameters(fixed, varying)
  # the fixed row, then one row per varying value
  expect_identical(nrow(out), 3L)
  expect_equal(out$shift, c(1, 5, 6))
  # the parameters that are not varying keep their fixed value
  expect_all_equal(out$scale, 2)
})

# ------------------------------------------------------------------------------
# looTrainWorkflow: the same paths as the nominal workflow, one row at a time
#
# These go through train() because the workflow is only reachable that way. The
# sentinel fixtures make exactly one held-out row fail (see helper-fake-models.R).

test_that("leave-one-out resampling reports a fit that fails for one row", {
  skip_on_cran()

  dat <- engine_sentinel_data(16)
  failing <- make_submodel_model(fail_fit = TRUE)

  set.seed(6011)
  expect_snapshot_warning(
    fit <- train(
      dat[, 1:3],
      dat$y,
      method = failing,
      tuneLength = 2,
      trControl = trainControl(
        method = "LOOCV",
        classProbs = TRUE,
        savePredictions = "all"
      )
    )
  )
  # the held-out row that failed leaves missing values behind
  expect_s3_class(fit, "train")
  expect_true(anyNA(fit$pred$one))
})

test_that("leave-one-out resampling reports predictions that fail", {
  skip_on_cran()

  dat <- engine_sentinel_data(16)
  bad_pred <- make_submodel_model(fail_pred = TRUE)

  set.seed(6011)
  expect_snapshot_warning(
    fit <- train(
      dat[, 1:3],
      dat$y,
      method = bad_pred,
      tuneLength = 2,
      trControl = trainControl(method = "LOOCV")
    )
  )
  expect_s3_class(fit, "train")
})

test_that("leave-one-out resampling carries case weights", {
  skip_on_cran()

  reg <- engine_regression(16)
  wts <- rep(c(1, 2), length.out = nrow(reg))

  set.seed(3517)
  fit <- train(
    reg[, 1:3],
    reg$y,
    method = "lm",
    weights = wts,
    trControl = trainControl(method = "LOOCV", savePredictions = "all")
  )
  # each held-out row is scored with its own weight
  expect_in("weights", names(fit$pred))
  expect_setequal(unique(fit$pred$weights), unique(wts))
})

test_that("leave-one-out resampling reports its progress and debug trace", {
  skip_on_cran()

  reg <- engine_regression(12)
  tolerant <- make_custom_model()

  set.seed(4471)
  expect_snapshot(
    fit <- suppressWarnings(train(
      reg[, 1:3],
      reg$y,
      method = tolerant,
      tuneLength = 2,
      trControl = trainControl(method = "LOOCV", verboseIter = TRUE),
      testing = TRUE
    )),
    transform = mask_decimals
  )
  expect_s3_class(fit, "train")
})
