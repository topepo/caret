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
