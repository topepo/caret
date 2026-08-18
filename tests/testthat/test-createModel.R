# Tests for createModel() (R/createModel.R), the function that fits one model on
# one resample. The workflows drive most of it; these cover the pre-processing
# and sub-sampling options that train() only reaches with particular arguments.

test_that("createModel fits a model and records the tuning values on it", {
  reg <- engine_regression(20)
  method <- getModelInfo("lm", regex = FALSE)[[1]]

  out <- caret:::createModel(
    x = reg[, 1:3],
    y = reg$y,
    wts = NULL,
    method = method,
    tuneValue = data.frame(intercept = TRUE),
    obsLevels = NULL,
    classProbs = FALSE
  )
  # the fit and the pre-processing object are returned separately
  expect_named(out, c("fit", "preProc"))
  expect_s3_class(out$fit, "lm")
  expect_null(out$preProc)
  # the five elements createModel adds to whatever the fit returns
  expect_identical(out$fit$xNames, names(reg)[1:3])
  expect_identical(out$fit$problemType, "Regression")
  expect_identical(out$fit$tuneValue, data.frame(intercept = TRUE))
  expect_null(out$fit$obsLevels)
  expect_identical(out$fit$param, list())
})

test_that("createModel names the problem type from the outcome", {
  cls <- engine_three_class()
  method <- getModelInfo("rpart", regex = FALSE)[[1]]
  skip_if_not_installed("rpart")

  out <- caret:::createModel(
    x = cls[, 1:4],
    y = cls$Species,
    wts = NULL,
    method = method,
    tuneValue = data.frame(cp = 0.1),
    obsLevels = levels(cls$Species),
    classProbs = TRUE
  )
  expect_identical(out$fit$problemType, "Classification")
  expect_identical(out$fit$obsLevels, levels(cls$Species))
})

test_that("createModel applies pre-processing before fitting", {
  reg <- engine_regression(20)
  method <- getModelInfo("lm", regex = FALSE)[[1]]

  out <- caret:::createModel(
    x = reg[, 1:3],
    y = reg$y,
    wts = NULL,
    method = method,
    tuneValue = data.frame(intercept = TRUE),
    obsLevels = NULL,
    classProbs = FALSE,
    pp = list(options = c("center", "scale"))
  )
  expect_s3_class(out$fit, "lm")
  # the pre-processing object comes back so predictions can reuse it, with the
  # call scrubbed to keep the resample data out of the saved object
  expect_s3_class(out$preProc, "preProcess")
  expect_identical(out$preProc$call, "scrubed")
  expect_setequal(names(out$preProc$mean), names(reg)[1:3])
})

test_that("createModel renames the ICA component argument", {
  # `preProcess()` calls the argument n.comp, but train() carries it as
  # ICAcomp, so createModel has to translate
  skip_if_not_installed("fastICA")

  reg <- engine_regression(20)
  method <- getModelInfo("lm", regex = FALSE)[[1]]

  out <- caret:::createModel(
    x = reg[, 1:3],
    y = reg$y,
    wts = NULL,
    method = method,
    tuneValue = data.frame(intercept = TRUE),
    obsLevels = NULL,
    classProbs = FALSE,
    pp = list(options = "ica", ICAcomp = 2)
  )
  # the model was fit on the two independent components
  expect_identical(out$fit$xNames, c("ICA1", "ICA2"))
})

test_that("createModel sub-samples before or after pre-processing", {
  reg <- engine_regression(20)
  method <- getModelInfo("lm", regex = FALSE)[[1]]

  # a sampling function that drops the last half of the rows, so the number of
  # rows the model sees says whether it ran
  halve <- function(x, y) {
    keep <- seq_len(floor(nrow(x) / 2))
    list(x = x[keep, , drop = FALSE], y = y[keep])
  }

  for (first in c(TRUE, FALSE)) {
    out <- caret:::createModel(
      x = reg[, 1:3],
      y = reg$y,
      wts = NULL,
      method = method,
      tuneValue = data.frame(intercept = TRUE),
      obsLevels = NULL,
      classProbs = FALSE,
      pp = list(options = c("center", "scale")),
      sampling = list(name = "halve", func = halve, first = first)
    )
    expect_identical(nrow(out$fit$model), 10L)
  }
})
