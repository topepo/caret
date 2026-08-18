# Tests for trainControl (R/trainControl.R).

test_that("resampling method 'none' doesn't conflict with default tuneLength", {
  skip_on_cran()

  data(BloodBrain)

  expect_snapshot(
    train(
      bbbDescr,
      logBBB,
      method = "earth",
      tuneLength = 2,
      trControl = trainControl(method = "none")
    ),
    error = TRUE
  )

  expect_snapshot(
    train(
      mpg ~ cyl + disp,
      data = mtcars,
      method = "gam",
      tuneLength = 2,
      trControl = trainControl(method = "none")
    ),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------
# argument validation

test_that("trainControl checks the arguments it can", {
  expect_snapshot(trainControl(selectionFunction = NULL), error = TRUE)
  expect_snapshot(trainControl(returnResamp = "some"), error = TRUE)
  expect_snapshot(trainControl(predictionBounds = c(0, 10, 20)), error = TRUE)
  expect_snapshot(trainControl(search = "sobol"), error = TRUE)
})

test_that("trainControl keeps preProcess options for preProcess", {
  # the method and the data are supplied by train(), so they cannot be set here
  expect_snapshot(
    trainControl(preProcOptions = list(method = "range")),
    error = TRUE
  )
  expect_snapshot(
    trainControl(preProcOptions = list(x = iris[, 1:4])),
    error = TRUE
  )
})

test_that("trainControl warns when repeats cannot be used", {
  # only the repeated schemes repeat, so giving repeats elsewhere is a mistake
  expect_snapshot_warning(trainControl(method = "cv", repeats = 3))
  expect_silent(trainControl(method = "repeatedcv", repeats = 3))
})

test_that("trainControl checks the adaptive resampling settings", {
  adapt <- function(...) {
    modifyList(
      list(min = 5, alpha = 0.05, method = "gls", complete = TRUE),
      list(...)
    )
  }

  expect_snapshot(trainControl(adaptive = adapt(method = "lm")), error = TRUE)
  expect_snapshot(trainControl(adaptive = adapt(alpha = 2)), error = TRUE)

  # the burn-in has to leave resamples to adapt over, and to be more than one
  expect_snapshot(
    trainControl(
      method = "adaptive_cv",
      number = 5,
      repeats = 1,
      adaptive = adapt(min = 5)
    ),
    error = TRUE
  )
  expect_snapshot(
    trainControl(
      method = "adaptive_boot",
      number = 25,
      adaptive = adapt(min = 1)
    ),
    error = TRUE
  )
})

test_that("trainControl warns that out-of-bag resampling has fixed measures", {
  # the out-of-bag estimates come from the model itself, so a custom summary
  # function cannot be applied to them
  expect_snapshot_warning(
    trainControl(method = "oob", summaryFunction = twoClassSummary)
  )
})
