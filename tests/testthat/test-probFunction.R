# Tests for probFunction() (R/probFunction.R), the internal wrapper that calls a
# model's `prob` function and tidies up what comes back.
#
# The class levels are read with levels(modelFit), so the fixtures below carry
# them as an attribute the way a model object would.

test_that("probFunction returns a data frame from the model untouched", {
  method <- list(
    prob = function(modelFit, newdata, submodels = NULL) {
      data.frame(
        b = rep(0.25, nrow(newdata)),
        a = rep(0.75, nrow(newdata))
      )
    }
  )
  fit <- structure(list(), levels = c("a", "b"), class = "fake_fit")

  # a data frame is passed through as the model built it
  out <- caret:::probFunction(method, fit, data.frame(x = 1:3))
  expect_s3_class(out, "data.frame")
  expect_named(out, c("b", "a"))
})

test_that("probFunction orders matrix columns by the class levels", {
  method <- list(
    prob = function(modelFit, newdata, submodels = NULL) {
      cbind(
        b = rep(0.25, nrow(newdata)),
        a = rep(0.75, nrow(newdata))
      )
    }
  )
  fit <- structure(list(), levels = c("a", "b"), class = "fake_fit")

  out <- caret:::probFunction(method, fit, data.frame(x = 1:3))
  expect_s3_class(out, "data.frame")
  # a matrix is converted and its columns put in the order of the levels
  expect_named(out, c("a", "b"))
  expect_all_equal(out$a, 0.75)
  expect_all_equal(out$b, 0.25)
})

test_that("probFunction leaves a matrix alone when the levels are unknown", {
  method <- list(
    prob = function(modelFit, newdata, submodels = NULL) {
      cbind(b = rep(0.25, nrow(newdata)), a = rep(0.75, nrow(newdata)))
    }
  )
  # no levels attribute, so there is nothing to order the columns by
  fit <- structure(list(), class = "fake_fit")

  out <- caret:::probFunction(method, fit, data.frame(x = 1:3))
  expect_s3_class(out, "data.frame")
  expect_named(out, c("b", "a"))
})

test_that("probFunction returns sub-model probabilities as the model built them", {
  method <- list(
    prob = function(modelFit, newdata, submodels = NULL) {
      # with sub-models the model returns one data frame per candidate
      lapply(seq_len(nrow(submodels) + 1), function(i) {
        data.frame(b = rep(0.25, nrow(newdata)), a = rep(0.75, nrow(newdata)))
      })
    }
  )
  fit <- structure(list(), levels = c("a", "b"), class = "fake_fit")

  out <- caret:::probFunction(
    method,
    fit,
    data.frame(x = 1:3),
    param = data.frame(k = 5)
  )
  # the list is not tidied, since only the model knows how it is laid out
  expect_type(out, "list")
  expect_length(out, 2)
})

test_that("probFunction preprocesses newdata before the model sees it", {
  seen <- NULL
  method <- list(
    prob = function(modelFit, newdata, submodels = NULL) {
      seen <<- newdata
      data.frame(a = rep(0.5, nrow(newdata)), b = rep(0.5, nrow(newdata)))
    }
  )
  fit <- structure(list(), levels = c("a", "b"), class = "fake_fit")

  train_x <- data.frame(x = c(0, 5, 10))
  pp <- preProcess(train_x, method = "range")
  caret:::probFunction(method, fit, data.frame(x = c(0, 10)), preProc = pp)

  # the model was handed the scaled values, not the raw ones
  expect_equal(seen$x, c(0, 1))
})
