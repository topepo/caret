# Tests for the train print method and its text helpers. The helpers are pure
# and deterministic; print.train() itself is exercised by small fits, matched on
# stable substrings (the resampled metric values are RNG-dependent, so the full
# output can't be snapshotted).

# ------------------------------------------------------------------------------
# text helpers

test_that("stringFunc joins a vector into readable text", {
  expect_identical(caret:::stringFunc("a"), "a")
  expect_identical(caret:::stringFunc(c("a", "b")), "a and b")
  expect_identical(caret:::stringFunc(c("a", "b", "c")), "a, b and c")
  expect_identical(caret:::stringFunc(character(0)), "")
  # non-character input is formatted first
  expect_identical(caret:::stringFunc(c(1, 2)), "1 and 2")
})

test_that("truncateText wraps only when the text is wider than the console", {
  expect_identical(caret:::truncateText("short text"), "short text")
  # a very long string is wrapped onto multiple lines
  long <- paste(rep("word", 40), collapse = " ")
  expect_match(caret:::truncateText(long), "\n")
})

test_that("pp_list prints the expanded pre-processing names", {
  expect_snapshot(caret:::pp_list(c("center", "scale")))
  expect_snapshot(caret:::pp_list("BoxCox"))
})

# ------------------------------------------------------------------------------
# print.train across model types

test_that("print.train describes a classification model with tuning", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    preProc = c("center", "scale"),
    tuneLength = 3,
    trControl = trainControl(method = "cv", number = 3)
  )

  expect_output(print(fit), "150 samples")
  expect_output(print(fit), "3 classes")
  expect_output(print(fit), "Pre-processing: centered")
  expect_output(print(fit), "Resampling: Cross-Validated")
  expect_output(print(fit), "Resampling results across tuning parameters")
  expect_output(print(fit), "select the optimal model")
})

test_that("print.train abbreviates the sample sizes with many resamples", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 10)
  )

  # more than five resamples -> the sample-size list is truncated with "..."
  expect_output(print(fit), "Summary of sample sizes:.*\\.\\.\\.")
})

test_that("print.train handles a model fit without resampling", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "none")
  )

  # with no resampling there is no metric table, so the whole print is
  # deterministic and can be snapshotted
  expect_snapshot(print(fit))
})

test_that("print.train reports regression metrics", {
  skip_on_cran()

  set.seed(1)
  dat <- SLC14_1(120)
  fit <- train(
    y ~ .,
    data = dat,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )

  expect_output(print(fit), "predictor")
  expect_output(print(fit), "Resampling results")
  expect_output(print(fit), "RMSE")
})
