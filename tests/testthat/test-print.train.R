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

  expect_snapshot(print(fit))
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
  expect_snapshot(print(fit))
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

  expect_snapshot(print(fit))
})

test_that("pp_list says None when there is nothing to report", {
  # a list of pre-processing methods where every entry is empty
  expect_snapshot(caret:::pp_list(list(center = character(0))))
  # and a character vector with no methods in it
  expect_snapshot(caret:::pp_list(character(0)))
})

# ------------------------------------------------------------------------------
# print.train options

test_that("print.train can show the call and the final-model rows", {
  skip_on_cran()

  dat <- engine_three_class()
  folds <- createFolds(dat$Species, k = 3, returnTrain = TRUE)
  set.seed(6001)
  fit <- train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(
      method = "cv",
      index = folds,
      indexFinal = folds[[1]]
    )
  )

  # printCall adds the call, and indexFinal how many rows the final fit used
  expect_snapshot(print(fit, printCall = TRUE), transform = mask_decimals)
})

test_that("print.train lists the steps of a recipe", {
  skip_on_cran()

  rec <- recipes::recipe(Species ~ ., data = engine_three_class())
  rec <- recipes::step_normalize(rec, recipes::all_predictors())
  rec <- recipes::step_pca(rec, recipes::all_predictors(), num_comp = 2)

  set.seed(2277)
  fit <- train(
    rec,
    data = engine_three_class(),
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3)
  )
  # a recipe replaces the pre-processing line with the steps it uses
  expect_snapshot(print(fit), transform = mask_decimals)
})

test_that("print.train names the additional sampling scheme", {
  skip_on_cran()

  dat <- engine_two_class(60)
  set.seed(5540)
  fit <- train(
    Class ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    preProcess = "center",
    trControl = trainControl(
      method = "cv",
      number = 3,
      sampling = "down"
    )
  )
  expect_snapshot(print(fit), transform = mask_decimals)

  # the other schemes only differ in the label, so relabel the fitted object
  # rather than paying for four more fits
  for (nm in c("up", "smote", "rose", "custom")) {
    relabelled <- fit
    relabelled$control$sampling$name <- nm
    out <- capture.output(print(relabelled))
    expect_match(
      paste(out, collapse = " "),
      "Addtional sampling using .* prior to pre-processing"
    )
  }

  # and sampling after pre-processing reads differently again
  after <- fit
  after$control$sampling$first <- FALSE
  expect_match(
    paste(capture.output(print(after)), collapse = " "),
    "after to pre-processing"
  )
})

test_that("print.train can show the standard deviations", {
  skip_on_cran()

  dat <- engine_three_class()
  set.seed(9382)
  fit <- train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "cv", number = 3)
  )

  # showSD prints each metric as "mean (sd)"
  expect_snapshot(print(fit, showSD = TRUE), transform = mask_decimals)
})

test_that("print.train drops standard deviations that are all missing", {
  skip_on_cran()

  small <- engine_three_class()[c(1:6, 16:21, 31:36), ]
  set.seed(7136)
  fit <- train(
    Species ~ .,
    data = small,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "LOOCV")
  )

  # leave-one-out resampling has a single held-out row, so there is no standard
  # deviation to show and the columns are left out
  out <- capture.output(print(fit, showSD = TRUE))
  expect_false(any(grepl("SD", out)))
})

test_that("print.train keeps a tuning parameter called 'method' apart", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  # `method` is also a column of the printed table, so print.train renames it
  # before merging the chosen settings in. polr's only parameter is called
  # `method`; the outcome is binned from a continuous one so the fit converges
  reg <- engine_regression(90)
  reg$ord <- ordered(cut(reg$y, 3, labels = c("lo", "mid", "hi")))
  set.seed(4880)
  fit <- train(
    ord ~ x1 + x2 + x3,
    data = reg,
    method = "polr",
    tuneGrid = data.frame(method = c("logistic", "probit")),
    trControl = trainControl(method = "cv", number = 3)
  )
  out <- capture.output(print(fit))
  # the column keeps its own name in the printed table
  expect_match(paste(out, collapse = " "), "method")
  expect_true(any(grepl("logistic", out)))
})

test_that("print.train reports a model with nothing to tune", {
  skip_on_cran()

  reg <- engine_regression(40)
  set.seed(3355)
  fit <- train(
    y ~ .,
    data = reg,
    method = "glm",
    trControl = trainControl(method = "cv", number = 3)
  )
  # glm has no tuning parameters, so there is no "held constant" note
  out <- capture.output(print(fit))
  expect_false(any(grepl("held constant", out)))
  expect_true(any(grepl("Resampling results", out)))
})

test_that("print.train names a custom selection rule", {
  skip_on_cran()

  dat <- engine_three_class()
  set.seed(1094)
  fit <- train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5, 7)),
    trControl = trainControl(
      method = "cv",
      number = 3,
      selectionFunction = function(x, metric, maximize) 1
    )
  )
  expect_match(
    paste(capture.output(print(fit)), collapse = " "),
    "a custom selection rule"
  )
})

test_that("print.train says when the parameters were set by update()", {
  skip_on_cran()

  dat <- engine_three_class()
  set.seed(2960)
  fit <- train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "cv", number = 3)
  )
  updated <- update(fit, list(k = 5))
  expect_match(
    paste(capture.output(print(updated)), collapse = " "),
    "tuning parameter was set manually"
  )
})

test_that("print.train can print the final model as well", {
  skip_on_cran()

  reg <- engine_regression(40)
  set.seed(8032)
  lm_fit <- train(
    y ~ .,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  # for these models the details are the summary of the final model
  out <- capture.output(print(lm_fit, details = TRUE))
  expect_true(any(grepl("The final model:", out)))
  expect_true(any(grepl("Residual standard error", out)))

  skip_if_not_installed("rpart")
  set.seed(8032)
  rpart_fit <- train(
    Species ~ .,
    data = engine_three_class(),
    method = "rpart",
    tuneGrid = data.frame(cp = 0.1),
    trControl = trainControl(method = "cv", number = 3)
  )
  # and for others the final model is printed as it stands
  expect_true(any(grepl(
    "node.*, split, n, loss",
    capture.output(print(rpart_fit, details = TRUE))
  )))

  # knn is on the list of models whose details are skipped
  knn_fit <- train(
    Species ~ .,
    data = engine_three_class(),
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_false(any(grepl(
    "The final model:",
    capture.output(print(knn_fit, details = TRUE))
  )))
})

test_that("print.train prints an fda model's terms with its details", {
  skip_on_cran()
  skip_if_not_installed("mda")
  skip_if_not_installed("earth")

  set.seed(5817)
  # earth announces the dependencies it attaches; leave them attached (undoing
  # it here only moves the message to whichever test file loads earth next)
  suppressMessages(
    fit <- train(
      Species ~ .,
      data = engine_three_class(),
      method = "fda",
      tuneGrid = data.frame(degree = 1, nprune = 3),
      trControl = trainControl(method = "cv", number = 3)
    )
  )
  out <- capture.output(print(fit, details = TRUE))
  expect_true(any(grepl("Summary of Terms", out)))
})

test_that("truncateText joins a vector before wrapping it", {
  expect_identical(caret:::truncateText(c("one ", "two")), "one two")
})

test_that("print.train leaves out standard deviations it cannot compute", {
  skip_on_cran()

  dat <- engine_three_class()
  set.seed(6448)
  fit <- train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "boot", number = 1)
  )

  # a single resample has standard deviation columns but nothing to put in them
  expect_true(any(grepl("SD$", names(fit$results))))
  out <- capture.output(print(fit, showSD = TRUE))
  expect_false(any(grepl("NA", out)))
})
