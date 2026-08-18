# Tests for predict.train() and predict.list() (R/predict.train.R). The ordinary
# paths are covered by the model tests; these are the argument checks and the
# ways of supplying the data.

test_that("predict.list predicts from every model in a list", {
  skip_on_cran()

  dat <- engine_three_class()
  ctrl <- trainControl(method = "cv", number = 3)
  set.seed(9739)
  fits <- list(
    knn = train(
      Species ~ .,
      data = dat,
      method = "knn",
      tuneGrid = data.frame(k = 5),
      trControl = ctrl
    ),
    lda = train(Species ~ ., data = dat, method = "lda", trControl = ctrl)
  )

  out <- predict(fits, newdata = dat)
  # the names of the list are carried over to the predictions
  expect_named(out, c("knn", "lda"))
  expect_length(out$knn, nrow(dat))

  # an unnamed list gives an unnamed result
  expect_null(names(predict(unname(fits), newdata = dat)))
})

test_that("predict.train checks the type it is asked for", {
  skip_on_cran()

  reg <- engine_regression(30)
  set.seed(1521)
  fit <- train(
    y ~ .,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )

  expect_snapshot(predict(fit, reg, type = "class"), error = TRUE)
  # a regression model has no probabilities to give
  expect_snapshot(predict(fit, reg, type = "prob"), error = TRUE)
})

test_that("predict.train falls back to the training data", {
  skip_on_cran()

  reg <- engine_regression(30)
  set.seed(6624)
  fit <- train(
    y ~ .,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )

  # with no newdata the stored training data is predicted
  expect_length(predict(fit), nrow(reg))
})

test_that("predict.train expands factor predictors from the training data", {
  skip_on_cran()

  dat <- engine_regression(40)
  dat$grp <- factor(rep(c("a", "b"), length.out = nrow(dat)))
  set.seed(4370)
  fit <- train(
    y ~ .,
    data = dat,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )

  # the formula interface stores the un-expanded data, so the factor has to be
  # turned back into dummy variables before the model sees it
  expect_length(predict(fit), nrow(dat))
})

test_that("predict.train needs data when none was kept", {
  skip_on_cran()

  reg <- engine_regression(30)
  set.seed(8109)
  fit <- train(
    y ~ .,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3, returnData = FALSE)
  )

  expect_snapshot(predict(fit), error = TRUE)
})

test_that("predict.train reads the training data off a pam model", {
  skip_on_cran()
  skip_if_not_installed("pamr")

  dat <- engine_three_class()
  set.seed(3906)
  # pam keeps its own copy of the predictors, in its own layout. pamr prints a
  # progress counter as it fits, which is captured rather than shown
  invisible(capture.output(
    fit <- suppressWarnings(train(
      Species ~ .,
      data = dat,
      method = "pam",
      tuneGrid = data.frame(threshold = 1),
      trControl = trainControl(method = "cv", number = 3)
    ))
  ))
  invisible(capture.output(pred <- suppressWarnings(predict(fit))))
  expect_length(pred, nrow(dat))
})

test_that("predict.train updates an object from an old caret version", {
  skip_on_cran()

  dat <- engine_three_class()
  set.seed(2508)
  fit <- train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3)
  )

  # objects from caret 5.17-7 and earlier have no modelInfo element, so
  # predict() updates them first
  old <- fit
  old$modelInfo <- NULL
  expect_snapshot_warning(pred <- predict(old, dat))
  expect_length(pred, nrow(dat))
})

test_that("predict.train works out the class levels when they are missing", {
  skip_on_cran()

  dat <- engine_three_class()
  set.seed(7263)
  fit <- train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3)
  )

  # older objects did not store the levels, so they are recovered from the fit
  no_levels <- fit
  no_levels$levels <- NULL
  out <- predict(no_levels, dat)
  expect_s3_class(out, "factor")
  expect_identical(levels(out), levels(dat$Species))
})
