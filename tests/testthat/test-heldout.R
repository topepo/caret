# Tests for the hold-out prediction helpers in R/heldout.R. char_mode and
# corr_mat are pure (corr_mat reuses rs_fixture from helper-resamples.R); the
# oob_pred / get_averages / train_lev machinery is exercised through a small
# train() fit that saves its resampled predictions.

# ------------------------------------------------------------------------------

test_that("char_mode returns the most frequent value", {
  expect_identical(caret:::char_mode(c("a", "a", "b")), "a")
  # a tie falls back to the first value when random = FALSE
  expect_identical(caret:::char_mode(c("a", "b"), random = FALSE), "a")
  # missing values can be dropped first
  expect_identical(caret:::char_mode(c("a", "a", NA), na.rm = TRUE), "a")
})

test_that("corr_mat correlates models across resamples", {
  cm <- caret:::corr_mat(rs_fixture, metric = "RMSE")
  expect_identical(dim(cm), c(3L, 3L))
  expect_identical(diag(cm), c(A = 1, B = 1, C = 1))
  # C is a mirror image of A
  expect_equal(cm["A", "C"], -1)
})

# ------------------------------------------------------------------------------

test_that("oob_pred averages the held-out predictions of a classifier", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneLength = 3,
    trControl = trainControl(
      method = "cv",
      number = 3,
      savePredictions = "all",
      classProbs = TRUE
    )
  )

  op <- caret:::oob_pred(fit)
  expect_s3_class(op, "data.frame")
  # one averaged row per training sample, with predicted/observed and class probs
  expect_identical(nrow(op), nrow(iris))
  expect_true(all(c("pred", "obs", levels(iris$Species)) %in% colnames(op)))
  # train_lev recovers the class levels
  expect_identical(caret:::train_lev(fit), levels(iris$Species))
})

test_that("oob_pred averages the held-out predictions of a regression model", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    y ~ .,
    data = SLC14_1(120),
    method = "lm",
    trControl = trainControl(method = "cv", number = 3, savePredictions = "all")
  )

  op <- caret:::oob_pred(fit)
  expect_s3_class(op, "data.frame")
  expect_true(all(c("pred", "obs") %in% colnames(op)))
  # regression models have no class levels
  expect_null(caret:::train_lev(fit))
})

test_that("oob_pred needs saved predictions", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_snapshot(caret:::oob_pred(fit), error = TRUE)
})

test_that("oob_pred works for rfe objects", {
  skip_on_cran()

  set.seed(1)
  dat <- twoClassSim(120)
  set.seed(1)
  rf <- rfe(
    dat[, 1:8],
    dat$Class,
    sizes = c(2, 4),
    rfeControl = rfeControl(
      functions = lrFuncs,
      method = "cv",
      number = 3,
      saveDetails = TRUE,
      returnResamp = "all"
    )
  )
  op <- caret:::oob_pred(rf)
  expect_s3_class(op, "data.frame")
  expect_true(all(c("pred", "obs") %in% colnames(op)))
})

test_that("oob_pred works for sbf objects", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  set.seed(1)
  dat <- twoClassSim(120)
  set.seed(1)
  sf <- sbf(
    dat[, 1:8],
    dat$Class,
    sbfControl = sbfControl(
      functions = ldaSBF,
      method = "cv",
      number = 3,
      saveDetails = TRUE
    )
  )
  op <- caret:::oob_pred(sf)
  expect_s3_class(op, "data.frame")
  expect_true(all(c("pred", "obs") %in% colnames(op)))
})

test_that("oob_pred combines a list of models", {
  skip_on_cran()

  ctrl <- trainControl(method = "cv", number = 3, savePredictions = "all")
  set.seed(1)
  m1 <- train(
    Species ~ .,
    iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = ctrl
  )
  set.seed(1)
  m2 <- train(
    Species ~ .,
    iris,
    method = "knn",
    tuneGrid = data.frame(k = 7),
    trControl = ctrl
  )

  op <- caret:::oob_pred(list(a = m1, b = m2))
  expect_s3_class(op, "data.frame")
})
