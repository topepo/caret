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
  expect_in(c("pred", "obs", levels(iris$Species)), colnames(op))
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
  expect_in(c("pred", "obs"), colnames(op))
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
  expect_in(c("pred", "obs"), colnames(op))
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
  expect_in(c("pred", "obs"), colnames(op))
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

test_that("char_mode samples among tied values when random = TRUE", {
  set.seed(7734)
  out <- caret:::char_mode(c("a", "b"))
  expect_in(out, c("a", "b"))
  expect_length(out, 1)
})

test_that("oob_pred keeps the tuning parameter when best = FALSE", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = c(5, 7)),
    trControl = trainControl(
      method = "cv",
      number = 3,
      savePredictions = "all",
      classProbs = TRUE
    )
  )

  op <- caret:::oob_pred(fit, best = FALSE)
  expect_s3_class(op, "data.frame")
  # one averaged row per sample and tuning-parameter value
  expect_identical(nrow(op), nrow(iris) * 2L)
  expect_contains(colnames(op), "k")
})

test_that("oob_pred.rfe and oob_pred.sbf need saved details", {
  no_pred_rfe <- structure(list(pred = NULL), class = "rfe")
  expect_snapshot(caret:::oob_pred(no_pred_rfe), error = TRUE)

  no_pred_sbf <- structure(list(pred = NULL), class = "sbf")
  expect_snapshot(caret:::oob_pred(no_pred_sbf), error = TRUE)
})

test_that("oob_pred.rfe keeps the subset sizes when best = FALSE", {
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
      saveDetails = TRUE
    )
  )
  op <- caret:::oob_pred(rf, best = FALSE)
  expect_s3_class(op, "data.frame")
  expect_contains(colnames(op), "Variables")
})

test_that("oob_pred averages regression rfe and sbf hold-outs", {
  skip_on_cran()

  set.seed(6412)
  dat <- SLC14_1(100)
  set.seed(6412)
  rf <- rfe(
    dat[, 1:6],
    dat$y,
    sizes = c(2, 4),
    rfeControl = rfeControl(
      functions = lmFuncs,
      method = "cv",
      number = 3,
      saveDetails = TRUE
    )
  )
  op <- caret:::oob_pred(rf)
  expect_s3_class(op, "data.frame")
  expect_type(op$pred, "double")

  set.seed(6412)
  sf <- sbf(
    dat[, 1:6],
    dat$y,
    sbfControl = sbfControl(
      functions = lmSBF,
      method = "cv",
      number = 3,
      saveDetails = TRUE
    )
  )
  op2 <- caret:::oob_pred(sf)
  expect_s3_class(op2, "data.frame")
  expect_type(op2$pred, "double")
})

test_that("oob_pred.list prunes uncommon columns and auto-names models", {
  skip_on_cran()

  # m1 saves class probabilities, m2 does not, so the probability columns are
  # dropped when the two are combined
  set.seed(1)
  m1 <- train(
    Species ~ .,
    iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(
      method = "cv",
      number = 3,
      savePredictions = "all",
      classProbs = TRUE
    )
  )
  set.seed(1)
  m2 <- train(
    Species ~ .,
    iris,
    method = "knn",
    tuneGrid = data.frame(k = 7),
    trControl = trainControl(method = "cv", number = 3, savePredictions = "all")
  )

  # unnamed lists get Model1, Model2, ... labels
  op <- caret:::oob_pred(list(m1, m2))
  expect_s3_class(op, "data.frame")
  expect_contains(colnames(op), c("pred.Model1", "pred.Model2"))
  expect_all_false(grepl("setosa", colnames(op)))
})

test_that("oob_pred.list can return predictions or probabilities only", {
  skip_on_cran()

  ctrl <- trainControl(
    method = "cv",
    number = 3,
    savePredictions = "all",
    classProbs = TRUE
  )
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

  preds <- caret:::oob_pred(list(a = m1, b = m2), what = "pred")
  expect_all_false(grepl("^setosa\\.", colnames(preds)))
  expect_contains(colnames(preds), c("pred.a", "pred.b"))

  probs <- caret:::oob_pred(list(a = m1, b = m2), what = "prob")
  expect_all_false(grepl("^pred\\.", colnames(probs)))
  expect_contains(colnames(probs), c("setosa.a", "setosa.b"))

  long <- caret:::oob_pred(list(a = m1, b = m2), direction = "long")
  expect_contains(colnames(long), ".label")
})

test_that("oob_pred.list rejects models with different resampling footprints", {
  skip_on_cran()

  set.seed(1)
  m1 <- train(
    Species ~ .,
    iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3, savePredictions = "all")
  )
  # bootstrap hold-out counts vary from sample to sample, so mixing them with
  # the uniform cv counts trips the equal-footprint check
  set.seed(1)
  m2 <- train(
    Species ~ .,
    iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(
      method = "boot",
      number = 5,
      savePredictions = "all"
    )
  )
  expect_snapshot(caret:::oob_pred(list(a = m1, b = m2)), error = TRUE)
})

test_that("train_lev falls back to the model's stored levels", {
  # an S3 fit without a modelInfo levels function reads finalModel$obsLevel
  s3_fit <- list(
    modelType = "Classification",
    modelInfo = list(levels = NULL),
    finalModel = list(obsLevel = c("a", "b"))
  )
  expect_identical(caret:::train_lev(s3_fit), c("a", "b"))

  # an S4 container reads the saved predictions instead
  s4_fit <- new(
    "fake_s4_train",
    payload = list(
      modelType = "Classification",
      modelInfo = list(levels = NULL),
      pred = data.frame(obs = factor(c("a", "b", "a")))
    )
  )
  lev <- caret:::train_lev(s4_fit)
  expect_identical(as.character(sort(lev)), c("a", "b"))
})
