# Tests for the prediction-extraction pipeline. extractPrediction and extractProb
# are twins (same signature, a list of fitted models); plotClassProbs consumes
# extractProb's output and plotObsVsPred consumes extractPrediction's, so they
# are tested together off one fitted model list. The regression trimming path
# (trimPredictions) is covered separately.

test_that("extractPrediction / extractProb / plotClassProbs work on a model list", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  set.seed(1)
  tr <- twoClassSim(120)
  te <- twoClassSim(60)
  ctrl <- trainControl(method = "cv", number = 3, classProbs = TRUE)

  set.seed(2)
  m1 <- train(
    Class ~ .,
    data = tr,
    method = "knn",
    tuneLength = 2,
    trControl = ctrl
  )
  set.seed(2)
  m2 <- train(Class ~ ., data = tr, method = "lda", trControl = ctrl)
  mods <- list(knn = m1, lda = m2)

  te_x <- te[, colnames(te) != "Class"]

  # extractPrediction: class predictions for the training and test sets
  pred <- extractPrediction(mods, testX = te_x, testY = te$Class)
  expect_s3_class(pred, "data.frame")
  expect_true(all(
    c("obs", "pred", "model", "dataType", "object") %in% colnames(pred)
  ))
  expect_setequal(unique(pred$dataType), c("Training", "Test"))

  # extractProb: the same but with per-class probability columns
  prob <- extractProb(mods, testX = te_x, testY = te$Class)
  expect_s3_class(prob, "data.frame")
  expect_true(all(levels(tr$Class) %in% colnames(prob)))

  # plotClassProbs consumes the extractProb output
  expect_s3_class(plotClassProbs(prob), "trellis")
  expect_s3_class(plotClassProbs(prob, plotType = "densityplot"), "trellis")

  # plotObsVsPred consumes the extractPrediction output; for a factor outcome it
  # draws an accuracy dotplot
  expect_s3_class(plotObsVsPred(pred), "trellis")

  # unlabelled ("unknown") data is predicted with dataType "Unknown"
  unk <- extractPrediction(mods, unkX = te_x)
  expect_setequal(unique(unk$dataType), "Unknown")
  unk_prob <- extractProb(mods, unkX = te_x)
  expect_setequal(unique(unk_prob$dataType), "Unknown")
})

test_that("extractPrediction trims regression predictions to the bounds", {
  skip_on_cran()

  set.seed(1)
  tr <- SLC14_1(100)
  te <- SLC14_1(50)
  ctrl <- trainControl(method = "cv", number = 3, predictionBounds = c(0, NA))

  set.seed(2)
  fit <- train(y ~ ., data = tr, method = "lm", trControl = ctrl)
  pred <- extractPrediction(list(lm = fit), testX = te[, 1:20], testY = te$y)

  expect_s3_class(pred, "data.frame")
  # the lower bound of 0 is enforced on the predictions
  expect_true(min(pred$pred) >= 0)

  # for a numeric outcome plotObsVsPred draws an obs-vs-pred scatter; both the
  # shared-range and independent-range branches build a trellis object
  expect_s3_class(plotObsVsPred(pred), "trellis")
  expect_s3_class(plotObsVsPred(pred, equalRanges = FALSE), "trellis")
})

test_that("trimPredictions clamps regression predictions to bounds", {
  # numeric bounds clamp on both sides
  expect_identical(
    caret:::trimPredictions(c(-1, 5, 15), "Regression", c(0, 10)),
    c(0, 5, 10)
  )
  # a one-sided numeric bound (NA = no bound)
  expect_identical(
    caret:::trimPredictions(c(-1, 5, 15), "Regression", c(0, NA)),
    c(0, 5, 15)
  )
  # logical bounds use the supplied data limits
  expect_identical(
    caret:::trimPredictions(
      c(-1, 5, 15),
      "Regression",
      c(TRUE, TRUE),
      c(0, 10)
    ),
    c(0, 5, 10)
  )
  # classification predictions are left untouched
  expect_identical(
    caret:::trimPredictions(c(-1, 5, 15), "Classification", c(0, 10)),
    c(-1, 5, 15)
  )
})

test_that("trim_values applies bounds for numeric outcomes only", {
  # numeric bounds, single vector
  expect_identical(
    caret:::trim_values(c(-1, 5, 15), list(predictionBounds = c(0, 10)), TRUE),
    c(0, 5, 10)
  )
  # logical bounds fall back to the stored limits
  expect_identical(
    caret:::trim_values(
      c(-1, 5, 15),
      list(predictionBounds = c(TRUE, TRUE), yLimit = c(0, 10)),
      TRUE
    ),
    c(0, 5, 10)
  )
  # a list of prediction vectors is trimmed elementwise
  expect_identical(
    caret:::trim_values(
      list(a = c(-1, 5, 15)),
      list(predictionBounds = c(0, 10)),
      TRUE
    ),
    list(a = c(0, 5, 10))
  )
  # non-numeric (classification) outcomes are returned unchanged
  expect_identical(
    caret:::trim_values(c(-1, 5, 15), list(predictionBounds = c(0, 10)), FALSE),
    c(-1, 5, 15)
  )
})

test_that("extractProb requires models that produce probabilities", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    y ~ .,
    data = SLC14_1(80),
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_snapshot(extractProb(list(lm = fit)), error = TRUE)
})
