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
  expect_in(c("obs", "pred", "model", "dataType", "object"), colnames(pred))
  expect_setequal(unique(pred$dataType), c("Training", "Test"))

  # extractProb: the same but with per-class probability columns
  prob <- extractProb(mods, testX = te_x, testY = te$Class)
  expect_s3_class(prob, "data.frame")
  expect_in(levels(tr$Class), colnames(prob))

  # plotClassProbs consumes the extractProb output
  expect_s3_class(draw_trellis(plotClassProbs(prob)), "trellis")
  expect_s3_class(
    draw_trellis(plotClassProbs(prob, plotType = "densityplot")),
    "trellis"
  )

  # plotObsVsPred consumes the extractPrediction output; for a factor outcome it
  # draws an accuracy dotplot
  expect_s3_class(draw_trellis(plotObsVsPred(pred)), "trellis")

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
  expect_s3_class(draw_trellis(plotObsVsPred(pred)), "trellis")
  expect_s3_class(
    draw_trellis(plotObsVsPred(pred, equalRanges = FALSE)),
    "trellis"
  )
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
      list(predictionBounds = c(TRUE, TRUE), yLimits = c(0, 10)),
      TRUE
    ),
    c(0, 5, 10)
  )
  # a list of predictions with logical bounds is trimmed elementwise
  expect_identical(
    caret:::trim_values(
      list(c(-1, 5), c(15, 2)),
      list(predictionBounds = c(TRUE, TRUE), yLimits = c(0, 10)),
      TRUE
    ),
    list(c(0, 5), c(10, 2))
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

test_that("extractPrediction reports progress and handles unknowns", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3)
  )

  # a stray .outcome column is dropped from the test and unknown sets; the
  # verbose counts are all integers, so the snapshot is stable
  te <- iris[1:20, 1:4]
  te$.outcome <- iris$Species[1:20]
  unk <- iris[21:30, 1:4]
  unk$.outcome <- iris$Species[21:30]

  expect_snapshot(
    ep <- extractPrediction(
      list(fit),
      testX = te,
      testY = iris$Species[1:20],
      unkX = unk,
      verbose = TRUE
    )
  )
  # unnamed model lists get Object1, ... labels
  expect_identical(unique(as.character(ep$object)), "Object1")
  expect_setequal(
    unique(as.character(ep$dataType)),
    c("Training", "Test", "Unknown")
  )
  # unknown samples have no observed values (the "" placeholder is not a
  # factor level, so it becomes NA)
  expect_all_true(is.na(ep$obs[ep$dataType == "Unknown"]))
})

test_that("extractPrediction handles unknown-only regression extraction", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    mpg ~ .,
    data = mtcars,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )

  ep <- extractPrediction(
    list(lm = fit),
    unkX = mtcars[1:5, -1],
    unkOnly = TRUE
  )
  expect_identical(unique(as.character(ep$dataType)), "Unknown")
  # regression unknowns have missing observed values
  expect_all_true(is.na(ep$obs))
})

test_that("plotClassProbs conditions on the columns that vary", {
  skip_on_cran()

  dat <- engine_three_class()
  ctrl <- trainControl(method = "cv", number = 3, classProbs = TRUE)
  set.seed(6106)
  fit <- train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = ctrl
  )
  prob <- extractProb(list(knn = fit))

  # more than two classes puts each class in its own panel
  drawn <- draw_trellis(plotClassProbs(prob))
  expect_s3_class(drawn, "trellis")
  expect_gt(prod(dim(drawn)), 1)

  # with a single model and no test set there is nothing but the observed class
  # to condition on, and the density plot drops even that (it groups by it)
  expect_s3_class(
    draw_trellis(plotClassProbs(prob, plotType = "densityplot")),
    "trellis"
  )

  # the per-object panels can be turned off
  expect_s3_class(
    draw_trellis(plotClassProbs(prob, useObjects = FALSE)),
    "trellis"
  )
})
