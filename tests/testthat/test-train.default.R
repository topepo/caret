# Tests for train() itself (R/train.default.R) and, through it, the resampling
# workflows in R/workflows.R. One battery of very small fits sweeps the
# resampling methods and the options that change how results are collected, so
# the two files are covered together rather than separately.
#
# Fixtures: helper-engine-data.R (data) and helper-fake-models.R (custom
# `method` lists, including ones that fail on purpose).

# ------------------------------------------------------------------------------
# resampling methods

test_that("train fits with each bootstrap variant", {
  skip_on_cran()

  cls <- engine_two_class(60)

  for (m in c("boot", "boot632", "optimism_boot", "boot_all")) {
    set.seed(7712)
    fit <- train(
      Class ~ .,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = c(3, 5)),
      trControl = trainControl(
        method = m,
        number = 2,
        classProbs = TRUE,
        savePredictions = "all"
      )
    )
    expect_s3_class(fit, "train")
    expect_identical(fit$control$method, m)
    # every candidate is scored
    expect_identical(nrow(fit$results), 2L)
  }
})

test_that("train collects submodel predictions across the bootstrap variants", {
  skip_on_cran()
  skip_if_not_installed("pls")

  cls <- engine_two_class(60)

  # pls has a `loop`, so a single fit yields predictions for several components.
  # The optimism-bootstrap methods predict on the resampled training set, whose
  # rows repeat; that used to fail in predict.plsda() (issue #1515).
  for (m in c("boot", "boot632", "optimism_boot", "boot_all")) {
    set.seed(3196)
    fit <- train(
      Class ~ .,
      data = cls,
      method = "pls",
      tuneGrid = data.frame(ncomp = 1:3),
      trControl = trainControl(
        method = m,
        number = 2,
        classProbs = TRUE,
        savePredictions = "all"
      )
    )
    expect_identical(nrow(fit$results), 3L)
  }
})

test_that("train fits with leave-one-out resampling", {
  skip_on_cran()

  small <- engine_three_class()[c(1:5, 16:20, 31:35), ]
  set.seed(5230)
  fit <- train(
    Species ~ .,
    data = small,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "LOOCV")
  )
  expect_s3_class(fit, "train")
  expect_identical(fit$control$method, "LOOCV")
})

test_that("train collects submodels under leave-one-out resampling", {
  skip_on_cran()
  skip_if_not_installed("pls")

  small <- engine_three_class()[c(1:5, 16:20, 31:35), ]
  set.seed(1104)
  fit <- train(
    Species ~ .,
    data = small,
    method = "pls",
    tuneGrid = data.frame(ncomp = 1:2),
    trControl = trainControl(method = "LOOCV", classProbs = TRUE)
  )
  expect_identical(nrow(fit$results), 2L)
})

test_that("train fits with out-of-bag estimates", {
  skip_on_cran()
  skip_if_not_installed("randomForest")

  cls <- engine_two_class(60)
  set.seed(8891)
  fit <- train(
    Class ~ .,
    data = cls,
    method = "rf",
    ntree = 20,
    tuneGrid = data.frame(mtry = c(2, 3)),
    trControl = trainControl(method = "oob")
  )
  expect_s3_class(fit, "train")
  # out-of-bag results carry no resampled distribution
  expect_null(fit$resample)
})

test_that("train warns when the metric is unavailable for out-of-bag fits", {
  skip_on_cran()
  skip_if_not_installed("randomForest")

  reg <- engine_regression(40)
  set.seed(2977)
  # MAE is not among the out-of-bag statistics, so it is swapped out
  suppressWarnings(
    expect_snapshot_warning(
      train(
        y ~ .,
        data = reg,
        method = "rf",
        ntree = 20,
        tuneGrid = data.frame(mtry = 2),
        metric = "MAE",
        trControl = trainControl(method = "oob")
      )
    )
  )
})

test_that("train can skip resampling altogether", {
  skip_on_cran()

  cls <- engine_two_class(40)
  set.seed(4485)
  fit <- train(
    Class ~ .,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "none", classProbs = TRUE)
  )
  expect_s3_class(fit, "train")
  expect_identical(fit$control$method, "none")
  expect_s3_class(predict(fit, cls), "factor")
})

test_that("train can search the tuning space at random", {
  skip_on_cran()

  cls <- engine_two_class(50)
  set.seed(6014)
  fit <- train(
    Class ~ .,
    data = cls,
    method = "knn",
    tuneLength = 3,
    trControl = trainControl(method = "cv", number = 3, search = "random")
  )
  # a random grid may draw fewer distinct values than asked for
  expect_gte(nrow(fit$results), 1L)
  expect_lte(nrow(fit$results), 3L)
})

# ------------------------------------------------------------------------------
# how results are collected

test_that("train honours the returnResamp and savePredictions settings", {
  skip_on_cran()

  cls <- engine_two_class(50)

  all_resamp <- train(
    Class ~ .,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(
      method = "cv",
      number = 3,
      returnResamp = "all",
      savePredictions = TRUE
    )
  )
  # "all" keeps a row per resample and candidate
  expect_identical(nrow(all_resamp$resample), 6L)
  # a logical savePredictions is normalised to "all"
  expect_identical(all_resamp$control$savePredictions, "all")

  final_only <- train(
    Class ~ .,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(
      method = "cv",
      number = 3,
      returnResamp = "final",
      savePredictions = "final"
    )
  )
  expect_identical(nrow(final_only$resample), 3L)
  # only the winning candidate's predictions are kept
  expect_identical(nrow(unique(final_only$pred[, "k", drop = FALSE])), 1L)
})

test_that("train can drop the training data and time the model", {
  skip_on_cran()

  cls <- engine_two_class(40)
  set.seed(9503)
  fit <- train(
    Class ~ .,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(
      method = "cv",
      number = 3,
      returnData = FALSE,
      timingSamps = 5
    )
  )
  expect_null(fit$trainingData)
  # timing the prediction adds a third timing entry
  expect_contains(names(fit$times), "prediction")
})

test_that("train reports its progress and can trim the final model", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  reg <- engine_regression(40)
  set.seed(1425)
  # verboseIter prints one line per resample; the parameter values are integers
  expect_snapshot(
    fit <- train(
      y ~ .,
      data = reg,
      method = "rpart2",
      tuneGrid = data.frame(maxdepth = c(1, 2)),
      trControl = trainControl(
        method = "cv",
        number = 2,
        verboseIter = TRUE,
        trim = TRUE
      )
    )
  )
  expect_s3_class(fit, "train")
})

test_that("train accepts case weights", {
  skip_on_cran()

  reg <- engine_regression(40)
  set.seed(6338)
  wts <- runif(nrow(reg))
  fit <- train(
    y ~ .,
    data = reg,
    method = "lm",
    weights = wts,
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_s3_class(fit, "train")
})

test_that("train can pre-process inside the resampling loop", {
  skip_on_cran()

  cls <- engine_two_class(50)
  set.seed(3762)
  fit <- train(
    Class ~ .,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    preProcess = c("center", "scale", "pca"),
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_contains(names(fit$preProcess$method), c("center", "scale", "pca"))
})

test_that("train can balance the classes inside the resampling loop", {
  skip_on_cran()

  # an unbalanced two-class outcome
  cls <- engine_two_class(80)
  cls <- rbind(
    cls[cls$Class == "Class1", ][1:10, ],
    cls[cls$Class == "Class2", ]
  )

  for (samp in c("down", "up")) {
    set.seed(2048)
    fit <- train(
      Class ~ .,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = 3),
      trControl = trainControl(
        method = "cv",
        number = 3,
        sampling = samp
      )
    )
    expect_identical(fit$control$sampling$name, samp)
  }
})

test_that("train accepts a selection function by name or by value", {
  skip_on_cran()

  cls <- engine_two_class(50)
  grid <- data.frame(k = c(1, 5, 9))

  by_name <- train(
    Class ~ .,
    data = cls,
    method = "knn",
    tuneGrid = grid,
    trControl = trainControl(
      method = "cv",
      number = 3,
      selectionFunction = "oneSE"
    )
  )
  expect_contains(grid$k, by_name$bestTune$k)

  # a function is used as given; it indexes the results *after* the model's
  # own sort() has reordered them (knn sorts from the most to the least
  # regularised, so row 1 is the largest k)
  pick_first <- function(x, metric, maximize) 1
  by_value <- train(
    Class ~ .,
    data = cls,
    method = "knn",
    tuneGrid = grid,
    trControl = trainControl(
      method = "cv",
      number = 3,
      selectionFunction = pick_first
    )
  )
  sorted <- getModelInfo("knn", regex = FALSE)[[1]]$sort(by_value$results)
  expect_identical(by_value$bestTune$k, sorted$k[1])
})

test_that("train accepts user-supplied seeds and checks their shape", {
  skip_on_cran()

  cls <- engine_two_class(40)
  ctrl_args <- list(method = "cv", number = 3)

  # one seed per resample plus one for the final model, each long enough for
  # the number of candidates
  good <- c(lapply(1:3, function(i) rep(i, 2L)), list(99L))
  fit <- train(
    Class ~ .,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = do.call(trainControl, c(ctrl_args, list(seeds = good)))
  )
  expect_s3_class(fit, "train")

  too_short <- list(1L, 2L)
  expect_snapshot(
    train(
      Class ~ .,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = c(3, 5)),
      trControl = do.call(trainControl, c(ctrl_args, list(seeds = too_short)))
    ),
    error = TRUE
  )

  has_na <- good
  has_na[[1]][1] <- NA_integer_
  expect_snapshot(
    train(
      Class ~ .,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = c(3, 5)),
      trControl = do.call(trainControl, c(ctrl_args, list(seeds = has_na)))
    ),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------
# failing models

test_that("train warns about resamples whose fit failed", {
  skip_on_cran()

  reg <- engine_regression(40)
  # fail only the first fit, so the other resamples still produce results
  first_only <- make_custom_model(
    fail_when = local({
      calls <- 0L
      function(x, y) {
        calls <<- calls + 1L
        calls == 1L
      }
    })
  )

  suppressWarnings(
    expect_snapshot_warning(
      fit <- train(
        reg[, 1:3],
        reg$y,
        method = first_only,
        tuneLength = 2,
        trControl = trainControl(method = "cv", number = 3)
      )
    )
  )
  expect_s3_class(fit, "train")
})

test_that("train stops when every resample fails", {
  skip_on_cran()

  reg <- engine_regression(40)
  always <- make_custom_model(fail_when = function(x, y) TRUE)

  suppressWarnings(
    expect_snapshot(
      train(
        reg[, 1:3],
        reg$y,
        method = always,
        tuneLength = 2,
        trControl = trainControl(method = "cv", number = 3)
      ),
      error = TRUE,
      transform = mask_na_label
    )
  )
})

test_that("train warns when predictions fail in a resample", {
  skip_on_cran()

  reg <- engine_regression(40)
  bad_predict <- make_custom_model(pred_fails = TRUE)

  suppressWarnings(
    expect_snapshot(
      train(
        reg[, 1:3],
        reg$y,
        method = bad_predict,
        tuneLength = 2,
        trControl = trainControl(method = "cv", number = 3)
      ),
      error = TRUE,
      transform = mask_na_label
    )
  )
})

test_that("train passes the workflow debug flag through a tolerant fit", {
  skip_on_cran()

  reg <- engine_regression(30)
  tolerant <- make_custom_model()

  # `testing` is consumed by the workflow, and the custom fit ignores the extra
  # argument; the printed output is the workflow's own debug trace
  expect_snapshot(
    fit <- train(
      reg[, 1:3],
      reg$y,
      method = tolerant,
      tuneLength = 2,
      trControl = trainControl(method = "cv", number = 2),
      testing = TRUE
    )
  )
  expect_s3_class(fit, "train")
})

# ------------------------------------------------------------------------------
# methods on the fitted object

test_that("summary.train summarises the final model", {
  skip_on_cran()

  reg <- engine_regression(40)
  fit <- train(
    y ~ .,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "none")
  )
  expect_s3_class(summary(fit), "summary.lm")
})

test_that("residuals.train uses the final model or falls back to predicting", {
  skip_on_cran()

  reg <- engine_regression(40)
  lm_fit <- train(
    y ~ .,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "none")
  )
  # lm keeps its own residuals
  expect_length(residuals(lm_fit), nrow(reg))

  # knnreg does not, so the residuals are recomputed from the training data
  knn_fit <- train(
    y ~ .,
    data = reg,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "none")
  )
  expect_length(residuals(knn_fit), nrow(reg))

  # without the training data there is nothing to recompute from
  no_data <- train(
    y ~ .,
    data = reg,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "none", returnData = FALSE)
  )
  expect_snapshot(residuals(no_data), error = TRUE)
})

test_that("residuals.train refuses classification models", {
  skip_on_cran()

  cls <- engine_two_class(40)
  fit <- train(
    Class ~ .,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "none")
  )
  expect_snapshot(residuals(fit), error = TRUE)
})

test_that("fitted.train returns the final model's fitted values", {
  skip_on_cran()

  reg <- engine_regression(40)
  lm_fit <- train(
    y ~ .,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "none")
  )
  expect_length(fitted(lm_fit), nrow(reg))

  # a model without stored fitted values predicts the training data instead
  knn_fit <- train(
    y ~ .,
    data = reg,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "none")
  )
  expect_length(fitted(knn_fit), nrow(reg))
})

# ------------------------------------------------------------------------------
# train.formula

test_that("train.formula accepts a matrix as the data argument", {
  skip_on_cran()

  reg <- engine_regression(40)
  fit <- train(
    y ~ .,
    data = as.matrix(reg),
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_s3_class(fit, "train")
})

test_that("train.formula stops when na.action removes every row", {
  skip_on_cran()

  reg <- engine_regression(20)
  reg$x1 <- NA
  expect_snapshot(
    train(
      y ~ .,
      data = reg,
      method = "lm",
      na.action = na.omit,
      trControl = trainControl(method = "cv", number = 3)
    ),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------
# the optimism bootstrap and repeated rows

test_that("the optimism bootstrap works for a single-row grid", {
  skip_on_cran()
  skip_if_not_installed("pls")

  # The optimism bootstrap scores the resampled training set itself, so the data
  # being predicted contains the same row several times. That used to error out
  # of predict.plsda() with "duplicate 'row.names' are not allowed" (issue
  # #1515). The grids with sub-models are covered by the bootstrap sweep above;
  # this is the single-candidate case, which failed the same way.
  cls <- engine_two_class(60)

  for (m in c("optimism_boot", "boot_all")) {
    set.seed(9021)
    fit <- train(
      Class ~ .,
      data = cls,
      method = "pls",
      tuneGrid = data.frame(ncomp = 2),
      metric = "ROC",
      trControl = trainControl(
        method = m,
        number = 2,
        classProbs = TRUE,
        summaryFunction = twoClassSummary
      )
    )
    expect_identical(nrow(fit$results), 1L)
    expect_all_false(is.na(fit$results$ROC))
  }
})

test_that("the optimism bootstrap works without class probabilities", {
  skip_on_cran()

  # without classProbs the extra predictions get a frame of missing probability
  # values instead, which is a separate branch of optimism_xy()
  reg <- engine_regression(40)

  set.seed(3308)
  fit <- train(
    reg[, 1:3],
    reg$y,
    method = "lm",
    trControl = trainControl(method = "optimism_boot", number = 2)
  )
  expect_contains(names(fit$results), c("RMSEApparent", "RMSEOptimism"))
  expect_all_false(is.na(fit$results$RMSE))
})

test_that("the optimism bootstrap carries case weights into the extra summaries", {
  skip_on_cran()
  skip_if_not_installed("pls")

  reg <- engine_regression(40)
  wts <- rep(c(1, 2), length.out = nrow(reg))

  # one fit per candidate: the weights are attached to each extra prediction set
  set.seed(1177)
  plain <- train(
    reg[, 1:3],
    reg$y,
    method = "lm",
    weights = wts,
    trControl = trainControl(method = "optimism_boot", number = 2)
  )
  expect_all_false(is.na(plain$results$RMSE))

  # and with sub-models, where the extra predictions are a list per candidate
  set.seed(1177)
  subs <- train(
    reg[, 1:3],
    reg$y,
    method = "pls",
    tuneGrid = data.frame(ncomp = 1:3),
    weights = wts,
    trControl = trainControl(method = "boot_all", number = 2)
  )
  expect_identical(nrow(subs$results), 3L)
  expect_all_false(is.na(subs$results$RMSE))
})
