# Unit tests for the small internal helpers in R/train_recipes.R, plus one
# end-to-end check that train() accepts a recipe. The heavy resampling workflows
# (train_rec, loo_train_rec, oob_train_rec, train_adapt_rec) are only reachable
# through train(), so they are covered by the integration test at the bottom.

# ------------------------------------------------------------------------------

test_that("get_vector passes vectors through and unwraps one-column frames", {
  # not a data frame: returned untouched
  expect_identical(caret:::get_vector(1:5), 1:5)
  # a single-column frame is turned into the bare column
  expect_identical(caret:::get_vector(data.frame(a = 1:3)), 1:3)
  # anything wider is ambiguous and should error
  expect_snapshot(
    caret:::get_vector(data.frame(a = 1:3, b = 4:6)),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------

test_that("role_cols returns the variable names for a given recipe role", {
  info <- list(
    term_info = data.frame(
      variable = c("x1", "x2", "y"),
      role = c("predictor", "predictor", "outcome"),
      stringsAsFactors = FALSE
    )
  )
  expect_identical(caret:::role_cols(info, "predictor"), c("x1", "x2"))
  expect_identical(caret:::role_cols(info, "outcome"), "y")
  # a role that isn't present gives an empty result, not an error
  expect_identical(caret:::role_cols(info, "case weight"), character(0))
})

# ------------------------------------------------------------------------------

test_that("preproc_dots warns only about leftover preProc arguments", {
  # old-style preProc* arguments are ignored with a heads-up
  expect_snapshot(caret:::preproc_dots(preProcOptions = list(k = 5)))
  # unrelated arguments pass quietly
  expect_no_warning(caret:::preproc_dots(foo = 1))
})

# ------------------------------------------------------------------------------

test_that("model_failed spots the ways a recipe model fit can fail", {
  err <- try(stop("boom"), silent = TRUE)
  expect_true(caret:::model_failed(err))
  expect_true(caret:::model_failed(list(fit = err)))
  expect_true(caret:::model_failed(list(recipe = err)))
  # a fit with working parts is fine
  expect_false(caret:::model_failed(list(fit = 1, recipe = 1)))
})

# ------------------------------------------------------------------------------

test_that("pred_failed only flags try-errors", {
  expect_true(caret:::pred_failed(try(stop("boom"), silent = TRUE)))
  expect_false(caret:::pred_failed(1:5))
})

# ------------------------------------------------------------------------------

test_that("train() fits and predicts from a recipe", {
  skip_on_cran()

  rec <- recipes::recipe(Species ~ ., data = iris)
  rec <- recipes::step_normalize(rec, recipes::all_predictors())

  set.seed(1)
  fit <- train(
    rec,
    data = iris,
    method = "knn",
    trControl = trainControl(method = "cv", number = 3)
  )

  expect_s3_class(fit, "train.recipe")
  expect_false(is.null(fit$finalModel))
  expect_false(is.null(fit$recipe))
  expect_length(predict(fit, iris), nrow(iris))
})

# ------------------------------------------------------------------------------

test_that("train() with a recipe returns class probabilities", {
  skip_on_cran()

  rec <- recipes::recipe(Species ~ ., data = iris)
  rec <- recipes::step_normalize(rec, recipes::all_predictors())

  set.seed(1)
  fit <- train(
    rec,
    data = iris,
    method = "knn",
    trControl = trainControl(method = "cv", number = 3, classProbs = TRUE)
  )

  probs <- predict(fit, iris, type = "prob")
  expect_identical(colnames(probs), levels(iris$Species))
  expect_identical(unname(rowSums(probs)), rep(1, nrow(iris)))
})

# ------------------------------------------------------------------------------

test_that("train() drives a recipe through leave-one-out resampling", {
  skip_on_cran()

  # a small, balanced subset keeps the LOOCV fold count manageable
  small <- iris[c(1:12, 51:62, 101:112), ]
  rec <- recipes::recipe(Species ~ ., data = small)
  rec <- recipes::step_normalize(rec, recipes::all_predictors())

  set.seed(1)
  fit <- train(
    rec,
    data = small,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "LOOCV")
  )

  expect_s3_class(fit, "train.recipe")
  expect_identical(fit$control$method, "LOOCV")
  expect_length(predict(fit, small), nrow(small))
})

# ------------------------------------------------------------------------------

test_that("train() drives a recipe through adaptive resampling", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  rec <- recipes::recipe(Species ~ ., data = iris)
  rec <- recipes::step_normalize(rec, recipes::all_predictors())

  k_grid <- c(1, 5, 9, 13, 17)

  set.seed(1)
  fit <- suppressWarnings(train(
    rec,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = k_grid),
    trControl = trainControl(
      method = "adaptive_cv",
      number = 10,
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  ))

  expect_s3_class(fit, "train.recipe")
  expect_identical(fit$control$method, "adaptive_cv")
  expect_true(fit$bestTune$k %in% k_grid)
})

test_that("themis::step_upsample balances the classes inside train", {
  skip_on_cran()
  skip_if_not_installed("themis")
  withr::local_seed(2542)
  dat <- twoClassSim(200, intercept = 6)

  rec <-
    recipe(Class ~ TwoFactor1 + TwoFactor2 + Linear01, data = dat) %>%
    themis::step_upsample(Class, seed = 534)

  mod <- train(
    rec,
    dat,
    method = "knn",
    trControl = trainControl(method = "cv")
  )
  expect_equal(
    rep(max(table(dat$Class)), 2),
    as.vector(table(mod$finalModel$learn$y)),
    ignore_attr = TRUE
  )
})

# ------------------------------------------------------------------------------

test_that("train() drives a recipe through the optimism bootstrap", {
  skip_on_cran()

  # optimism_boot scores the resampled training set as well as the held-out
  # rows, which is the only path through optimism_rec()
  rec <- recipes::recipe(Species ~ ., data = iris)
  rec <- recipes::step_normalize(rec, recipes::all_predictors())

  set.seed(4132)
  fit <- train(
    rec,
    data = iris[c(1:20, 51:70, 101:120), ],
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "optimism_boot", number = 2)
  )
  expect_s3_class(fit, "train.recipe")
  expect_identical(nrow(fit$results), 2L)
  # the apparent performance and the optimism estimate sit alongside the
  # ordinary columns
  expect_contains(
    names(fit$results),
    c("Accuracy", "AccuracyApparent", "AccuracyOptimism")
  )
  expect_all_false(is.na(fit$results$Accuracy))
})

test_that("train() runs the recipe optimism bootstrap with class probabilities", {
  skip_on_cran()

  rec <- recipes::recipe(Species ~ ., data = iris)
  rec <- recipes::step_normalize(rec, recipes::all_predictors())

  set.seed(8877)
  fit <- train(
    rec,
    data = iris[c(1:20, 51:70, 101:120), ],
    method = "knn",
    tuneGrid = data.frame(k = 5),
    metric = "logLoss",
    trControl = trainControl(
      method = "boot_all",
      number = 2,
      classProbs = TRUE,
      summaryFunction = multiClassSummary
    )
  )
  expect_identical(nrow(fit$results), 1L)
  expect_all_false(is.na(fit$results$logLoss))
})

test_that("train() runs the recipe optimism bootstrap with sub-models", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  # rpart scores several cp values from one fit, so the sub-model branch of
  # optimism_rec() has to line the extra predictions up with the candidates
  rec <- recipes::recipe(Species ~ ., data = iris)

  set.seed(6390)
  fit <- train(
    rec,
    data = iris[c(1:20, 51:70, 101:120), ],
    method = "rpart",
    tuneGrid = data.frame(cp = c(0.01, 0.1, 0.3)),
    trControl = trainControl(method = "optimism_boot", number = 2)
  )
  expect_identical(nrow(fit$results), 3L)
  expect_contains(
    names(fit$results),
    c("AccuracyApparent", "AccuracyOptimism")
  )
})

test_that("the recipe optimism bootstrap combines sub-models and probabilities", {
  skip_on_cran()
  skip_if_not_installed("rpart")
  skip_if_not_installed("MLmetrics")

  # sub-models and class probabilities together: the extra predictions are a
  # list per candidate and each has to be bound to its own probability frame
  rec <- recipes::recipe(Species ~ ., data = iris)

  set.seed(3121)
  fit <- train(
    rec,
    data = iris[c(1:20, 51:70, 101:120), ],
    method = "rpart",
    tuneGrid = data.frame(cp = c(0.01, 0.1)),
    metric = "logLoss",
    trControl = trainControl(
      method = "optimism_boot",
      number = 2,
      classProbs = TRUE,
      summaryFunction = multiClassSummary
    )
  )
  expect_identical(nrow(fit$results), 2L)
  expect_contains(names(fit$results), c("logLossApparent", "logLossOptimism"))
})

# ------------------------------------------------------------------------------
# recipe roles

test_that("train() uses a case-weight role from the recipe", {
  skip_on_cran()

  reg <- engine_regression(60)
  reg$wt <- rep(c(1, 2), length.out = nrow(reg))

  rec <- recipes::recipe(y ~ ., data = reg)
  rec <- recipes::update_role(rec, wt, new_role = "case weight")

  set.seed(4823)
  fit <- train(
    rec,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  # the weights column is not a predictor, and the model is fit weighted
  expect_disjoint(fit$finalModel$xNames, "wt")
  expect_identical(unname(fit$finalModel$weights), reg$wt)
})

test_that("train() only allows one case-weight column", {
  skip_on_cran()

  reg <- engine_regression(30)
  reg$wt1 <- 1
  reg$wt2 <- 2

  rec <- recipes::recipe(y ~ ., data = reg)
  rec <- recipes::update_role(rec, wt1, wt2, new_role = "case weight")

  expect_snapshot(
    train(
      rec,
      data = reg,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3)
    ),
    error = TRUE
  )
})

test_that("train() passes performance variables to the summary function", {
  skip_on_cran()

  reg <- engine_regression(60)
  reg$extra <- seq_len(nrow(reg))

  rec <- recipes::recipe(y ~ ., data = reg)
  rec <- recipes::update_role(rec, extra, new_role = "performance var")

  # the summary function sees the extra column alongside the predictions
  saw_extra <- function(data, lev = NULL, model = NULL) {
    c(HasExtra = as.numeric("extra" %in% names(data)))
  }

  set.seed(1935)
  fit <- train(
    rec,
    data = reg,
    method = "lm",
    metric = "HasExtra",
    maximize = TRUE,
    trControl = trainControl(
      method = "cv",
      number = 3,
      summaryFunction = saw_extra
    )
  )
  expect_all_equal(fit$resample$HasExtra, 1)
  expect_disjoint(fit$finalModel$xNames, "extra")
})

test_that("train() rejects a recipe with several outcomes", {
  skip_on_cran()

  reg <- engine_regression(30)
  reg$y2 <- reg$y * 2

  rec <- recipes::recipe(y + y2 ~ x1 + x2 + x3, data = reg)
  expect_snapshot(
    train(
      rec,
      data = reg,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3, sampling = "down")
    ),
    error = TRUE
  )
})

test_that("train() sub-samples a recipe before and after preparing it", {
  skip_on_cran()

  # a two-class problem with an imbalance to correct
  cls <- engine_three_class()[c(1:15, 16:20, 31:35), ]
  cls$Species <- factor(
    ifelse(cls$Species == "setosa", "one", "two"),
    levels = c("one", "two")
  )
  rec <- recipes::recipe(Species ~ ., data = cls)

  # `first` says whether the sub-sampling happens on the raw data or after the
  # recipe has been prepared; the built-in wrapper is used because it takes just
  # `x` and `y`, as trainControl() requires
  down_func <- getSamplingInfo("down", regex = FALSE)[[1]]

  for (first in c(TRUE, FALSE)) {
    set.seed(7791)
    fit <- train(
      rec,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = 3),
      trControl = trainControl(
        method = "cv",
        number = 3,
        sampling = list(name = "down", func = down_func, first = first)
      )
    )
    expect_s3_class(fit, "train.recipe")
    # the stored training data is the original; it is the fits that are
    # down-sampled, so the final model has seen a balanced set
    expect_all_equal(as.vector(table(fit$finalModel$learn$y)), 10L)
  }
})

# ------------------------------------------------------------------------------
# recipe workflow: failures, debugging and prediction shapes

test_that("train() reports a recipe model that fails in a resample", {
  skip_on_cran()

  reg <- engine_regression(40)
  rec <- recipes::recipe(y ~ ., data = reg)
  # exactly one fold holds out the largest outcome, so exactly one fit fails,
  # whatever the fold sizes turn out to be
  sometimes <- make_custom_model(
    fail_when = function(x, y) max(y) < max(reg$y)
  )

  suppressWarnings(
    expect_snapshot_warning(
      fit <- train(
        rec,
        data = reg,
        method = sometimes,
        tuneLength = 2,
        trControl = trainControl(method = "cv", number = 3)
      )
    )
  )
  expect_s3_class(fit, "train.recipe")
})

test_that("train() reports a recipe model whose predictions fail", {
  skip_on_cran()

  reg <- engine_regression(40)
  rec <- recipes::recipe(y ~ ., data = reg)
  bad_predict <- make_custom_model(pred_fails = TRUE)

  suppressWarnings(
    expect_snapshot(
      train(
        rec,
        data = reg,
        method = bad_predict,
        tuneLength = 2,
        trControl = trainControl(method = "cv", number = 3)
      ),
      error = TRUE,
      transform = mask_na_label
    )
  )
})

test_that("train() prints its progress for a recipe", {
  skip_on_cran()

  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)
  tolerant <- make_custom_model()

  # verboseIter names each resample and candidate as it goes
  set.seed(2716)
  expect_snapshot(
    fit <- train(
      rec,
      data = reg,
      method = tolerant,
      tuneLength = 2,
      trControl = trainControl(method = "cv", number = 2, verboseIter = TRUE)
    )
  )
  expect_s3_class(fit, "train.recipe")
})

test_that("train() passes the debug flag through the recipe workflow", {
  skip_on_cran()

  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)
  tolerant <- make_custom_model()

  # The trace prints the fitted recipe, and that print is cli-formatted: its
  # rule width depends on the console, so the text is matched rather than
  # snapshotted. Both streams are captured, since the recipe prints as a message.
  set.seed(2716)
  notes <- NULL
  # the constant predictions leave R-squared undefined, which the workflow warns
  # about; the trace is what is under test here
  printed <- capture.output(
    notes <- capture.output(
      fit <- suppressWarnings(train(
        rec,
        data = reg,
        method = tolerant,
        tuneLength = 2,
        trControl = trainControl(method = "cv", number = 2),
        testing = TRUE
      )),
      type = "message"
    )
  )
  joined <- paste(c(printed, notes), collapse = " ")
  # the workflow says where it is, and shows the model, the predictions and the
  # resampled results as it goes
  expect_match(joined, "pre-model")
  expect_match(joined, "custom_fit")
  expect_match(joined, "Recipe")
  expect_match(joined, "RMSE")
  expect_s3_class(fit, "train.recipe")
})

test_that("recipe predictions are reduced to a vector", {
  skip_on_cran()

  # a model whose predict method returns a one-column matrix: the workflow takes
  # the first column so the predictions line up with the outcome
  matrix_pred <- make_custom_model()
  matrix_pred$predict <- function(modelFit, newdata, submodels = NULL) {
    matrix(rep(modelFit$tuneValue$shift, nrow(newdata)), ncol = 1)
  }

  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)

  set.seed(5502)
  # the predictions are constant within a candidate, so R-squared is undefined
  # and the workflow says so; RMSE is what matters here
  fit <- suppressWarnings(train(
    rec,
    data = reg,
    method = matrix_pred,
    tuneLength = 2,
    trControl = trainControl(method = "cv", number = 2)
  ))
  expect_all_false(is.na(fit$results$RMSE))
})

test_that("recipe class probabilities are ordered by the class levels", {
  skip_on_cran()

  # The prob function returns a matrix with the columns the wrong way round. The
  # levels are read with levels(<fit>), so the fit has to carry them as an
  # attribute the way a real model object would.
  reversed <- make_custom_model()
  reversed$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
    structure(list(lev = lev), levels = lev, class = "custom_fit")
  }
  reversed$prob <- function(modelFit, newdata, submodels = NULL) {
    out <- cbind(0.25, 0.75)
    colnames(out) <- rev(levels(modelFit))
    out[rep(1, nrow(newdata)), , drop = FALSE]
  }

  cls <- engine_two_class(40)
  rec <- recipes::recipe(Class ~ ., data = cls)

  set.seed(3480)
  fit <- train(
    rec,
    data = cls,
    method = reversed,
    tuneLength = 2,
    trControl = trainControl(
      method = "cv",
      number = 2,
      classProbs = TRUE,
      savePredictions = "all"
    )
  )
  # Class1 is the first level, so it must be the first probability column
  expect_identical(
    names(fit$pred)[names(fit$pred) %in% levels(cls$Class)],
    levels(cls$Class)
  )
  expect_all_equal(fit$pred$Class1, 0.75)
})

# ------------------------------------------------------------------------------
# the other recipe workflows

test_that("leave-one-out resampling works with a recipe and class probabilities", {
  skip_on_cran()

  small <- engine_three_class()[c(1:6, 16:21, 31:36), ]
  rec <- recipes::recipe(Species ~ ., data = small)
  rec <- recipes::step_normalize(rec, recipes::all_predictors())

  set.seed(4165)
  fit <- train(
    rec,
    data = small,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "LOOCV", classProbs = TRUE)
  )
  expect_identical(fit$control$method, "LOOCV")
  expect_identical(nrow(fit$results), 2L)
  expect_all_false(is.na(fit$results$Accuracy))
})

test_that("out-of-bag resampling works with a recipe", {
  skip_on_cran()
  skip_if_not_installed("randomForest")

  cls <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = cls)

  set.seed(9268)
  # randomForest announces the ggplot2 object it masks when caret attaches it
  fit <- suppressMessages(train(
    rec,
    data = cls,
    method = "rf",
    tuneGrid = data.frame(mtry = c(2, 3)),
    ntree = 20,
    trControl = trainControl(method = "oob")
  ))
  # the model reports its own out-of-bag estimates, so there are no resamples
  expect_identical(fit$control$method, "oob")
  expect_identical(nrow(fit$results), 2L)
  expect_null(fit$resample)
})

# ------------------------------------------------------------------------------
# adaptive resampling with a recipe

test_that("the recipe race collects class probabilities and predictions", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  cls <- engine_two_class(60)
  rec <- recipes::recipe(Class ~ ., data = cls)
  rec <- recipes::step_normalize(rec, recipes::all_predictors())

  set.seed(7920)
  fit <- suppressWarnings(train(
    rec,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5, 7, 9)),
    metric = "ROC",
    trControl = trainControl(
      method = "adaptive_cv",
      number = 6,
      classProbs = TRUE,
      savePredictions = "all",
      summaryFunction = twoClassSummary,
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  ))
  expect_s3_class(fit, "train.recipe")
  expect_contains(names(fit$pred), c("Class1", "Class2", "obs", "rowIndex"))
  expect_all_false(is.na(fit$results$ROC))
})

test_that("the recipe race scores sub-models from one fit", {
  skip_on_cran()
  skip_if_not_installed("nlme")
  skip_if_not_installed("rpart")

  cls <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = cls)

  set.seed(5108)
  fit <- suppressWarnings(train(
    rec,
    data = cls,
    method = "rpart",
    tuneGrid = data.frame(cp = c(0.01, 0.05, 0.1, 0.3)),
    trControl = trainControl(
      method = "adaptive_cv",
      number = 6,
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  ))
  expect_gte(nrow(fit$results), 1L)
  expect_in(fit$bestTune$cp, c(0.01, 0.05, 0.1, 0.3))
})

test_that("the recipe race works for regression with a case-weight role", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  reg <- engine_regression(60)
  reg$wt <- rep(c(1, 2), length.out = nrow(reg))
  rec <- recipes::recipe(y ~ ., data = reg)
  rec <- recipes::update_role(rec, wt, new_role = "case weight")

  set.seed(2648)
  fit <- suppressWarnings(train(
    rec,
    data = reg,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5, 7, 9)),
    trControl = trainControl(
      method = "adaptive_cv",
      number = 6,
      savePredictions = "final",
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  ))
  expect_identical(fit$modelType, "Regression")
  expect_all_false(is.na(fit$results$RMSE))
})

test_that("the recipe race runs over bootstrap and group splits", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  cls <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = cls)

  for (m in c("adaptive_boot", "adaptive_LGOCV")) {
    set.seed(8836)
    fit <- suppressWarnings(train(
      rec,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = c(3, 5, 7, 9)),
      trControl = trainControl(
        method = m,
        number = 6,
        p = 0.75,
        adaptive = list(min = 3, alpha = 0.05, method = "BT", complete = TRUE)
      )
    ))
    expect_identical(fit$control$method, m)
    expect_all_false(is.na(fit$results$Accuracy))
  }
})

test_that("the recipe race can stop as soon as one model is left", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  cls <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = cls)

  set.seed(3355)
  fit <- suppressWarnings(train(
    rec,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = c(1, 5, 9, 13, 17)),
    trControl = trainControl(
      method = "adaptive_cv",
      number = 10,
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = FALSE)
    )
  ))
  expect_s3_class(fit, "train.recipe")
  expect_in(fit$bestTune$k, c(1, 5, 9, 13, 17))
})

test_that("the recipe race reports a model that fails everywhere", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  reg <- engine_regression(40)
  rec <- recipes::recipe(y ~ ., data = reg)
  always <- make_custom_model(fail_when = function(x, y) TRUE)

  suppressWarnings(
    expect_snapshot(
      train(
        rec,
        data = reg,
        method = always,
        tuneLength = 2,
        trControl = trainControl(
          method = "adaptive_cv",
          number = 4,
          adaptive = list(
            min = 2,
            alpha = 0.05,
            method = "gls",
            complete = TRUE
          )
        )
      ),
      error = TRUE
    )
  )
})

test_that("the recipe race passes the debug flag through", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)
  tolerant <- make_custom_model()

  # as above, the trace contains a cli-formatted recipe print, so it is matched
  # rather than snapshotted
  set.seed(4471)
  notes <- NULL
  printed <- capture.output(
    notes <- capture.output(
      fit <- suppressWarnings(train(
        rec,
        data = reg,
        method = tolerant,
        tuneLength = 2,
        trControl = trainControl(
          method = "adaptive_cv",
          number = 4,
          verboseIter = TRUE,
          adaptive = list(
            min = 2,
            alpha = 0.05,
            method = "gls",
            complete = TRUE
          )
        ),
        testing = TRUE
      )),
      type = "message"
    )
  )
  joined <- paste(c(printed, notes), collapse = " ")
  expect_match(joined, "pre-model")
  # verboseIter names the resample and the candidate being fit
  expect_match(joined, "Fold1")
  expect_match(joined, "shift=")
  expect_s3_class(fit, "train.recipe")
})

test_that("the recipe race carries on when a model fails in one resample", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  reg <- engine_regression(60)
  rec <- recipes::recipe(y ~ ., data = reg)
  # exactly one fold holds out the largest outcome, so exactly one fit fails
  sometimes <- make_custom_model(
    fail_when = function(x, y) max(y) < max(reg$y)
  )

  set.seed(6693)
  expect_snapshot(
    fit <- train(
      rec,
      data = reg,
      method = sometimes,
      tuneLength = 2,
      trControl = trainControl(
        method = "adaptive_cv",
        number = 5,
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )
  expect_s3_class(fit, "train.recipe")
})

test_that("the recipe race reports its progress", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  cls <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = cls)

  set.seed(8298)
  expect_snapshot(
    fit <- suppressWarnings(train(
      rec,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = c(1, 9, 17)),
      trControl = trainControl(
        method = "adaptive_cv",
        number = 5,
        verboseIter = TRUE,
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )),
    transform = mask_decimals
  )
  expect_s3_class(fit, "train.recipe")
})

test_that("leave-one-out resampling scores recipe sub-models", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  small <- engine_three_class()[c(1:6, 16:21, 31:36), ]
  rec <- recipes::recipe(Species ~ ., data = small)

  set.seed(1204)
  fit <- train(
    rec,
    data = small,
    method = "rpart",
    tuneGrid = data.frame(cp = c(0.01, 0.1, 0.3)),
    trControl = trainControl(method = "LOOCV", classProbs = TRUE)
  )
  # one fit per held-out row scores all three cp values
  expect_identical(nrow(fit$results), 3L)
  expect_all_false(is.na(fit$results$Accuracy))
})

test_that("leave-one-out resampling reports a failing recipe model", {
  skip_on_cran()

  small <- engine_regression(12)
  rec <- recipes::recipe(y ~ ., data = small)
  always <- make_custom_model(fail_when = function(x, y) TRUE)

  suppressWarnings(
    expect_snapshot(
      train(
        rec,
        data = small,
        method = always,
        tuneLength = 2,
        trControl = trainControl(method = "LOOCV")
      ),
      error = TRUE,
      transform = mask_na_label
    )
  )
})

# ------------------------------------------------------------------------------
# validation, the recipe interface's mirror of test-train.default-validation.R

test_that("train rejects a recipe whose outcome the model cannot handle", {
  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)
  # lda is classification only
  expect_snapshot(train(rec, data = reg, method = "lda"), error = TRUE)
})

test_that("train wants a character matrix for the string kernels", {
  skip_if_not_installed("kernlab")

  reg <- engine_regression(20)
  rec <- recipes::recipe(y ~ ., data = reg)
  # a recipe always yields a data frame, so that is the shape reported
  expect_snapshot(
    train(rec, data = reg, method = "svmSpectrumString"),
    error = TRUE
  )
})

test_that("train warns about a two-valued numeric outcome from a recipe", {
  reg <- engine_regression(30)
  reg$y <- rep(c(0, 1), length.out = nrow(reg))
  rec <- recipes::recipe(y ~ ., data = reg)

  expect_snapshot_warning(
    train(
      rec,
      data = reg,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3)
    )
  )
})

test_that("train refuses sampling for a recipe with a numeric outcome", {
  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)
  expect_snapshot(
    train(
      rec,
      data = reg,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3, sampling = "down")
    ),
    error = TRUE
  )
})

test_that("train rejects a recipe outcome level with no data", {
  cls <- engine_three_class()
  cls$Species <- factor(cls$Species, levels = c(levels(cls$Species), "empty"))
  rec <- recipes::recipe(Species ~ ., data = cls)

  expect_snapshot(train(rec, data = cls, method = "lda"), error = TRUE)
})

test_that("train needs valid level names for recipe class probabilities", {
  cls <- engine_three_class()
  levels(cls$Species) <- c("one", "2 two", "three")
  rec <- recipes::recipe(Species ~ ., data = cls)

  expect_snapshot(
    train(
      rec,
      data = cls,
      method = "lda",
      trControl = trainControl(method = "cv", number = 3, classProbs = TRUE)
    ),
    error = TRUE
  )
})

test_that("train checks the metric against the recipe's outcome type", {
  cls <- engine_three_class()
  cls_rec <- recipes::recipe(Species ~ ., data = cls)
  reg <- engine_regression(30)
  reg_rec <- recipes::recipe(y ~ ., data = reg)

  # regression metrics for a factor outcome, and the other way round
  expect_snapshot(
    train(cls_rec, data = cls, method = "lda", metric = "RMSE"),
    error = TRUE
  )
  expect_snapshot(
    train(reg_rec, data = reg, method = "lm", metric = "Kappa"),
    error = TRUE
  )
  # the ROC curve needs probabilities to be computed from
  expect_snapshot(
    train(cls_rec, data = cls, method = "lda", metric = "ROC"),
    error = TRUE
  )
})

test_that("train drops recipe class probabilities it cannot produce", {
  skip_on_cran()

  cls <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = cls)
  # `prob` has to be present for the method list to be accepted, so it is left
  # in place but is not a function
  no_prob <- make_custom_model()
  no_prob$prob <- NA

  set.seed(8817)
  expect_snapshot_warning(
    fit <- train(
      rec,
      data = cls,
      method = no_prob,
      tuneLength = 2,
      trControl = trainControl(method = "cv", number = 2, classProbs = TRUE)
    )
  )
  expect_false(fit$control$classProbs)
})

test_that("train drops class probabilities for a recipe regression outcome", {
  skip_on_cran()

  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)

  expect_snapshot_warning(
    fit <- train(
      rec,
      data = reg,
      method = "lm",
      trControl = trainControl(method = "cv", number = 2, classProbs = TRUE)
    )
  )
  expect_false(fit$control$classProbs)
})

test_that("train validates the recipe fit's other options", {
  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)

  # `preProcess` is not checked here: the recipe does the pre-processing, so
  # train.recipe ignores the argument rather than validating it
  expect_snapshot(
    train(
      rec,
      data = reg,
      method = "lm",
      trControl = trainControl(method = "cv", savePredictions = "some")
    ),
    error = TRUE
  )
  # adaptive resampling needs something to choose between
  expect_snapshot(
    train(
      rec,
      data = reg,
      method = "knn",
      tuneGrid = data.frame(k = 5),
      trControl = trainControl(method = "adaptive_cv", number = 4)
    ),
    error = TRUE
  )
})

test_that("train checks a recipe fit's tuning grid", {
  cls <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = cls)

  expect_snapshot(
    train(rec, data = cls, method = "knn", tuneGrid = data.frame(bogus = 5)),
    error = TRUE
  )
  expect_snapshot(
    train(
      rec,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = 5, bogus = 1)
    ),
    error = TRUE
  )
})

test_that("train checks what a recipe model's loop function returns", {
  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)
  bad_loop <- make_custom_model()
  bad_loop$loop <- function(grid) list(nonsense = grid)

  expect_snapshot(
    train(rec, data = reg, method = bad_loop, tuneLength = 2),
    error = TRUE
  )
})

test_that("train accepts logical savePredictions and dot-named grids", {
  skip_on_cran()

  cls <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = cls)

  set.seed(1633)
  fit <- train(
    rec,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(.k = c(3, 5)),
    trControl = trainControl(
      method = "cv",
      number = 3,
      savePredictions = TRUE
    )
  )
  # TRUE means "all", and the dots are stripped from the grid names
  expect_identical(fit$control$savePredictions, "all")
  expect_in("k", names(fit$results))
})

test_that("the recipe race fills in sub-model predictions when a fit fails", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  # as in test-adaptive.R: the fit fails for whichever resample holds out the
  # sentinel row, so exactly one of them fails
  dat <- engine_sentinel_data(60)
  rec <- recipes::recipe(y ~ ., data = dat)
  failing <- make_submodel_model(fail_fit = TRUE)

  set.seed(5590)
  expect_snapshot(
    fit <- train(
      rec,
      data = dat,
      method = failing,
      tuneLength = 3,
      trControl = trainControl(
        method = "adaptive_cv",
        number = 5,
        classProbs = TRUE,
        savePredictions = "all",
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )
  expect_identical(nrow(fit$results), 3L)
  expect_contains(names(fit$pred), c("one", "two", "shift", "scale"))
})

test_that("the recipe race fills in sub-models when prediction fails", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  dat <- engine_sentinel_data(60)
  rec <- recipes::recipe(y ~ ., data = dat)
  bad_pred <- make_submodel_model(fail_pred = TRUE)

  set.seed(5590)
  expect_snapshot(
    fit <- train(
      rec,
      data = dat,
      method = bad_pred,
      tuneLength = 3,
      trControl = trainControl(
        method = "adaptive_cv",
        number = 5,
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )
  expect_identical(nrow(fit$results), 3L)
})

test_that("the recipe workflows score sub-models for a numeric outcome", {
  skip_on_cran()

  # no probabilities to collect, so the placeholder frames are built instead
  dat <- engine_sentinel_data(60, classification = FALSE)
  rec <- recipes::recipe(y ~ ., data = dat)
  subs <- make_submodel_model()

  for (m in c("cv", "LOOCV")) {
    small <- if (m == "LOOCV") dat[1:16, ] else dat
    set.seed(7714)
    fit <- suppressWarnings(train(
      recipes::recipe(y ~ ., data = small),
      data = small,
      method = subs,
      tuneLength = 3,
      trControl = trainControl(method = m, number = 3, savePredictions = "all")
    ))
    expect_identical(nrow(fit$results), 3L)
    expect_contains(names(fit$pred), c("pred", "obs", "shift", "scale"))
  }
})

test_that("the recipe workflows report a sub-model fit that fails", {
  skip_on_cran()

  dat <- engine_sentinel_data(60)
  rec <- recipes::recipe(y ~ ., data = dat)
  failing <- make_submodel_model(fail_fit = TRUE)

  set.seed(3161)
  expect_snapshot(
    fit <- train(
      rec,
      data = dat,
      method = failing,
      tuneLength = 3,
      trControl = trainControl(
        method = "cv",
        number = 5,
        classProbs = TRUE,
        savePredictions = "all"
      )
    )
  )
  expect_identical(nrow(fit$results), 3L)
})

# ------------------------------------------------------------------------------
# the recipe path's remaining options

test_that("train checks the seeds given for a recipe fit", {
  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)

  # one integer vector per resample plus one for the final fit
  expect_snapshot(
    train(
      rec,
      data = reg,
      method = "knn",
      tuneGrid = data.frame(k = c(3, 5)),
      trControl = trainControl(method = "cv", number = 3, seeds = 1:2)
    ),
    error = TRUE
  )
})

test_that("train falls back when a recipe fit's metric is not computed", {
  skip_on_cran()

  reg <- engine_regression(40)
  rec <- recipes::recipe(y ~ ., data = reg)
  median_error <- function(data, lev = NULL, model = NULL) {
    c(MedianError = median(abs(data$obs - data$pred)))
  }

  expect_snapshot_warning(
    fit <- train(
      rec,
      data = reg,
      method = "lm",
      metric = "RMSE",
      trControl = trainControl(
        method = "cv",
        number = 3,
        summaryFunction = median_error
      )
    )
  )
  expect_identical(fit$metric, "MedianError")
})

test_that("train uses a custom selection function for a recipe fit", {
  skip_on_cran()

  dat <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = dat)

  # the function is handed the candidates in the order the model sorts them
  # (descending k, for knn), and here it always takes the first
  offered <- NULL
  set.seed(1094)
  fit <- train(
    rec,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5, 7)),
    trControl = trainControl(
      method = "cv",
      number = 3,
      selectionFunction = function(x, metric, maximize) {
        offered <<- x
        1
      }
    )
  )
  expect_setequal(offered$k, c(3, 5, 7))
  expect_identical(fit$bestTune$k, offered$k[1])
})

test_that("train fits a recipe without resampling", {
  skip_on_cran()

  dat <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = dat)
  rec <- recipes::step_normalize(rec, recipes::all_predictors())

  set.seed(6006)
  fit <- train(
    rec,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "none")
  )
  # the single candidate is taken as given; with no resampling there is nothing
  # to report, so the results table has the metric columns but no rows
  expect_identical(fit$bestTune, data.frame(k = 5))
  expect_identical(nrow(fit$results), 0L)
  expect_contains(names(fit$results), c("Accuracy", "Kappa", "k"))
  expect_length(predict(fit, dat), nrow(dat))
})

test_that("train trims a recipe fit's model object", {
  skip_on_cran()
  skip_if_not_installed("earth")

  reg <- engine_regression(60)
  rec <- recipes::recipe(y ~ ., data = reg)

  # earth defines a trim method, and verboseIter reports how much was saved
  set.seed(9204)
  expect_snapshot(
    fit <- suppressMessages(train(
      rec,
      data = reg,
      method = "earth",
      tuneGrid = data.frame(degree = 1, nprune = 3),
      trControl = trainControl(
        method = "cv",
        number = 2,
        trim = TRUE,
        verboseIter = TRUE
      )
    )),
    transform = mask_decimals
  )
  expect_s3_class(fit, "train.recipe")
})

test_that("train times a recipe fit's predictions", {
  skip_on_cran()

  reg <- engine_regression(40)
  rec <- recipes::recipe(y ~ ., data = reg)

  set.seed(5528)
  fit <- train(
    rec,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3, timingSamps = 5)
  )
  expect_in("prediction", names(fit$times))
})

test_that("fitted values come from a recipe fit", {
  skip_on_cran()

  reg <- engine_regression(40)
  rec <- recipes::recipe(y ~ ., data = reg)

  set.seed(8109)
  fit <- train(
    rec,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_length(fitted(fit), nrow(reg))

  # the model object carries its own fitted values, so dropping the stored
  # training data does not prevent this
  dropped <- train(
    rec,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3, returnData = FALSE)
  )
  expect_length(fitted(dropped), nrow(reg))
})

# ------------------------------------------------------------------------------
# the recipe race's outer phases, mirroring test-adaptive.R

test_that("the recipe race fills in a failure during the burn-in", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  dat <- engine_sentinel_data(60)
  rec <- recipes::recipe(y ~ ., data = dat)
  # the sentinel is in the first fold's holdout, and min = 3 makes the first two
  # resamples the burn-in
  holdouts <- list(51:60, 1:10, 11:20, 21:30, 31:40, 41:50)
  index <- lapply(holdouts, function(h) setdiff(seq_len(nrow(dat)), h))
  failing <- make_submodel_model(fail_fit = TRUE)

  set.seed(4471)
  expect_snapshot(
    fit <- train(
      rec,
      data = dat,
      method = failing,
      tuneLength = 3,
      trControl = trainControl(
        method = "adaptive_cv",
        index = index,
        indexOut = holdouts,
        classProbs = TRUE,
        savePredictions = "all",
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )
  expect_s3_class(fit, "train.recipe")
  expect_true(anyNA(fit$pred$one))
})

test_that("the recipe race fills in a failure while finishing up", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  # a numeric outcome, so the candidates differ and the race settles early;
  # the failing fold is then scored by the completion pass
  dat <- engine_sentinel_data(60, classification = FALSE)
  rec <- recipes::recipe(y ~ ., data = dat)
  holdouts <- split(seq_len(nrow(dat)), rep(1:6, each = 10))
  index <- lapply(holdouts, function(h) setdiff(seq_len(nrow(dat)), h))
  failing <- make_submodel_model(fail_fit = TRUE)

  set.seed(4471)
  expect_snapshot(
    fit <- train(
      rec,
      data = dat,
      method = failing,
      tuneLength = 3,
      trControl = trainControl(
        method = "adaptive_cv",
        index = index,
        indexOut = holdouts,
        savePredictions = "all",
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )
  # the recipe path reports how many resamples each candidate was scored on;
  # the survivor has more than the ones the race dropped
  expect_in("Num_Resamples", names(fit$results))
  expect_gt(max(fit$results$Num_Resamples), min(fit$results$Num_Resamples))
})

test_that("the recipe workflow reports predictions that fail", {
  skip_on_cran()

  dat <- engine_sentinel_data(60, classification = FALSE)
  rec <- recipes::recipe(y ~ ., data = dat)
  bad_pred <- make_submodel_model(fail_pred = TRUE)

  set.seed(6011)
  expect_snapshot(
    fit <- train(
      rec,
      data = dat,
      method = bad_pred,
      tuneLength = 3,
      trControl = trainControl(
        method = "cv",
        number = 3,
        savePredictions = "all"
      )
    )
  )
  expect_identical(nrow(fit$results), 3L)
})

test_that("train checks a custom method list given with a recipe", {
  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)

  # a method list has to provide the required elements
  incomplete <- make_custom_model()[c("library", "type", "parameters")]
  expect_snapshot(train(rec, data = reg, method = incomplete), error = TRUE)

  # and a named method has to be one caret knows
  expect_snapshot(
    train(rec, data = reg, method = "not_a_caret_model"),
    error = TRUE
  )
})

test_that("train validates the resampling method for a recipe fit", {
  reg <- engine_regression(30)
  rec <- recipes::recipe(y ~ ., data = reg)

  # out-of-bag estimates need a model that reports them
  expect_snapshot(
    train(
      rec,
      data = reg,
      method = "lm",
      trControl = trainControl(method = "oob")
    ),
    error = TRUE
  )

  # and without resampling there can only be one candidate
  expect_snapshot(
    train(
      rec,
      data = reg,
      method = "knn",
      tuneGrid = data.frame(k = c(3, 5)),
      trControl = trainControl(method = "none")
    ),
    error = TRUE
  )
})

test_that("train falls back on a recipe fit's metric with class probabilities", {
  skip_on_cran()

  cls <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = cls)

  # the summary function does not compute Accuracy, so the first thing it does
  # compute is used instead
  expect_snapshot_warning(
    fit <- train(
      rec,
      data = cls,
      method = "lda",
      metric = "Accuracy",
      trControl = trainControl(
        method = "cv",
        number = 3,
        classProbs = TRUE,
        summaryFunction = function(data, lev = NULL, model = NULL) {
          c(HitRate = mean(data$obs == data$pred))
        }
      )
    )
  )
  expect_identical(fit$metric, "HitRate")
})

test_that("leave-one-out resampling reports a recipe prediction that fails", {
  skip_on_cran()

  dat <- engine_sentinel_data(16)
  rec <- recipes::recipe(y ~ ., data = dat)
  # the predictions fail for whichever held-out row leaves the sentinel out of
  # the training set, which with leave-one-out is exactly one row
  bad_pred <- make_submodel_model(fail_pred = TRUE)

  set.seed(6011)
  expect_snapshot_warning(
    fit <- train(
      rec,
      data = dat,
      method = bad_pred,
      tuneLength = 2,
      trControl = trainControl(method = "LOOCV", classProbs = TRUE)
    )
  )
  expect_s3_class(fit, "train.recipe")
})

test_that("the recipe race reports a prediction failure while racing", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  # a factor outcome, so every candidate predicts the same class and the race
  # never narrows: the fold holding out the sentinel (the fourth) is scored by
  # the racing loop rather than the burn-in or the completion pass
  dat <- engine_sentinel_data(60)
  rec <- recipes::recipe(y ~ ., data = dat)
  holdouts <- list(1:10, 11:20, 21:30, 51:60, 31:40, 41:50)
  index <- lapply(holdouts, function(h) setdiff(seq_len(nrow(dat)), h))
  bad_pred <- make_submodel_model(fail_pred = TRUE)

  set.seed(4471)
  expect_snapshot(
    fit <- train(
      rec,
      data = dat,
      method = bad_pred,
      tuneLength = 3,
      trControl = trainControl(
        method = "adaptive_cv",
        index = index,
        indexOut = holdouts,
        classProbs = TRUE,
        savePredictions = "all",
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )
  expect_s3_class(fit, "train.recipe")
})

test_that("the recipe race reports a prediction failure while finishing up", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  # a numeric outcome, so the race settles early and the failing fold is scored
  # by the completion pass
  dat <- engine_sentinel_data(60, classification = FALSE)
  rec <- recipes::recipe(y ~ ., data = dat)
  holdouts <- split(seq_len(nrow(dat)), rep(1:6, each = 10))
  index <- lapply(holdouts, function(h) setdiff(seq_len(nrow(dat)), h))
  bad_pred <- make_submodel_model(fail_pred = TRUE)

  set.seed(4471)
  expect_snapshot(
    fit <- train(
      rec,
      data = dat,
      method = bad_pred,
      tuneLength = 3,
      trControl = trainControl(
        method = "adaptive_cv",
        index = index,
        indexOut = holdouts,
        savePredictions = "all",
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )
  expect_gt(max(fit$results$Num_Resamples), min(fit$results$Num_Resamples))
})

test_that("the recipe race labels sub-model results while finishing up", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  dat <- engine_sentinel_data(60, classification = FALSE)
  rec <- recipes::recipe(y ~ ., data = dat)
  holdouts <- split(seq_len(nrow(dat)), rep(1:6, each = 10))
  index <- lapply(holdouts, function(h) setdiff(seq_len(nrow(dat)), h))
  mod <- make_submodel_model()

  set.seed(4471)
  suppressWarnings(
    fit <- train(
      rec,
      data = dat,
      method = mod,
      tuneLength = 3,
      trControl = trainControl(
        method = "adaptive_cv",
        index = index,
        indexOut = holdouts,
        savePredictions = "all",
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )

  # as in the non-recipe race: each candidate predicts its own `shift`, so a
  # mismatch means the completion pass scored the wrong sub-models
  expect_equal(fit$pred$pred, fit$pred$shift)
  expect_in(names(index), fit$pred$Resample)
})

test_that("the recipe race scores class probabilities while finishing up", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  # as in the non-recipe race: k = 30 is dropped after the burn-in, leaving the
  # completion pass to score the winner on the remaining resamples
  cls <- engine_three_class()
  rec <- recipes::recipe(Species ~ ., data = cls)

  set.seed(9527)
  fit <- train(
    rec,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = c(1, 30)),
    trControl = trainControl(
      method = "adaptive_cv",
      number = 6,
      classProbs = TRUE,
      savePredictions = "all",
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  )

  # every saved prediction says which candidate made it, the resamples the race
  # starts from included
  expect_all_false(is.na(fit$pred$k))
  scored <- tapply(fit$pred$Resample, fit$pred$k, function(x) length(unique(x)))
  expect_equal(as.vector(scored), c(6, 3))
  expect_equal(fit$results$Num_Resamples[order(fit$results$k)], c(6L, 3L))
  expect_contains(names(fit$pred), levels(cls$Species))
  expect_all_false(is.na(fit$pred$setosa))
})

test_that("the recipe race keeps the cell counts for more than five classes", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  # the confusion-matrix cells are kept for up to fifty classes, and every
  # phase of the race has to agree on that: the resamples the race starts from
  # were dropping them past five
  set.seed(7712)
  many <- data.frame(
    x1 = rnorm(72),
    x2 = rnorm(72),
    y = factor(rep(paste0("class", 1:6), each = 12))
  )
  rec <- recipes::recipe(y ~ ., data = many)

  set.seed(9527)
  fit <- train(
    rec,
    data = many,
    method = "knn",
    tuneGrid = data.frame(k = c(1, 30)),
    trControl = trainControl(
      method = "adaptive_cv",
      number = 6,
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  )

  cells <- grep("^cell", names(fit$resampledCM), value = TRUE)
  expect_length(cells, 36)
  # the counts are complete for every resample, the first ones included
  expect_all_false(as.vector(is.na(as.matrix(fit$resampledCM[, cells]))))
  expect_setequal(fit$resampledCM$Resample, names(fit$control$index))
})

test_that("the recipe race reports a model fit failure while racing", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  # as above, but the fit rather than the prediction fails in the fourth
  # resample, which the racing loop scores
  dat <- engine_sentinel_data(60)
  rec <- recipes::recipe(y ~ ., data = dat)
  holdouts <- list(1:10, 11:20, 21:30, 51:60, 31:40, 41:50)
  index <- lapply(holdouts, function(h) setdiff(seq_len(nrow(dat)), h))
  failing <- make_submodel_model(fail_fit = TRUE)

  set.seed(4471)
  expect_snapshot(
    fit <- train(
      rec,
      data = dat,
      method = failing,
      tuneLength = 3,
      trControl = trainControl(
        method = "adaptive_cv",
        index = index,
        indexOut = holdouts,
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )
  expect_s3_class(fit, "train.recipe")
})
