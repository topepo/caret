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
