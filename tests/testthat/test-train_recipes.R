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
  skip_if_not_installed("recipes")

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
  skip_if_not_installed("recipes")

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
  skip_if_not_installed("recipes")

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
  skip_if_not_installed("recipes")
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
