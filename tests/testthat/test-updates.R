# `diff_coef()` and the shared `updates_dat` / `updates_y_ind` live in helper-updates.R

# ------------------------------------------------------------------------------

test_that("train updating", {
  skip_on_cran()
  ctrl <- trainControl(method = "cv")

  lm_obj_form <- train(
    y ~ Var01 + Var02,
    data = updates_dat,
    method = "lm",
    trControl = ctrl
  )
  lm_obj_form_2 <- update(lm_obj_form, list(intercept = FALSE))
  expect_length(lm_obj_form_2$finalModel$coefficients, 2)

  rec <- recipe(y ~ Var01 + Var02, data = updates_dat) %>%
    step_mutate(Var01 = Var01 / 2)
  lm_obj_rec <- train(rec, data = updates_dat, method = "lm", trControl = ctrl)
  lm_obj_rec_2 <- update(lm_obj_rec, list(intercept = FALSE))
  expect_length(lm_obj_rec_2$finalModel$coefficients, 2)
})


# ------------------------------------------------------------------------------

test_that("safs updating", {
  skip_on_cran()
  ctrl <- safsControl(functions = caretSA, method = "cv", number = 3)

  set.seed(3997)
  sa_xy <-
    safs(
      x = updates_dat[, -updates_y_ind],
      y = updates_dat$y,
      safsControl = ctrl,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  new_iter <- ifelse(sa_xy$optIter == 1, 2, 1)
  sa_xy_2 <- update(
    sa_xy,
    iter = new_iter,
    x = updates_dat[, -updates_y_ind],
    y = updates_dat$y
  )
  expect_true(diff_coef(sa_xy, sa_xy_2))
  expect_snapshot(update(sa_xy, iter = new_iter), error = TRUE)

  rec <- recipe(y ~ ., data = updates_dat) %>%
    step_mutate(Var01 = Var01 / 2)
  set.seed(3997)
  sa_rec <-
    safs(
      rec,
      data = updates_dat,
      safsControl = ctrl,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  new_iter <- ifelse(sa_rec$optIter == 1, 2, 1)
  sa_rec_2 <- update(sa_rec, iter = new_iter)
  expect_true(diff_coef(sa_rec, sa_rec_2))
  sa_rec$recipe$template <- NULL
  expect_snapshot(update(sa_rec, iter = new_iter), error = TRUE)
})

# ------------------------------------------------------------------------------

test_that("gafs updating", {
  skip_on_cran()
  ctrl <- gafsControl(functions = caretGA, method = "cv", number = 3)

  set.seed(3997)
  ga_xy <-
    gafs(
      x = updates_dat[, -updates_y_ind],
      y = updates_dat$y,
      gafsControl = ctrl,
      popSize = 4,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  new_iter <- ifelse(ga_xy$optIter == 1, 2, 1)
  ga_xy_2 <- update(
    ga_xy,
    iter = new_iter,
    x = updates_dat[, -updates_y_ind],
    y = updates_dat$y
  )
  expect_true(diff_coef(ga_xy, ga_xy_2))
  expect_snapshot(update(ga_xy, iter = new_iter), error = TRUE)

  rec <- recipe(y ~ ., data = updates_dat) %>%
    step_mutate(Var01 = Var01 / 2)
  set.seed(3997)
  ga_rec <-
    gafs(
      rec,
      data = updates_dat,
      gafsControl = ctrl,
      popSize = 4,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  new_iter <- ifelse(ga_rec$optIter == 1, 2, 1)
  ga_rec_2 <- update(ga_rec, iter = new_iter)
  expect_true(diff_coef(ga_rec, ga_rec_2))
  ga_rec$recipe$template <- NULL
  expect_snapshot(update(ga_rec, iter = new_iter), error = TRUE)
})


# ------------------------------------------------------------------------------

test_that("rfe updating", {
  skip_on_cran()
  ctrl <- rfeControl(functions = caretFuncs, method = "cv", number = 3)

  set.seed(3997)
  rfe_xy <-
    rfe(
      x = updates_dat[, -updates_y_ind],
      y = updates_dat$y,
      rfeControl = ctrl,
      sizes = 1:5,
      method = "lm",
      trControl = trainControl(method = "none")
    )
  expect_snapshot_warning(
    rfe_xy_2 <- update(
      rfe_xy,
      size = 5,
      x = updates_dat[, -updates_y_ind],
      y = updates_dat$y
    )
  )
  expect_length(rfe_xy_2$fit$finalModel$coefficients, 6)
  expect_snapshot(update(rfe_xy, size = 5), error = TRUE)

  rec <- recipe(y ~ ., data = updates_dat) %>%
    step_mutate(Var01 = Var01 / 2)
  set.seed(3997)
  rfe_rec <-
    rfe(
      rec,
      data = updates_dat,
      rfeControl = ctrl,
      sizes = 1:5,
      method = "lm",
      trControl = trainControl(method = "none")
    )
  expect_snapshot_warning(rfe_rec_2 <- update(rfe_rec, size = 5))
  expect_length(rfe_rec_2$fit$finalModel$coefficients, 6)
  rfe_rec$recipe$template <- NULL
  expect_snapshot(update(rfe_rec, size = 5), error = TRUE)
})

# ------------------------------------------------------------------------------
# update.train argument checking

test_that("update.train needs the training data to re-fit", {
  skip_on_cran()

  # without the data there is nothing to re-fit the model on
  fit <- train(
    y ~ Var01 + Var02,
    data = updates_dat,
    method = "lm",
    trControl = trainControl(method = "cv", returnData = FALSE)
  )
  expect_snapshot(update(fit, list(intercept = FALSE)), error = TRUE)
})

test_that("update.train checks the parameters it is given", {
  skip_on_cran()

  fit <- train(
    y ~ Var01 + Var02,
    data = updates_dat,
    method = "lm",
    trControl = trainControl(method = "cv")
  )

  # not a data frame or a named list
  expect_snapshot(update(fit, 3), error = TRUE)
  # one model at a time
  expect_snapshot(
    update(fit, data.frame(intercept = c(TRUE, FALSE))),
    error = TRUE
  )
  # every tuning parameter has to be given, and named as the model names it
  expect_snapshot(update(fit, list(intercept = TRUE, extra = 1)), error = TRUE)
  expect_snapshot(update(fit, list(wrong_name = TRUE)), error = TRUE)
})

test_that("update.train accepts the old dot-prefixed parameter names", {
  skip_on_cran()

  fit <- train(
    Species ~ .,
    data = engine_three_class(),
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "cv", number = 3)
  )

  updated <- update(fit, list(.k = 3))
  # the dot is dropped, so the re-fit uses the parameter the model knows
  expect_identical(updated$bestTune, data.frame(k = 3))
  expect_identical(updated$update, data.frame(k = 3))
})

test_that("update.train re-applies pre-processing, weights and extra arguments", {
  skip_on_cran()

  wts <- rep(c(1, 2), length.out = nrow(updates_dat))
  fit <- train(
    y ~ Var01 + Var02,
    data = updates_dat,
    method = "lm",
    weights = wts,
    preProcess = c("center", "scale"),
    singular.ok = TRUE,
    trControl = trainControl(method = "cv")
  )

  # re-fitting does not complain about the pre-processing methods it was given
  expect_no_warning(updated <- update(fit, list(intercept = FALSE)))
  expect_length(updated$finalModel$coefficients, 2)
  # the pre-processing is re-estimated rather than dropped
  expect_s3_class(updated$preProcess, "preProcess")
  # and the case weights are used again, whichever interface was used
  expect_identical(updated$finalModel$weights, wts)

  xy_fit <- train(
    updates_dat[, c("Var01", "Var02")],
    updates_dat$y,
    method = "lm",
    weights = wts,
    trControl = trainControl(method = "cv")
  )
  expect_identical(
    update(xy_fit, list(intercept = FALSE))$finalModel$weights,
    wts
  )
})

# ------------------------------------------------------------------------------
# update.train on objects from old caret versions

test_that("update.train fills in the model information of an old object", {
  skip_on_cran()

  fit <- train(
    y ~ Var01 + Var02,
    data = updates_dat,
    method = "lm",
    trControl = trainControl(method = "cv")
  )
  # objects from caret 5.17-7 and earlier had no modelInfo element
  old <- fit
  old$modelInfo <- NULL

  expect_snapshot_warning(updated <- update(old))
  expect_true(updated$modelInfo$updated)
  # the method is matched exactly, so "lm" does not pick up bayesglm or glmnet
  expect_identical(updated$modelInfo$label, "Linear Regression")
})

test_that("update.train cannot rescue an old object of an unknown type", {
  skip_on_cran()

  fit <- train(
    y ~ Var01 + Var02,
    data = updates_dat,
    method = "lm",
    trControl = trainControl(method = "cv")
  )
  old <- fit
  old$modelInfo <- NULL
  old$method <- "a_method_that_never_existed"

  expect_snapshot(update(old), error = TRUE)
})

test_that("update.train keeps the class levels of a recipe model", {
  skip_on_cran()

  # the outcome comes back out of the recipe, so its levels have to be read from
  # there rather than from the (dropped) training data
  rec <- recipes::recipe(Species ~ ., data = engine_three_class())
  set.seed(9556)
  fit <- train(
    rec,
    data = engine_three_class(),
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "cv", number = 3)
  )

  updated <- update(fit, list(k = 3))
  expect_identical(updated$finalModel$obsLevels, levels(iris$Species))
  expect_identical(updated$bestTune, data.frame(k = 3))
})
