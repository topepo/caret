# Tests for the xgbTree, xgbDART, and xgbLinear model code in
# inst/model_sources/files, which requires xgboost >= 3.

test_that("xgboost models fit and predict all outcome types", {
  skip_on_cran()
  skip_if_not_installed("xgboost", "3.0.0")

  reg <- engine_regression()
  two <- engine_two_class()
  three <- engine_three_class()

  for (method in c("xgbTree", "xgbDART", "xgbLinear")) {
    info <- getModelInfo(method, regex = FALSE)[[1]]

    withr::local_seed(6093)
    expect_no_warning(fit <- xgb_fit(method, reg[, 1:3], reg$y))
    expect_s3_class(fit, "xgb.Booster")
    pred <- info$predict(fit, reg[, 1:3])
    expect_type(pred, "double")
    expect_length(pred, nrow(reg))

    for (dat in list(two, three)) {
      # the outcome is the last column
      y <- dat[[ncol(dat)]]
      x <- dat[, -ncol(dat)]
      expect_no_warning(fit <- xgb_fit(method, x, y))
      expect_equal(info$levels(fit), levels(y))

      pred <- info$predict(fit, x)
      expect_all_true(pred %in% levels(y))
      expect_length(pred, nrow(x))

      probs <- info$prob(fit, x)
      expect_named(probs, levels(y))
      expect_equal(nrow(probs), nrow(x))
      expect_equal(rowSums(probs), rep(1, nrow(x)), tolerance = 1e-6)
      # the predicted class has the largest probability
      expect_equal(pred, levels(y)[apply(probs, 1, which.max)])
    }
  }
})

test_that("xgboost submodel predictions match models with fewer rounds", {
  skip_on_cran()
  skip_if_not_installed("xgboost", "3.0.0")

  dat <- engine_three_class()
  x <- dat[, -5]
  y <- dat$Species
  info <- getModelInfo("xgbTree", regex = FALSE)[[1]]
  grid <- data.frame(
    nrounds = 10,
    max_depth = 2,
    eta = 0.3,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1,
    lambda = 1,
    alpha = 0
  )
  fit_rounds <- function(n) {
    grid$nrounds <- n
    xgb_fit("xgbTree", x, y, grid = grid)
  }
  full <- fit_rounds(10)
  small <- fit_rounds(4)

  sub_probs <- info$prob(full, x, submodels = data.frame(nrounds = 4))
  expect_length(sub_probs, 2)
  expect_equal(sub_probs[[1]], info$prob(full, x))
  expect_equal(sub_probs[[2]], info$prob(small, x))

  sub_class <- info$predict(full, x, submodels = data.frame(nrounds = 4))
  expect_equal(sub_class[[2]], info$predict(small, x))
})

test_that("xgboost options passed to train() go to the booster parameters", {
  skip_on_cran()
  skip_if_not_installed("xgboost", "3.0.0")

  dat <- engine_two_class()
  ctrl <- trainControl(method = "none", classProbs = TRUE)
  grid <- data.frame(
    nrounds = 5,
    max_depth = 2,
    eta = 0.3,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )

  withr::local_seed(2741)
  expect_no_warning(
    fit <- train(
      Class ~ .,
      data = dat,
      method = "xgbTree",
      tuneGrid = grid,
      trControl = ctrl,
      nthread = 1,
      max_bin = 32,
      objective = "binary:logitraw",
      verbose = 0
    )
  )
  params <- attr(fit$finalModel, "params")
  expect_equal(params$nthread, 1)
  expect_equal(params$max_bin, 32)
  expect_equal(params$objective, "binary:logitraw")

  # the raw scores are converted to probabilities
  probs <- predict(fit, dat, type = "prob")
  expect_all_true(probs$Class1 >= 0 & probs$Class1 <= 1)
  raw <- predict(fit$finalModel, as.matrix(dat[, names(dat) != "Class"]))
  expect_equal(probs$Class1, binomial()$linkinv(raw))
})

test_that("train() with xgboost keeps caret's information on the booster", {
  skip_on_cran()
  skip_if_not_installed("xgboost", "3.0.0")

  dat <- engine_three_class()
  withr::local_seed(5318)
  fit <- train(
    Species ~ .,
    data = dat,
    method = "xgbTree",
    tuneLength = 1,
    trControl = trainControl(method = "cv", number = 3),
    nthread = 1
  )
  expect_equal(attr(fit$finalModel, "xNames"), names(dat)[1:4])
  expect_equal(
    attr(fit$finalModel, "obsLevels"),
    levels(dat$Species),
    ignore_attr = "ordered"
  )
  expect_equal(attr(fit$finalModel, "problemType"), "Classification")
  expect_equal(levels(fit), levels(dat$Species))

  # extra columns in `newdata` are dropped before predicting
  extra <- dat
  extra$junk <- rnorm(nrow(extra))
  extra <- extra[, c("junk", names(dat))]
  expect_equal(predict(fit, extra), predict(fit, dat))

  imp <- varImp(fit)$importance
  expect_setequal(rownames(imp), names(dat)[1:4])
  expect_all_true(predictors(fit) %in% names(dat)[1:4])
})
