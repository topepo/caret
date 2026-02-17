
context('misc functions')


test_that("R2 and RMSE are calculating correctly", {

  pred <- runif(25)
  obs <- runif(25)

  expect_equal(R2(pred, obs), cor(obs, pred)^2)
  expect_equal(RMSE(pred, obs), sqrt(mean((pred - obs)^2)))

})

test_that("weighted regression summaries are honored", {

  pred <- c(0, 1, 2, 3)
  obs <- c(0, 1, 3, 2)
  wts <- c(1, 1, 20, 20)

  w_mean_obs <- weighted.mean(obs, wts)
  w_mean_pred <- weighted.mean(pred, wts)
  w_cov <- sum(wts * (obs - w_mean_obs) * (pred - w_mean_pred))/sum(wts)
  w_var_obs <- sum(wts * (obs - w_mean_obs)^2)/sum(wts)
  w_var_pred <- sum(wts * (pred - w_mean_pred)^2)/sum(wts)
  w_rsq <- (w_cov/sqrt(w_var_obs * w_var_pred))^2

  w_rmse <- sqrt(weighted.mean((pred - obs)^2, wts))
  w_mae <- weighted.mean(abs(pred - obs), wts)

  out <- postResample(pred, obs, weights = wts)
  expect_equal(unname(out["RMSE"]), w_rmse)
  expect_equal(unname(out["Rsquared"]), w_rsq)
  expect_equal(unname(out["MAE"]), w_mae)

  data_out <- defaultSummary(data.frame(obs = obs, pred = pred, weights = wts))
  expect_equal(unname(data_out["Rsquared"]), w_rsq)
})

test_that("postResample argument matching remains backward compatible", {

  pred <- c(0, 1, 2, 3)
  obs <- c(0, 1, 3, 2)
  wts <- c(1, 1, 20, 20)

  base <- postResample(pred, obs)
  expect_equal(postResample(pred = pred, obs = obs), base)
  expect_equal(postResample(obs = obs, pred = pred), base)

  weighted_named <- postResample(pred, obs, weights = wts)
  weighted_positional <- postResample(pred, obs, wts)
  weighted_partial <- postResample(pred, obs, w = wts)

  expect_equal(weighted_positional, weighted_named)
  expect_equal(weighted_partial, weighted_named)
})


test_that("auc calculation is > .5 when Xs provide prediction", {
  skip_if_not_installed("MLmetrics")
  skip_if_not_installed("earth")
  skip_if_not_installed("mda")

  trCntlListMulti  <-
    trainControl(
      method = "cv",
      number = 3,
      verboseIter = FALSE,
      classProbs = TRUE,
      summaryFunction = multiClassSummary
    )

  set.seed(3453)
  knnFit <- train(Species ~ .,
                  data = iris,
                  method = "knn",
                  trControl = trCntlListMulti)

  expect_true(all(knnFit$resample$AUC > .5))

  library(caret)

  set.seed(1)
  tr_dat <- twoClassSim(200)
  te_dat <- tr_dat
  tr_dat$Class = factor(tr_dat$Class, levels = rev(levels(te_dat$Class)))

  modle <- train(
    Class ~ .,
    data = te_dat,
    method = "fda",
    tuneLength = 10,
    metric = "ROC",
    trControl = trainControl(classProbs = TRUE,
                             summaryFunction = twoClassSummary)
  )

  expect_true(all(modle$resample$AUC > .5))

})
