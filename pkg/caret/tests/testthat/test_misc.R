
context('misc functions')


test_that("R2 and RMSE are calculating correctly", {

  pred <- runif(25)
  obs <- runif(25)

  expect_equal(R2(pred, obs), cor(obs, pred)^2)
  expect_equal(RMSE(pred, obs), sqrt(mean((pred - obs)^2)))

})


test_that("auc calculation is > .5 when Xs provide prediction", {

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
