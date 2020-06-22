context('multiClassSummary')

test_that("multiClassSummary presenting warnings from train", {
  skip_if_not_installed("ModelMetrics", "1.2.2.2")
  library(caret)
  N = 1000

  M = 2

  set.seed(1)
  xTrain = matrix(runif(N * M), nrow = N)

  colnames(xTrain) <-
    sapply(1:M, function(u)
      paste0(collapse = '', letters[sample(26, 3, replace = TRUE)]))
  yTrain = as.factor(letters[sample(3, N, replace = TRUE)])

  trCntlListMulti  <-
    trainControl(
      method = "cv",
      number = 3,
      verboseIter = FALSE,
      classProbs = TRUE,
      summaryFunction = multiClassSummary
    )

  expect_silent({
    enFitMulti <-
      train(
        x = xTrain,
        y = yTrain,
        trControl = trCntlListMulti,
        method = "knn",
        tuneLength = 2
      )
  })

})

test_that("multiClassSummary ROC values", {

  lvls <- levels(iris$Species)
  set.seed(46337)
  in_train <- createDataPartition(iris$Species, list = FALSE)
  ir_tr <- iris[ in_train,]
  ir_te <- iris[-in_train,]
  mod <- MASS::lda(Species ~ ., data = ir_tr)
  pred <- predict(mod, ir_te[, -5])$posterior
  pred <- as.data.frame(pred, stringsAsFactors = TRUE)
  dat <- pred
  dat$pred <-  predict(mod, ir_te[, -5])$class
  dat$obs <- ir_te$Species

  obs_roc <- multiClassSummary(dat, lev = lvls)

  exp_roc <- rep(NA_real_, 3)
  names(exp_roc) <- lvls
  for(i in lvls) {
    tmp <- dat
    tmp$obs <- ifelse(dat$obs == i, "pos", "neg")
    tmp$obs <- factor(tmp$obs, levels = c("pos", "neg"))
    exp_roc[i] <- pROC::roc(tmp$obs, tmp[, i], direct = ">", quiet = TRUE)$auc
  }
  expect_equal(mean(exp_roc), unname(obs_roc["AUC"]))

})



