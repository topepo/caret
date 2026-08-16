# Tests for the summary functions in R/postResample.R: twoClassSummary,
# mnLogLoss and multiClassSummary. The mnLogLoss fixtures live in
# helper-postResample.R.

# --- twoClassSummary ----------------------------------------------------------

test_that("twoClassSummary is calculating correctly", {
  skip_on_cran()
  set.seed(8225)
  tr_dat <- twoClassSim(100)
  te_dat <- twoClassSim(100)
  mod <- knn3(x = as.matrix(tr_dat[, 1:5]), y = tr_dat$Class)
  te_pred <- predict(mod, te_dat[, 1:5], type = "class")
  te_prob <- predict(mod, te_dat[, 1:5], type = "prob")
  te_prob <- as.data.frame(te_prob, stringsAsFactors = TRUE)

  cm <- caret::confusionMatrix(te_pred, te_dat$Class)
  roc_crv <- pROC::roc(
    te_dat$Class,
    te_prob$Class1,
    direction = ">",
    quiet = TRUE
  )
  roc_auc <- as.numeric(pROC::auc(roc_crv))

  te_res <- te_prob
  te_res$pred <- te_pred
  te_res$obs <- te_dat$Class

  tcs_res <- twoClassSummary(te_res, lev = levels(te_pred))
  expect_equal(roc_auc, unname(tcs_res["ROC"]))
  expect_equal(unname(cm$byClass["Sensitivity"]), unname(tcs_res["Sens"]))
  expect_equal(unname(cm$byClass["Specificity"]), unname(tcs_res["Spec"]))
})

# --- mnLogLoss ----------------------------------------------------------------

test_that("Multiclass logloss returns expected values", {
  skip_on_cran()
  result1 <- mnLogLoss(mnll_dat, mnll_classes)

  test_dat2 <- mnll_dat
  test_dat2$A[1] <- NA
  result2 <- mnLogLoss(test_dat2, mnll_classes)

  test_dat3 <- mnll_dat[, rev(1:5)]
  result3 <- mnLogLoss(test_dat3, mnll_classes)

  expect_equal(result1, c(logLoss = 0.424458), tolerance = 0.000001)
  expect_equal(result2, c(logLoss = 0.5093496), tolerance = 0.000001)
  expect_equal(result3, c(logLoss = 0.424458), tolerance = 0.000001)
})

# Issue #637
test_that("Twoclass logloss returns expected values", {
  skip_on_cran()
  result1 <- mnLogLoss(mnll_dat_b, mnll_classes_b)

  test_dat2 <- mnll_dat_b
  test_dat2$A[1] <- NA
  result2 <- mnLogLoss(test_dat2, mnll_classes_b)

  test_dat3 <- mnll_dat_b[, rev(1:4)]
  result3 <- mnLogLoss(test_dat3, mnll_classes_b)

  expect_equal(result1, c(logLoss = 0.244998), tolerance = 0.00001)
  expect_equal(result2, c(logLoss = 0.306248), tolerance = 0.000001)
  expect_equal(result3, c(logLoss = 0.244998), tolerance = 0.00001)
})

# --- multiClassSummary --------------------------------------------------------

test_that("multiClassSummary presenting warnings from train", {
  skip_on_cran()
  skip_if_not_installed("MLmetrics")
  skip_if_not_installed("ModelMetrics", "1.2.2.2")
  N = 1000

  M = 2

  set.seed(1)
  xTrain = matrix(runif(N * M), nrow = N)

  colnames(xTrain) <-
    sapply(1:M, function(u) {
      paste0(collapse = '', letters[sample(26, 3, replace = TRUE)])
    })
  yTrain = as.factor(letters[sample(3, N, replace = TRUE)])

  trCntlListMulti <-
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
  skip_on_cran()
  skip_if_not_installed("MLmetrics")

  lvls <- levels(iris$Species)
  set.seed(46337)
  in_train <- createDataPartition(iris$Species, list = FALSE)
  ir_tr <- iris[in_train, ]
  ir_te <- iris[-in_train, ]
  mod <- MASS::lda(Species ~ ., data = ir_tr)
  pred <- predict(mod, ir_te[, -5])$posterior
  pred <- as.data.frame(pred, stringsAsFactors = TRUE)
  dat <- pred
  dat$pred <- predict(mod, ir_te[, -5])$class
  dat$obs <- ir_te$Species

  obs_roc <- multiClassSummary(dat, lev = lvls)

  exp_roc <- rep(NA_real_, 3)
  names(exp_roc) <- lvls
  for (i in lvls) {
    tmp <- dat
    tmp$obs <- ifelse(dat$obs == i, "pos", "neg")
    tmp$obs <- factor(tmp$obs, levels = c("pos", "neg"))
    exp_roc[i] <- pROC::roc(tmp$obs, tmp[, i], direct = ">", quiet = TRUE)$auc
  }
  expect_equal(mean(exp_roc), unname(obs_roc["AUC"]))
})
