
context('twoClassSummary testing')


test_that("twoClassSummary is calculating correctly", {
  set.seed(1)
  tr_dat <- twoClassSim(100)
  te_dat <- twoClassSim(100)
  mod <- knn3(x = as.matrix(tr_dat[, 1:5]), y = tr_dat$Class)
  te_pred <- predict(mod, te_dat[, 1:5], type = "class")
  te_prob <- predict(mod, te_dat[, 1:5], type = "prob")
  te_prob <- as.data.frame(te_prob, stringsAsFactors = TRUE)

  cm <- caret::confusionMatrix(te_pred, te_dat$Class)
  library(pROC)
  roc_crv <- pROC::roc(te_dat$Class, te_prob$Class1, direction = ">", quiet = TRUE)
  roc_auc <- as.numeric(pROC::auc(roc_crv))

  te_res <- te_prob
  te_res$pred <- te_pred
  te_res$obs <- te_dat$Class

  tcs_res <- twoClassSummary(te_res, lev = levels(te_pred))
  expect_equal(roc_auc, unname(tcs_res["ROC"]))
  expect_equal(unname(cm$byClass["Sensitivity"]), unname(tcs_res["Sens"]))
  expect_equal(unname(cm$byClass["Specificity"]), unname(tcs_res["Spec"]))
})
