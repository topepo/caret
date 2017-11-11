timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "gbm"



#########################################################################

gbmGrid <- expand.grid(interaction.depth = 1:2,
                       shrinkage = .1,
                       n.trees = c(10, 50, 100),
                       n.minobsinnode = 10)

set.seed(2)
training <- twoClassSim(300, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary, 
                       seeds = seeds)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary, 
                       seeds = seeds)
cctrl3 <- trainControl(method = "oob", seeds = seeds)
cctrl4 <- trainControl(method = "none",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary, 
                       seeds = seeds)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random", 
                       seeds = seeds)
cctrlB632 <- trainControl(method = "boot632", number = 10, search = "random", timingSamps = 11, classProbs = TRUE)
cctrlBopt <- trainControl(method = "optimism_boot", number = 10, search = "random", savePredictions = "final", classProbs = TRUE)
cctrlAdapt <- trainControl(method = "adaptive_boot", number = 15, search = "random", classProbs = TRUE)


set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "gbm", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             tuneGrid = gbmGrid,
                             verbose = FALSE)

set.seed(849)
test_class_cv_dist <- train(trainX, trainY, 
                            method = "gbm", 
                            trControl = cctrl1,
                            metric = "ROC", 
                            preProc = c("center", "scale"),
                            tuneGrid = gbmGrid,
                            verbose = FALSE,
                            distribution = "adaboost")


set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "gbm", 
                            trControl = cctrl1,
                            metric = "ROC", 
                            preProc = c("center", "scale"),
                            tuneGrid = gbmGrid,
                            verbose = FALSE)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "gbm", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         verbose = FALSE)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "gbm", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              tuneGrid = gbmGrid,
                              verbose = FALSE)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "gbm", 
                               trControl = cctrl4,
                               tuneGrid = gbmGrid[nrow(gbmGrid),],
                               verbose = FALSE,
                               metric = "ROC", 
                               preProc = c("center", "scale"))
test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_b632_model <- train(trainX, trainY, 
                               method = "gbm", 
                               trControl = cctrlB632,
                               preProc = c("center", "scale"),
                               tuneGrid = gbmGrid,
                               verbose = FALSE)


set.seed(849)
test_class_bopt_model <- train(trainX, trainY, 
                               method = "gbm", 
                               trControl = cctrlBopt,
                               preProc = c("center", "scale"),
                               tuneGrid = gbmGrid,
                               verbose = FALSE)


set.seed(849)
test_class_adapt_model <- train(trainX, trainY, 
                                method = "gbm", 
                                trControl = cctrlAdapt,
                                preProc = c("center", "scale"),
                                tuneGrid = gbmGrid,
                                verbose = FALSE)

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "gbm", 
                        trControl = cctrl1,
                        metric = "ROC",
                        tuneGrid = gbmGrid,
                        verbose = FALSE)

test_class_pred_rec <- predict(test_class_rec, testing[, -ncol(testing)])
test_class_prob_rec <- predict(test_class_rec, testing[, -ncol(testing)], 
                               type = "prob")

set.seed(849)
test_class_b632_rec_model <- train(x = rec_cls,
                                   data = training, 
                                   method = "gbm", 
                                   trControl = cctrlB632,
                                   tuneGrid = gbmGrid,
                                   verbose = FALSE)
if(!isTRUE(all.equal(test_class_b632_rec_model$results, 
                     test_class_b632_model$results)))
  stop("x/y and recipe interface have different results for B632")

set.seed(849)
test_class_bopt_rec_model <- train(x = rec_cls,
                                   data = training, 
                                   method = "gbm", 
                                   trControl = cctrlBopt,
                                   tuneGrid = gbmGrid,
                                   verbose = FALSE)
if(!isTRUE(all.equal(test_class_bopt_rec_model$results, 
                     test_class_bopt_model$results)))
  stop("x/y and recipe interface have different results for B optim")

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

library(caret)
library(plyr)
library(recipes)
library(dplyr)
set.seed(1)
training <- SLC14_1(75)
testing <- SLC14_1(100)
trainX <- training[, -ncol(training)]
trainY <- training$y

rec_reg <- recipe(y ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "oob")
rctrl4 <- trainControl(method = "none")
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "gbm", 
                           trControl = rctrl1,
                           preProc = c("center", "scale"),
                           tuneGrid = gbmGrid,
                           verbose = FALSE)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_dist <- train(trainX, trainY, 
                          method = "gbm", 
                          trControl = rctrl1,
                          preProc = c("center", "scale"),
                          tuneGrid = gbmGrid,
                          verbose = FALSE,
                          distribution = "laplace")

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "gbm", 
                          trControl = rctrl1,
                          preProc = c("center", "scale"),
                          tuneGrid = gbmGrid,
                          verbose = FALSE)
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "gbm", 
                       trControl = rctrlR,
                       tuneLength = 4,
                       verbose = FALSE)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "gbm",
                            trControl = rctrl2,
                            preProc = c("center", "scale"),
                            tuneGrid = gbmGrid,
                            verbose = FALSE)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "gbm", 
                             trControl = rctrl4,
                             tuneGrid = gbmGrid[nrow(gbmGrid),],
                             verbose = FALSE,
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, testX)

set.seed(849)
test_reg_rec <- train(x = rec_reg,
                      data = training,
                      method = "gbm", 
                      trControl = rctrl1,
                      tuneGrid = gbmGrid,
                      verbose = FALSE)

if(
  !isTRUE(
    all.equal(test_reg_cv_model$results, 
              test_reg_rec$results))
)
  stop("CV weights not giving the same results")

test_reg_imp_rec <- varImp(test_reg_rec)


test_reg_pred_rec <- predict(test_reg_rec, testing[, -ncol(testing)])

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

test_class_imp <- varImp(test_class_cv_model)
test_reg_imp <- varImp(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

#q("no")


