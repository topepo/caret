timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)
library(xgboost)

modelX <- "xgbDART"

## In case the package or one of its dependencies uses random numbers
## on startup so we'll pre-load the required libraries: 

for(i in getModelInfo(modelX)[[1]]$library)
  do.call("requireNamespace", list(package = i))


#########################################################################

xgbGrid <- expand.grid(nrounds = c(1, 10), 
                       max_depth = 2,
                       eta = 0.30,
                       rate_drop = 0.10,
                       skip_drop = 0.10,
                       colsample_bytree = 0.90,
                       min_child_weight = 2,
                       subsample = 0.75,
                       gamma = 0.10)
set.seed(2)
training <- twoClassSim(100, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

train_sparse <- xgb.DMatrix(as.matrix(trainX))

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl3 <- trainControl(method = "oob")
cctrl4 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(649)
test_class_cv_model <- train(trainX, trainY, 
                             method = modelX, 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             tuneGrid = xgbGrid)

set.seed(649)
test_class_cv_model_sp <- train(train_sparse, trainY, 
                                method = modelX, 
                                trControl = cctrl1,
                                metric = "ROC", 
                                tuneGrid = xgbGrid) # expect a warning here as xgb.DMatrix is used.


set.seed(649)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = modelX, 
                            trControl = cctrl1,
                            metric = "ROC", 
                            preProc = c("center", "scale"),
                            tuneGrid = xgbGrid)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(649)
test_class_rand <- train(trainX, trainY, 
                         method = modelX, 
                         trControl = cctrlR,
                         tuneLength = 4)

set.seed(649)
test_class_loo_model <- train(trainX, trainY, 
                              method = modelX, 
                              trControl = cctrl2,
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              tuneGrid = xgbGrid)

set.seed(649)
test_class_none_model <- train(trainX, trainY, 
                               method = modelX, 
                               trControl = cctrl4,
                               tuneGrid = xgbGrid[nrow(xgbGrid),],
                               metric = "ROC", 
                               preProc = c("center", "scale"))
test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(649)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = modelX, 
                        trControl = cctrl1,
                        metric = "ROC",
                        tuneGrid = xgbGrid)


if(!isTRUE( all.equal(test_class_cv_model$results, test_class_rec$results))){
  stop("CV weights not giving the same results")
}

test_class_imp_rec <- varImp(test_class_rec)


test_class_pred_rec <- predict(test_class_rec, testing[, -ncol(testing)])
test_class_prob_rec <- predict(test_class_rec, testing[, -ncol(testing)], type = "prob")


rep_eval_of_predict_cls = sapply( 1:100, function(x) 
  predict(test_class_rec, newdata = testing[1, -ncol(testing)], type="prob")[,1])
if(length(unique(rep_eval_of_predict_cls))>1){
  stop("Repeated evaluation do not return the same result (classification).")
}


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

train_sparse <- xgb.DMatrix(as.matrix(trainX))

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "oob")
rctrl4 <- trainControl(method = "none")
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(649)
test_reg_cv_model <- train(trainX, trainY, 
                           method = modelX, 
                           trControl = rctrl1,
                           preProc = c("center", "scale"),
                           tuneGrid = xgbGrid)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(649)
test_reg_cv_model_sp <- train(train_sparse, trainY, 
                              method = modelX, 
                              trControl = rctrl1,
                              tuneGrid = xgbGrid)  # expect a warning here as xgb.DMatrix is used.
test_reg_pred_sp <- predict(test_reg_cv_model_sp, testX)


set.seed(649)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = modelX, 
                          trControl = rctrl1,
                          preProc = c("center", "scale"),
                          tuneGrid = xgbGrid)
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(649)
test_reg_rand <- train(trainX, trainY, 
                       method = modelX, 
                       trControl = rctrlR,
                       tuneLength = 4)

set.seed(649)
test_reg_loo_model <- train(trainX, trainY, 
                            method = modelX,
                            trControl = rctrl2,
                            preProc = c("center", "scale"),
                            tuneGrid = xgbGrid)

set.seed(649)
test_reg_none_model <- train(trainX, trainY, 
                             method = modelX, 
                             trControl = rctrl4,
                             tuneGrid = xgbGrid[nrow(xgbGrid),],
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, testX)

set.seed(649)
test_reg_rec <- train(x = rec_reg,
                      data = training,
                      method = modelX, 
                      trControl = rctrl1,
                      tuneGrid = xgbGrid)

if( !isTRUE( all.equal(test_reg_cv_model$results, test_reg_rec$results))){
  stop("CV weights not giving the same optimal parameters")
}

test_reg_imp_rec <- varImp(test_reg_rec)


test_reg_pred_rec <- predict(test_reg_rec, testing[, -ncol(testing)])

#########################################################################

rep_eval_of_predict_reg = sapply(1:100, function(x) 
  predict(test_reg_rec, newdata = testing[1, -ncol(testing)]))
if(length(unique(rep_eval_of_predict_reg))>1){
  stop("Repeated evaluation do not return the same result (regression).")
}

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
     file = file.path(getwd(), paste(modelX, ".RData", sep = "")))

if(!interactive()) q("no")


