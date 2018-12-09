timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "glm"



#########################################################################

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

weight_test <- function (data, lev = NULL, model = NULL)  {
  mean(data$weights)
  postResample(data[, "pred"], data[, "obs"])
}

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)

cctrl4 <- trainControl(method = "cv", number = 3, 
                       summaryFunction = weight_test)
cctrl5 <- trainControl(method = "LOOCV", summaryFunction = weight_test)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "glm", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "glm", 
                            trControl = cctrl1,
                            metric = "ROC", 
                            preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "glm", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "glm", 
                               trControl = cctrl3,
                               tuneLength = 1,
                               metric = "ROC", 
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

case_weights <- runif(nrow(trainX))

set.seed(849)
test_class_cv_weight <- train(trainX, trainY, 
                              weights = case_weights,
                              method = "glm", 
                              trControl = cctrl4,
                              tuneLength = 1,
                              metric = "Accuracy", 
                              preProc = c("center", "scale"))

set.seed(849)
test_class_loo_weight <- train(trainX, trainY, 
                               weights = case_weights,
                               method = "glm", 
                               trControl = cctrl5,
                               tuneLength = 1,
                               metric = "Accuracy", 
                               preProc = c("center", "scale"))

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "glm", 
                        trControl = cctrl1,
                        metric = "ROC")


if(
  !isTRUE(
    all.equal(test_class_cv_model$results, 
              test_class_rec$results))
)
  stop("CV weights not giving the same results")

test_class_imp_rec <- varImp(test_class_rec)


test_class_pred_rec <- predict(test_class_rec, testing[, -ncol(testing)])
test_class_prob_rec <- predict(test_class_rec, testing[, -ncol(testing)], 
                               type = "prob")

tmp <- training
tmp$wts <- case_weights

class_rec <- recipe(Class ~ ., data = tmp) %>%
  update_role(wts, new_role = "case weight") %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

set.seed(849)
test_class_cv_weight_rec <- train(class_rec, 
                                  data = tmp,
                                  method = "glm", 
                                  trControl = cctrl4,
                                  tuneLength = 1,
                                  metric = "Accuracy")
if(
  !isTRUE(
    all.equal(test_class_cv_weight_rec$results, 
              test_class_cv_weight$results))
  )
  stop("CV weights not giving the same results")

set.seed(849)
test_class_loo_weight_rec <- train(class_rec, 
                                  data = tmp,
                                  method = "glm", 
                                  trControl = cctrl5,
                                  tuneLength = 1,
                                  metric = "Accuracy")
if(
  !isTRUE(
    all.equal(test_class_loo_weight_rec$results, 
              test_class_loo_weight$results))
)
  stop("CV weights not giving the same results")


test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

library(caret)
library(plyr)
library(recipes)
library(dplyr)
set.seed(1)
training <- SLC14_1(30)
testing <- SLC14_1(100)
trainX <- training[, -ncol(training)]
trainY <- training$y

case_weights <- runif(nrow(trainX))

rec_reg <- recipe(y ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")
rctrl1 <- trainControl(method = "cv", number = 3, summaryFunction = weight_test)
rctrl2 <- trainControl(method = "LOOCV", summaryFunction = weight_test)

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "glm", 
                           trControl = rctrl1,
                           preProc = c("center", "scale"))
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "glm", 
                          trControl = rctrl1,
                          preProc = c("center", "scale"))
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "glm",
                            trControl = rctrl2,
                            preProc = c("center", "scale"))


set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "glm", 
                             trControl = rctrl3,
                             tuneLength = 1,
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, testX)

set.seed(849)
test_reg_cv_weight <- train(trainX, trainY, 
                            weights = case_weights,
                            method = "glm", 
                            trControl = cctrl4,
                            tuneLength = 1,
                            preProc = c("center", "scale"))

set.seed(849)
test_reg_loo_weight <- train(trainX, trainY, 
                             weights = case_weights,
                             method = "glm", 
                             trControl = cctrl5,
                             tuneLength = 1,
                             preProc = c("center", "scale"))

set.seed(849)
test_reg_rec <- train(x = rec_reg,
                      data = training,
                      method = "glm", 
                      trControl = rctrl1)

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

if(!interactive())
   q("no")


