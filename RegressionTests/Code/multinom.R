timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "multinom"



#########################################################################

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

wts <- runif(nrow(trainX))

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
cctrl6 <- trainControl(method = "LOOCV", classProbs = TRUE, 
                       summaryFunction = multiClassSummary)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "multinom", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             trace = FALSE)

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "multinom", 
                            trControl = cctrl1,
                            metric = "ROC", 
                            preProc = c("center", "scale"),
                            trace = FALSE)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "multinom", 
                         trControl = cctrlR,
                         tuneLength = 4, 
                         preProc = c("center", "scale"),
                         trace = FALSE)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "multinom", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              trace = FALSE)

set.seed(849)
test_class_loo_3class_model <- 
  train(Species ~ ., 
        data = iris,
        method = "multinom", 
        trControl = cctrl6,
        metric = "AUC", 
        preProc = c("center", "scale"),
        trace = FALSE)


set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "multinom", 
                               trControl = cctrl3,
                               tuneLength = 1,
                               metric = "ROC", 
                               preProc = c("center", "scale"),
                               trace = FALSE)

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_cv_weight <- train(trainX, trainY, 
                              weights = wts,
                              method = "multinom", 
                              trControl = cctrl4,
                              tuneLength = 2,
                              metric = "Accuracy", 
                              preProc = c("center", "scale"),
                              trace = FALSE)

set.seed(849)
test_class_loo_weight <- train(trainX, trainY, 
                               weights = wts, 
                               method = "multinom", 
                               trControl = cctrl5,
                               tuneLength = 2,
                               metric = "Accuracy", 
                               preProc = c("center", "scale"),
                               trace = FALSE)

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "multinom", 
                        trControl = cctrl1,
                        metric = "ROC",
                        trace = FALSE)


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
tmp$wts <- wts

weight_rec <- recipe(Class ~ ., data = tmp) %>%
  update_role(wts, new_role = "case weight") %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

set.seed(849)
test_class_cv_weight_rec <- train(weight_rec, data = tmp,
                                  method = "multinom", 
                                  trControl = cctrl4,
                                  tuneLength = 2,
                                  metric = "Accuracy",
                                  trace = FALSE)

if(
  !isTRUE(
    all.equal(test_class_cv_weight_rec$results, 
              test_class_cv_weight$results))
)
  stop("CV weights not giving the same results")


test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)

#########################################################################

test_class_imp <- varImp(test_class_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


