timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "ordinalNet"



#########################################################################

set.seed(2)
training <- twoClassSim(100, ordinal = TRUE)
testing <- twoClassSim(500, ordinal = TRUE)
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
                       classProbs = TRUE)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE)

cctrl4 <- trainControl(method = "cv", number = 3, 
                       summaryFunction = weight_test)
cctrl5 <- trainControl(method = "LOOCV", summaryFunction = weight_test)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method ="ordinalNet", 
                             trControl = cctrl1,
                             metric = "Kappa", 
                             preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method ="ordinalNet", 
                            trControl = cctrl1,
                            metric = "Kappa", 
                            preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method ="ordinalNet", 
                              trControl = cctrl2,
                              metric = "Kappa", 
                              preProc = c("center", "scale"))

set.seed(849)

test_class_none_model <- train(trainX, trainY, 
                               method ="ordinalNet", 
                               trControl = cctrl3,
                               tuneGrid = test_class_cv_model$bestTune,
                               metric = "Kappa", 
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_cv_weight <- train(trainX, trainY, 
                              weights = wts,
                              method ="ordinalNet", 
                              trControl = cctrl4,
                              tuneLength = 1,
                              metric = "Accuracy", 
                              preProc = c("center", "scale"))

set.seed(849)
test_class_loo_weight <- train(trainX, trainY, 
                               weights = wts,
                               method ="ordinalNet", 
                               trControl = cctrl5,
                               tuneLength = 1,
                               metric = "Accuracy", 
                               preProc = c("center", "scale"))


set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method ="ordinalNet", 
                             trControl = cctrl1,
                             metric = "Kappa", 
                             preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form_rec <- train(rec_cls, data = training, 
                                method ="ordinalNet", 
                                trControl = cctrl1,
                                metric = "Kappa")

if(
  !isTRUE(
    all.equal(test_class_cv_form$results, 
              test_class_cv_form_rec$results))
)
  stop("CV weights not giving the same results")

set.seed(849)
test_class_loo_model_rec <- train(rec_cls, data = training, 
                                  method ="ordinalNet", 
                                  trControl = cctrl2,
                                  metric = "Kappa")

if(
  !isTRUE(
    all.equal(test_class_loo_model_rec$results, 
              test_class_loo_model$results))
)
  stop("CV weights not giving the same results")


tmp <- training
tmp$wts <- wts

weight_rec <- recipe(Class ~ ., data = tmp) %>%
  update_role(wts, new_role = "case weight") %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

set.seed(849)
test_class_cv_weight_rec <- train(weight_rec, data = tmp,
                                  method ="ordinalNet", 
                                  trControl = cctrl4,
                                  tuneLength = 1,
                                  metric = "Accuracy")

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


