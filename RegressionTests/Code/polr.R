timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "polr"



#########################################################################

set.seed(2)
training <- twoClassSim(100, ordinal = TRUE)
testing <- twoClassSim(500, ordinal = TRUE)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())


library(MASS)
## polr can have issues with starting values
mod <- polr(Class ~ ., data = training)
strt <- c(coef(mod), mod$zeta)

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

cctrl6 <- trainControl(method = "cv", number = 3, summaryFunction = multiClassSummary,
                       classProbs = TRUE)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "polr", 
                             trControl = cctrl1,
                             metric = "Kappa", 
                             start = strt,
                             preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "polr", 
                            trControl = cctrl1, 
                            start = strt,
                            metric = "Kappa", 
                            preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "polr", 
                              start = strt, 
                              trControl = cctrl2,
                              metric = "Kappa", 
                              preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "polr", 
                               trControl = cctrl3, 
                               start = strt,
                               tuneGrid = test_class_cv_model$bestTune,
                               metric = "Kappa", 
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_cv_weight <- train(trainX, trainY, 
                              weights = runif(nrow(trainX)),
                              method = "polr", 
                              trControl = cctrl4, 
                              start = strt,
                              tuneLength = 1,
                              metric = "Accuracy", 
                              preProc = c("center", "scale"))

set.seed(849)
test_class_loo_weight <- train(trainX, trainY, 
                               weights = runif(nrow(trainX)),
                               method = "polr", 
                               trControl = cctrl5,
                               tuneLength = 1, 
                               start = strt,
                               metric = "Accuracy", 
                               preProc = c("center", "scale"))

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "polr", 
                        trControl = cctrl6,
                        metric = "ROC",
                        start = strt) # This will return warnings


if(  !isTRUE(
  all.equal(test_class_cv_model$results[,'Accuracy'], 
            test_class_rec$results[,'Accuracy'])) ){
  stop("CV weights not giving the same results")
  
}

test_class_imp_rec <- varImp(test_class_rec)


test_class_pred_rec <- predict(test_class_rec, testing[, -ncol(testing)])
test_class_prob_rec <- predict(test_class_rec, testing[, -ncol(testing)], 
                               type = "prob")

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


