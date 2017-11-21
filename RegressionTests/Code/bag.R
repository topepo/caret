timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "bag"



library(MASS)
library(party)

#########################################################################

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
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
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary,
                       seeds = seeds)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")


set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "bag", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                             bagControl = bagControl(fit = ldaBag$fit,
                                                     predict = ldaBag$pred,
                                                     aggregate = ldaBag$aggregate))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                             method = "bag", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                             bagControl = bagControl(fit = ldaBag$fit,
                                                     predict = ldaBag$pred,
                                                     aggregate = ldaBag$aggregate))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "bag", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         preProc = c("center", "scale"),
                         bagControl = bagControl(fit = ldaBag$fit,
                                                 predict = ldaBag$pred,
                                                 aggregate = ldaBag$aggregate))

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "bag", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                              bagControl = bagControl(fit = ldaBag$fit,
                                                      predict = ldaBag$pred,
                                                      aggregate = ldaBag$aggregate),
                              preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "bag", 
                               trControl = cctrl3,
                               metric = "ROC", 
                               bagControl = bagControl(fit = ldaBag$fit,
                                                       predict = ldaBag$pred,
                                                       aggregate = ldaBag$aggregate),
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "bag", 
                        trControl = cctrl1,
                        metric = "ROC",
                        tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                        bagControl = bagControl(fit = ldaBag$fit,
                                                predict = ldaBag$pred,
                                                aggregate = ldaBag$aggregate))


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

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)

#########################################################################

test_class_imp <- varImp(test_class_cv_model)

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

rec_reg <- recipe(y ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

rctrl1 <- trainControl(method = "cv", number = 3, 
                       returnResamp = "all",
                       seed = seeds)
rctrl2 <- trainControl(method = "LOOCV", seed = seeds)
rctrl3 <- trainControl(method = "none", seed = seeds)
rctrl4 <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")


set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "bag", 
                           trControl = rctrl1,
                           tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                           bagControl = bagControl(fit = ctreeBag$fit,
                                                   predict = ctreeBag$pred,
                                                   aggregate = ctreeBag$aggregate))

test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                           method = "bag", 
                           trControl = rctrl1,
                           tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                           bagControl = bagControl(fit = ctreeBag$fit,
                                                   predict = ctreeBag$pred,
                                                   aggregate = ctreeBag$aggregate))

test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "bag", 
                       trControl = rctrl4,
                       tuneLength = 4,
                       bagControl = bagControl(fit = ctreeBag$fit,
                                               predict = ctreeBag$pred,
                                               aggregate = ctreeBag$aggregate))

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "bag", 
                            trControl = rctrl2,
                            tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                            bagControl = bagControl(fit = ctreeBag$fit,
                                                    predict = ctreeBag$pred,
                                                    aggregate = ctreeBag$aggregate))


set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "bag", 
                             trControl = rctrl3,
                             bagControl = bagControl(fit = ctreeBag$fit,
                                                     predict = ctreeBag$pred,
                                                     aggregate = ctreeBag$aggregate))

test_reg_none_pred <- predict(test_reg_none_model, testX)

set.seed(849)
test_reg_rec <- train(x = rec_reg,
                      data = training,
                      method = "bag", 
                      trControl = rctrl1,
                      tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                      bagControl = bagControl(fit = ctreeBag$fit,
                                              predict = ctreeBag$pred,
                                              aggregate = ctreeBag$aggregate))

if(
  !isTRUE(
    all.equal(test_reg_cv_model$results, 
              test_reg_rec$results))
)
  stop("CV weights not giving the same results")

test_reg_imp_rec <- varImp(test_reg_rec)


test_reg_pred_rec <- predict(test_reg_rec, testing[, -ncol(testing)])

#########################################################################

test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


