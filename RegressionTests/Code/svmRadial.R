timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "svmRadial"



#########################################################################

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
cctrl2 <- trainControl(method = "LOOCV")
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
cctrlB632 <- trainControl(method = "boot632", number = 10, search = "random", timingSamps = 11, classProbs = TRUE)
cctrlBopt <- trainControl(method = "optimism_boot", number = 10, search = "random", savePredictions = "final", classProbs = TRUE)
cctrlAdapt <- trainControl(method = "adaptive_boot", number = 15, search = "random", classProbs = TRUE)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "svmRadial",
                             tuneGrid = data.frame(.C = c(.25, .5, 1),
                                                   .sigma = .05), 
                             trControl = cctrl1,
                             preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "svmRadial",
                            tuneGrid = data.frame(.C = c(.25, .5, 1),
                                                  .sigma = .05), 
                            trControl = cctrl1,
                            preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "svmRadial", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         preProc = c("center", "scale"))

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "svmRadial",
                              tuneGrid = data.frame(.C = c(.25, .5, 1),
                                                    .sigma = .05), 
                              trControl = cctrl2,
                              preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "svmRadial", 
                               trControl = cctrl3,
                               tuneGrid = test_class_cv_model$bestTune,
                               metric = "ROC", 
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_b632_model <- train(trainX, trainY, 
                               method = "svmRadial", 
                               trControl = cctrlB632,
                               tuneLength = 4,
                               preProc = c("center", "scale"))

set.seed(849)
test_class_bopt_model <- train(trainX, trainY, 
                               method = "svmRadial", 
                               trControl = cctrlBopt,
                               tuneLength = 4,
                               preProc = c("center", "scale"))

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "svmRadial", 
                        tuneGrid = data.frame(.C = c(.25, .5, 1),
                                              .sigma = .05), 
                        trControl = cctrl1)

set.seed(849)
test_class_b632_rec_model <- train(x = rec_cls,
                                   data = training,
                                   method = "svmRadial", 
                                   trControl = cctrlB632,
                                   tuneLength = 4)

if(!isTRUE(all.equal(test_class_b632_rec_model$results, 
                     test_class_b632_rec_model$results)))
  stop("x/y and recipe interface have different results for B632")


set.seed(849)
test_class_bopt_rec_model <- train(x = rec_cls,
                                   data = training,
                                   method = "svmRadial", 
                                   trControl = cctrlBopt,
                                   tuneLength = 4)
if(!isTRUE(all.equal(test_class_bopt_rec_model$results, 
                     test_class_bopt_rec_model$results)))
  stop("x/y and recipe interface have different results for B optim")

# set.seed(849)
# test_class_adapt_model <- train(x = rec_cls,
#                                 data = training, 
#                                 method = "svmRadial", 
#                                 trControl = cctrlAdapt,
#                                 tuneLength = 4,
#                                 preProc = c("center", "scale"))


if(
  !isTRUE(
    all.equal(test_class_cv_model$results, 
              test_class_rec$results))
)
  stop("CV weights not giving the same results")

test_class_imp_rec <- varImp(test_class_rec)


test_class_pred_rec <- predict(test_class_rec, testing[, -ncol(testing)])

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

rec_reg <- recipe(y ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
rctrlB632 <- trainControl(method = "boot632", number = 10, search = "random", 
                          timingSamps = 11, predictionBounds = c(-10, 20))
rctrlBopt <- trainControl(method = "optimism_boot", number = 10, search = "random", 
                          savePredictions = "final", predictionBounds = c(NA, 20))
rctrlAdapt <- trainControl(method = "adaptive_boot", number = 15, search = "random", 
                           predictionBounds = c(TRUE, FALSE))

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "svmRadial",
                           tuneGrid = data.frame(.C = c(.25, .5, 1),
                                                 .sigma = .05), 
                           trControl = rctrl1,
                           preProc = c("center", "scale"))
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "svmRadial",
                          tuneGrid = data.frame(.C = c(.25, .5, 1),
                                                .sigma = .05), 
                          trControl = rctrl1,
                          preProc = c("center", "scale"))
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "svmRadial", 
                       trControl = rctrlR,
                       tuneLength = 4,
                       preProc = c("center", "scale"))

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "svmRadial",
                            tuneGrid = data.frame(.C = c(.25, .5, 1),
                                                  .sigma = .05),
                            trControl = rctrl2,
                            preProc = c("center", "scale"))

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "svmRadial", 
                             trControl = rctrl3,
                             tuneGrid = test_reg_cv_model$bestTune,
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, testX)

set.seed(849)
test_reg_b632 <- train(trainX, trainY, 
                       method = "svmRadial", 
                       trControl = rctrlB632,
                       tuneLength = 4,
                       preProc = c("center", "scale"))

set.seed(849)
test_reg_bopt <- train(trainX, trainY, 
                       method = "svmRadial", 
                       trControl = rctrlBopt,
                       tuneLength = 4,
                       preProc = c("center", "scale"))

set.seed(849)
test_reg_adapt <- train(trainX, trainY, 
                        method = "svmRadial", 
                        trControl = rctrlAdapt,
                        tuneLength = 4,
                        preProc = c("center", "scale"))

set.seed(849)
test_reg_rec <- train(x = rec_reg,
                      data = training,
                      method = "svmRadial", 
                      tuneGrid = data.frame(C = c(.25, .5, 1),
                                            sigma = .05), 
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

test_class_predictors2 <- predictors(test_class_cv_model$finalModel)
test_reg_predictors2 <- predictors(test_reg_cv_model$finalModel)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


