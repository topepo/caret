timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "mlpKerasDropoutCost"

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

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "mlpKerasDropoutCost", 
                             trControl = cctrl1,
                             preProc = c("center", "scale"),
                             tuneLength = 2,
                             verbose = 0,
                             epochs = 10)

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "mlpKerasDropoutCost", 
                            trControl = cctrl1,
                            preProc = c("center", "scale"),
                            tuneLength = 2,
                            verbose = 0,
                            epochs = 10)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "mlpKerasDropoutCost", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         preProc = c("center", "scale"),
                         verbose = 0,
                         epochs = 10)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "mlpKerasDropoutCost", 
                              trControl = cctrl2,
                              tuneLength = 2,
                              preProc = c("center", "scale"),
                              verbose = 0,
                              epochs = 5)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "mlpKerasDropoutCost", 
                               trControl = cctrl3,
                               tuneLength = 1,
                               metric = "ROC", 
                               preProc = c("center", "scale"),
                               verbose = 0,
                               epochs = 10)

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "mlpKerasDropoutCost", 
                        trControl = cctrl1,
                        tuneLength = 2,
                        verbose = 0,
                        epochs = 10)

test_class_pred_rec <- predict(test_class_rec, testing[, -ncol(testing)])

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


