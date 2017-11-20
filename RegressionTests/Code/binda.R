timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "binda"



#########################################################################
library(QSARdata)
data(bbb2)

trainX <- as.matrix(bbb2_Daylight_FP[, -1])
trainY <- bbb2_Outcome$Class
training <- bbb2_Daylight_FP[, -1]
training$Class <- bbb2_Outcome$Class
testing <- training[1:20,]

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "binda", 
                             trControl = cctrl1,
                             verbose = FALSE,
                             metric = "ROC")

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "binda", 
                            trControl = cctrl1,
                            verbose = FALSE,
                            metric = "ROC")

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "binda", 
                         trControl = cctrlR,
                         tuneLength = 4)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "binda", 
                              trControl = cctrl2,
                              verbose = FALSE,
                              metric = "ROC")

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "binda", 
                               trControl = cctrl3,
                               verbose = FALSE,
                               metric = "ROC", 
                               tuneGrid = test_class_cv_model$bestTune)

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")


test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


