timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "lda2"


#########################################################################

set.seed(1)

library(caret)
library(plyr)
library(recipes)
library(dplyr)

library(mlbench)
data(Glass)
Glass$Type <- factor(paste("Class", as.numeric(Glass$Type), sep = ""))

set.seed(2)

inTrain <- createDataPartition(Glass$Type, p = .90)
trainX <-Glass[inTrain[[1]], -ncol(Glass)]
trainY <-Glass$Type[inTrain[[1]]]
testX <-Glass[-inTrain[[1]], -ncol(Glass)]
testY <- Glass$Type[-inTrain[[1]]]
training <-Glass[inTrain[[1]], ]
testing <-Glass[-inTrain[[1]], ]

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "lda2", 
                             trControl = cctrl1,
                             preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form <- train(Type ~ ., data = training, 
                            method = "lda2", 
                            trControl = cctrl1,
                            preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                            method = "lda2", 
                            trControl = cctrl2,
                            preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "lda2", 
                               trControl = cctrl3,
                               tuneGrid = test_class_cv_model$bestTune,
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testX)
test_class_none_prob <- predict(test_class_none_model, testX, type = "prob")

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


