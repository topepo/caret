library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "avNNet"

#########################################################################

set.seed(2)
training <- twoClassSim(100)
testing <- twoClassSim(500)
trainX <- training[, -ncol(training)]
trainY <- training$Class

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "avNNet", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             trace = FALSE,
                             repeats = 2)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "avNNet", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              trace = FALSE,
                              repeats = 2)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "avNNet", 
                               trControl = cctrl3,
                               tuneLength = 1,
                               metric = "ROC", 
                               preProc = c("center", "scale"),
                               trace = FALSE,
                               repeats = 2)

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

test_levels <- levels(test_class_cv_model)

#########################################################################

data(BloodBrain)
bbbDescr <-bbbDescr[, -nearZeroVar(bbbDescr)]
bbbDescr <-bbbDescr[, -findCorrelation(cor(bbbDescr), .5)]

set.seed(2)

inTrain <- createDataPartition(logBBB, p = .5)
trainX <-bbbDescr[inTrain[[1]], ]
trainY <- logBBB[inTrain[[1]]]
testX <- bbbDescr[-inTrain[[1]], ]
testY <- logBBB[-inTrain[[1]]]

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "avNNet", 
                           trControl = rctrl1,
                           preProc = c("center", "scale"),
                           trace = FALSE,
                           repeats = 2)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "avNNet",
                            trControl = rctrl2,
                            preProc = c("center", "scale"),
                            trace = FALSE,
                            repeats = 2)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "avNNet", 
                             trControl = rctrl3,
                             tuneLength = 1,
                             preProc = c("center", "scale"),
                             trace = FALSE,
                             repeats = 2)
test_reg_none_pred <- predict(test_reg_none_model, testX)

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


