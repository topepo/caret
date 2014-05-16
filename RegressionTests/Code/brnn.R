library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "brnn"

#########################################################################

data(BloodBrain)
bbbDescr <-bbbDescr[, -nearZeroVar(bbbDescr)]
bbbDescr <-bbbDescr[, -findCorrelation(cor(bbbDescr), .5)]

set.seed(2)

inTrain <- createDataPartition(logBBB, p = .90)
trainX <-bbbDescr[inTrain[[1]], ]
trainY <- logBBB[inTrain[[1]]]
testX <- bbbDescr[-inTrain[[1]], ]
testY <- logBBB[-inTrain[[1]]]

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")

set.seed(849)
test_reg_cv_model <- train(trainX[, 1:3], trainY, method = "brnn", trControl = rctrl1)
test_reg_pred <- predict(test_reg_cv_model, testX[, 1:3])

set.seed(849)
test_reg_loo_model <- train(trainX[, 1:3], trainY, method = "brnn", trControl = rctrl2)

set.seed(849)
test_reg_none_model <- train(trainX[, 1:3], trainY, 
                             method = "lm", 
                             trControl = rctrl3,
                             tuneLength = test_reg_cv_model$bestTune,
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, testX[, 1:3])

#########################################################################

test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

test_reg_imp <- varImp(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


