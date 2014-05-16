library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "lm"

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

weight_test <- function (data, lev = NULL, model = NULL)  {
  mean(data$weights)
  postResample(data[, "pred"], data[, "obs"])
}

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")
rctrl4 <- trainControl(method = "cv", summaryFunction = weight_test)
rctrl5 <- trainControl(method = "LOOCV", summaryFunction = weight_test)



set.seed(849)
test_reg_cv_model <- train(trainX, trainY, method = "lm", trControl = rctrl1)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, method = "lm", trControl = rctrl2)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "lm", 
                             trControl = rctrl3,
                             tuneLength = 1,
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, testX)

set.seed(849)
test_reg_cv_weights <- train(trainX, trainY, weights = runif(nrow(trainX)), method = "lm", trControl = rctrl4)

set.seed(849)
test_reg_loo_weights <- train(trainX, trainY, weights = runif(nrow(trainX)), method = "lm", trControl = rctrl5)


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


