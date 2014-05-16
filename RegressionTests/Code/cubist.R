library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "cubist"

#########################################################################

data(BloodBrain)
bbbDescr <-bbbDescr[, -nearZeroVar(bbbDescr)]
bbbDescr <-bbbDescr[, -findCorrelation(cor(bbbDescr), .5)]

set.seed(2)

inTrain <- createDataPartition(logBBB, p = .50)
trainX <-bbbDescr[inTrain[[1]], ]
trainY <- logBBB[inTrain[[1]]]
testX <- bbbDescr[-inTrain[[1]], ]
testY <- logBBB[-inTrain[[1]]]

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, method = "cubist", 
                           trControl = rctrl1,
                           control = cubistControl(seed = 1))
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, method = "cubist", 
                            trControl = rctrl1,
                            control = cubistControl(seed = 1))

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "cubist", 
                             trControl = rctrl3,
                             tuneGrid = data.frame(committees = 5, neighbors = 3),
                             preProc = c("center", "scale"),
                             control = cubistControl(seed = 1))
test_reg_none_pred <- predict(test_reg_none_model, testX)

#########################################################################

test_reg_imp <- varImp(test_reg_cv_model)

#########################################################################

sInfo <- sessionInfo()

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


