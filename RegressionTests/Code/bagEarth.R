library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "bagEarth"

#########################################################################

set.seed(2)
training <- twoClassSim(100)
testing <- twoClassSim(500)
trainX <- training[, -ncol(training)]
trainY <- training$Class

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary,
                       seed = list(a = 1:9, b = 1:9, c = 1:9, d = 10))


seeds <- vector(mode = "list", length = 189)
for(i in 1:189) seeds[[i]] <- i:(i+3)
seeds[[189]] <- 1

cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary,
                       seeds= seeds)
cctrl3 <- trainControl(method = "oob")
cctrl4 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary,
                       seeds= seeds)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "bagEarth", 
                             trControl = cctrl1,
                             tuneGrid = data.frame(.degree = 1,
                                                   .nprune = 2:4),
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             B = 10)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "bagEarth", 
                              trControl = cctrl2,
                              tuneGrid = data.frame(.degree = 1,
                                                    .nprune = 2:4),
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              B = 10)
test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")
  
set.seed(849)
test_class_oob_model <- train(trainX, trainY, 
                              method = "bagEarth",
                              tuneGrid = data.frame(.degree = 1,
                                                    .nprune = 2:4), 
                              trControl = cctrl3,
                              B = 10)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "bagEarth", 
                               trControl = cctrl4,
                               tuneLength = 1, 
                               preProc = c("center", "scale"),
                               B = 10)
test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

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

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       seed = list(a = 1:9, b = 1:9, c = 1:9, d = 10))

seeds <- vector(mode = "list", length = 189)
for(i in 1:189) seeds[[i]] <- i:(i+3)
seeds[[189]] <- 1

rctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
rctrl3 <- trainControl(method = "oob")
rctrl4 <- trainControl(method = "none", seeds = seeds)

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "bagEarth", 
                           trControl = rctrl1,
                           preProc = c("center", "scale"),
                           tuneGrid = data.frame(.degree = 1,
                                                 .nprune = 2:4),
                           B = 10)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "bagEarth",
                            trControl = rctrl2,
                            preProc = c("center", "scale"),
                            tuneGrid = data.frame(.degree = 1,
                                                  .nprune = 2:4),
                            B = 10)

set.seed(849)
test_reg_oob_model <- train(trainX, trainY, 
                            method = "bagEarth",
                            trControl = rctrl3,
                            preProc = c("center", "scale"),
                            tuneGrid = data.frame(.degree = 1,
                                                  .nprune = 2:4),
                            B = 10)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "bagEarth", 
                             trControl = rctrl4,
                             tuneLength = 1,
                             preProc = c("center", "scale"),
                             B = 10)
test_reg_none_pred <- predict(test_reg_none_model, testX)

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

test_class_imp <- varImp(test_class_cv_model)
test_reg_imp <- varImp(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


