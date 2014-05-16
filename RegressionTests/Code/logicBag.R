library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "logicBag"

#########################################################################
gobinary <- function(x){
  out <- apply(x, 2, function(x) ifelse(x > mean(x), 1, 0))
  colnames(out) <- colnames(x)
  out
} 

set.seed(2)
training <- twoClassSim(100)
testing <- twoClassSim(500)
trainX <- gobinary(training[, -ncol(training)])
trainY <- training$Class
testX <- gobinary(testing[, -ncol(testing)])
testY <- testing$Class

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
cctrl2 <- trainControl(method = "LOOCV")
cctrl3 <- trainControl(method = "none")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "logicBag", 
                             trControl = cctrl1,
                             tuneGrid = expand.grid(ntrees = 2:3,
                                                    nleaves = 2^(4:5)),
                             B = 3,
                             seed = 1)

test_class_pred <- predict(test_class_cv_model, testX)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "logicBag", 
                              trControl = cctrl2,
                              tuneGrid = expand.grid(ntrees = 2:3,
                                                     nleaves = 2^(4:5)),
                              B = 3,
                              seed = 1)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "logicBag", 
                               trControl = cctrl3,
                               tuneGrid = test_class_cv_model$bestTune,
                               B = 3,
                               seed = 1)

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

data(BloodBrain)
bbbDescr <-bbbDescr[, -nearZeroVar(bbbDescr)]
bbbDescr <-bbbDescr[, -findCorrelation(cor(bbbDescr), .5)]
numVal <- apply(bbbDescr, 2, function(x) length(unique(x)))
bbbDescr <- bbbDescr[, numVal > 10]

set.seed(2)

inTrain <- createDataPartition(logBBB, p = .5)
trainX <-gobinary(bbbDescr[inTrain[[1]], ])
trainY <- logBBB[inTrain[[1]]]
testX <- gobinary(bbbDescr[-inTrain[[1]], ])
testY <- logBBB[-inTrain[[1]]]

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "logicBag", 
                           trControl = rctrl1,
                           tuneGrid = expand.grid(ntrees = 2:3,
                                                  nleaves = 2^(4:5)),
                           B = 3,
                           seed = 1)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "logicBag",
                            trControl = rctrl2,
                            tuneGrid = expand.grid(ntrees = 2:3,
                                                   nleaves = 2^(4:5)),
                            B = 3,
                            seed = 1)

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


