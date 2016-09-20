library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "gbm_h2o"

library(h2o)
h2o.init()
h2o.no_progress()

#########################################################################

gbmGrid <- expand.grid(max_depth = 11,
                       learn_rate = .1,
                       ntrees = c(10, 50, 100),
                       min_rows = 10, 
                       col_sample_rate = .5)

set.seed(2)
training <- twoClassSim(300, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl3 <- trainControl(method = "oob")
cctrl4 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "gbm_h2o", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             tuneGrid = gbmGrid)

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "gbm_h2o", 
                            trControl = cctrl1,
                            metric = "ROC", 
                            preProc = c("center", "scale"),
                            tuneGrid = gbmGrid)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "gbm_h2o", 
                         trControl = cctrlR,
                         tuneLength = 4)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "gbm_h2o", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              tuneGrid = gbmGrid)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "gbm_h2o", 
                               trControl = cctrl4,
                               tuneGrid = gbmGrid[nrow(gbmGrid),],
                               metric = "ROC", 
                               preProc = c("center", "scale"))
test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

set.seed(1)
training <- SLC14_1(75)
testing <- SLC14_1(100)
trainX <- training[, -ncol(training)]
trainY <- training$y
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "oob")
rctrl4 <- trainControl(method = "none")
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "gbm_h2o", 
                           trControl = rctrl1,
                           preProc = c("center", "scale"),
                           tuneGrid = gbmGrid)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "gbm_h2o", 
                          trControl = rctrl1,
                          preProc = c("center", "scale"),
                          tuneGrid = gbmGrid)
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "gbm_h2o", 
                       trControl = rctrlR,
                       tuneLength = 4)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "gbm_h2o",
                            trControl = rctrl2,
                            preProc = c("center", "scale"),
                            tuneGrid = gbmGrid)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "gbm_h2o", 
                             trControl = rctrl4,
                             tuneGrid = gbmGrid[nrow(gbmGrid),],
                             preProc = c("center", "scale"))
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

#q("no")


