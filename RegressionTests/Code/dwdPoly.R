library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "dwdPoly"

#########################################################################

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
cctrl2 <- trainControl(method = "LOOCV")
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "dwdPoly", 
                             trControl = cctrl1,
                             preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "dwdPoly", 
                            trControl = cctrl1,
                            preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "dwdPoly", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         preProc = c("center", "scale"))

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "dwdPoly", 
                              trControl = cctrl2,
                              preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "dwdPoly", 
                               trControl = cctrl3,
                               tuneGrid = test_class_cv_model$bestTune,
                               metric = "ROC", 
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")


#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)

test_class_predictors2 <- predictors(test_class_cv_model$finalModel)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


