library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "multinom"

#########################################################################

set.seed(2)
training <- twoClassSim(100)
testing <- twoClassSim(500)
trainX <- training[, -ncol(training)]
trainY <- training$Class

weight_test <- function (data, lev = NULL, model = NULL)  {
  mean(data$weights)
  postResample(data[, "pred"], data[, "obs"])
}

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)

cctrl4 <- trainControl(method = "cv", number = 3, 
                       summaryFunction = weight_test)
cctrl5 <- trainControl(method = "LOOCV", summaryFunction = weight_test)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "multinom", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             trace = FALSE)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "multinom", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              trace = FALSE)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "multinom", 
                               trControl = cctrl3,
                               tuneLength = 1,
                               metric = "ROC", 
                               preProc = c("center", "scale"),
                               trace = FALSE)

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_cv_weight <- train(trainX, trainY, 
                              weights = runif(nrow(trainX)),
                              method = "multinom", 
                              trControl = cctrl4,
                              tuneLength = 2,
                              metric = "Accuracy", 
                              preProc = c("center", "scale"),
                              trace = FALSE)

set.seed(849)
test_class_loo_weight <- train(trainX, trainY, 
                               weights = runif(nrow(trainX)), 
                               method = "multinom", 
                               trControl = cctrl5,
                               tuneLength = 2,
                               metric = "Accuracy", 
                               preProc = c("center", "scale"),
                               trace = FALSE)


test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)

#########################################################################

test_class_imp <- varImp(test_class_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


