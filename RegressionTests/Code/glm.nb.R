timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)
library(pscl)

model <- "glm.nb"



#########################################################################

set.seed(1)
in_train <- createDataPartition(bioChemists$art, p = .5, list = FALSE)

training <- bioChemists[ in_train, ]
testing  <- bioChemists[-in_train, ]

trainX <- training[, -1]
testX  <- testing[, -1]
trainY <- training$art
testY <- testing$art

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
test_reg_cv_model <- train(trainX, trainY, method = "glm.nb", trControl = rctrl1, tuneLength = 2)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(art ~ ., data = training, method = "glm.nb", trControl = rctrl1, tuneLength = 2)
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, method = "glm.nb", trControl = rctrl2, tuneLength = 2)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "glm.nb", 
                             trControl = rctrl3,
                             tuneLength = 1,
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, testX)

set.seed(849)
test_reg_cv_weights <- train(trainX, trainY, weights = runif(nrow(trainX)), 
                             method = "glm.nb", trControl = rctrl4, tuneLength = 2)

set.seed(849)
test_reg_loo_weights <- train(trainX, trainY, weights = runif(nrow(trainX)), 
                              method = "glm.nb", trControl = rctrl5, tuneLength = 2)

#########################################################################

test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

test_reg_imp <- varImp(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


