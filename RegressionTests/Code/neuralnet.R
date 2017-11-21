timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "neuralnet"



#########################################################################

library(caret)
library(plyr)
library(recipes)
library(dplyr)
set.seed(1)
training <- SLC14_1(30)[, 18:21]
testing <- SLC14_1(100)[, 18:21]
trainX <- training[, -ncol(training)]
trainY <- training$y

rec_reg <- recipe(y ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, method = "neuralnet", trControl = rctrl1,
                           tuneGrid = data.frame(layer1 = 2:3, layer2 = 0, layer3 = 0),
                           rep = 3,
                           threshold = 0.1,        
                           stepmax = 1e+05,
                           preProc = c("center", "scale"))
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "neuralnet", trControl = rctrl1,
                          tuneGrid = data.frame(layer1 = 2:3, layer2 = 0, layer3 = 0),
                          rep = 3,
                          threshold = 0.1,
                          startweights = test_reg_cv_model$finalModel$weights,
                          preProc = c("center", "scale"))
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "neuralnet", 
                       trControl = rctrlR,
                       tuneLength = 4,
                       rep = 10,
                       threshold = 0.1,
                       preProc = c("center", "scale"))

set.seed(849)
test_reg_loo_model <- train(y ~ ., data = training, 
                            method = "neuralnet", 
                            trControl = rctrl2,
                            tuneGrid = data.frame(layer1 = 2:3, layer2 = 0, layer3 = 0),
                            rep = 3,
                            threshold = 0.1,
                            startweights = test_reg_cv_model$finalModel$weights,
                            preProc = c("center", "scale"))

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "neuralnet", 
                             trControl = rctrl3,
                             tuneLength = 1,
                             rep = 3,
                             threshold = 0.1,
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, testX)

set.seed(849)
test_reg_rec <- train(x = rec_reg,
                      data = training,
                      method = "neuralnet", 
                      trControl = rctrl1,
                      tuneGrid = data.frame(layer1 = 2:3, layer2 = 0, layer3 = 0),
                      rep = 3,
                      threshold = 0.1,        
                      stepmax = 1e+05)

if(
  !isTRUE(
    all.equal(test_reg_cv_model$results, 
              test_reg_rec$results))
)
  stop("CV weights not giving the same results")

test_reg_imp_rec <- varImp(test_reg_rec)


test_reg_pred_rec <- predict(test_reg_rec, testing[, -ncol(testing)])

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


