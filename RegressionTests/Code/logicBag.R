timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "logicBag"



#########################################################################
gobinary <- function(x){
  out <- apply(x, 2, function(x) ifelse(x > mean(x), 1, 0))
  colnames(out) <- colnames(x)
  out
} 

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
training[, -ncol(training)] <- gobinary(training[, -ncol(training)])
testing[, -ncol(testing)] <- gobinary(testing[, -ncol(testing)])
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())
testX <- testing[, -ncol(testing)]
testY <- testing$Class

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all", seeds = seeds)
cctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
cctrl3 <- trainControl(method = "none", seeds = seeds)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "logicBag", 
                             trControl = cctrl1,
                             tuneGrid = expand.grid(ntrees = 2:3,
                                                    nleaves = 2^(4:5)),
                             B = 3,
                             seed = 1)

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "logicBag", 
                            trControl = cctrl1,
                            tuneGrid = expand.grid(ntrees = 2:3,
                                                   nleaves = 2^(4:5)),
                            B = 3,
                            seed = 1)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "logicBag", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         B = 3,
                         seed = 1)

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

library(caret)
library(plyr)
library(recipes)
library(dplyr)
set.seed(1)
training <- SLC14_1(300)
testing <- SLC14_1(100)
training[, -ncol(training)] <- gobinary(training[, -ncol(training)])
testing[, -ncol(testing)] <- gobinary(testing[, -ncol(testing)])
trainX <- training[, -ncol(training)]
trainY <- training$y

rec_reg <- recipe(y ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 
testX <- testing[, -ncol(testing)]
testY <- testing$y

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all", seeds = seeds)
rctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

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
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "logicBag", 
                          trControl = rctrl1,
                          tuneGrid = expand.grid(ntrees = 2:3,
                                                 nleaves = 2^(4:5)),
                          B = 3,
                          seed = 1)
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "logicBag", 
                       trControl = rctrlR,
                       tuneLength = 4,
                       B = 3,
                       seed = 1)

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
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


