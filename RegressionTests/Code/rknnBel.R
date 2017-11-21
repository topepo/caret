timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "rknnBel"



#########################################################################

set.seed(2)
training <- twoClassSim(50, noiseVars = 2)
testing <- twoClassSim(500, noiseVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

cctrl1 <- trainControl(method = "cv", number = 3, seeds = seeds)
cctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
cctrl3 <- trainControl(method = "none", seeds = seeds)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "rknnBel", 
                             trControl = cctrl1, 
                             tuneLength = 2,
                             preProc = c("center", "scale"), 
                             seed = 135)

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "rknnBel", 
                            trControl = cctrl1, 
                            tuneLength = 2,
                            preProc = c("center", "scale"), 
                            seed = 135)
test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "rknnBel", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         preProc = c("center", "scale"), 
                         seed = 135)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "rknnBel", 
                              trControl = cctrl2, 
                              tuneLength = 2,
                              preProc = c("center", "scale"), 
                              seed = 135)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "rknnBel", 
                               trControl = cctrl3,
                               tuneGrid = test_class_cv_model$bestTune, 
                               preProc = c("center", "scale"), 
                               seed = 135)

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "rknnBel", 
                        tuneLength = 2,
                        trControl = cctrl1,
                        seed = 135)


if(
  !isTRUE(
    all.equal(test_class_cv_model$results, 
              test_class_rec$results))
)
  stop("CV weights not giving the same results")

test_class_imp_rec <- varImp(test_class_rec)


test_class_pred_rec <- predict(test_class_rec, testing[, -ncol(testing)])

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

library(caret)
library(plyr)
library(recipes)
library(dplyr)
grid <- expand.grid(k = c(5, 10), d = c(1, 2), mtry = 2)

set.seed(1)
training <- SLC14_1(50)
testing <- SLC14_1(100)
trainX <- training[, -ncol(training)]
trainY <- training$y

rec_reg <- recipe(y ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all", seeds = seeds)
rctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
rctrl3 <- trainControl(method = "none", seeds = seeds)
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "rknnBel", 
                           trControl = rctrl1, 
                           tuneGrid = grid,
                           preProc = c("center", "scale"), 
                           seed = 135)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "rknnBel", 
                          trControl = rctrl1, 
                          tuneGrid = grid,
                          preProc = c("center", "scale"), 
                          seed = 135)
test_reg_pred_form <- predict(test_reg_cv_form, testX)


set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "rknnBel", 
                       trControl = rctrlR,
                       tuneLength = 4,
                       preProc = c("center", "scale"), 
                       seed = 135)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "rknnBel",
                            trControl = rctrl2, 
                            tuneGrid = grid,
                            preProc = c("center", "scale"), 
                            seed = 135)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "rknnBel", 
                             trControl = rctrl3,
                             tuneGrid = test_reg_cv_model$bestTune,
                             preProc = c("center", "scale"), 
                             seed = 135)
test_reg_none_pred <- predict(test_reg_none_model, testX)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

# q("no")


