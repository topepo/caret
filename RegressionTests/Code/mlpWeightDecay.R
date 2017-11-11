timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "mlpWeightDecay"



#########################################################################

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all", seeds = seeds)
cctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
cctrl3 <- trainControl(method = "none", seeds = seeds)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

library(RSNNS)

grid <- expand.grid(.decay = c(0, .01), .size = 1:3)

set.seed(849)
test_class_cv_model <- caret:::train(trainX, trainY, 
                                     method = "mlpWeightDecay", 
                                     trControl = cctrl1,
                                     tuneGrid = grid,
                                     preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form <- caret:::train(Class ~ ., data = training, 
                                    method = "mlpWeightDecay", 
                                    trControl = cctrl1,
                                    tuneGrid = grid,
                                    preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(81)
test_class_rand <- caret:::train(trainX, trainY, 
                                 method = "mlpWeightDecay", 
                                 trControl = cctrlR,
                                 tuneLength = 4)

set.seed(849)
test_class_loo_model <- caret:::train(trainX, trainY, 
                                      method = "mlpWeightDecay", 
                                      trControl = cctrl2,
                                      tuneGrid = grid,
                                      preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- caret:::train(trainX, trainY, 
                                       method = "mlpWeightDecay", 
                                       trControl = cctrl3,
                                       tuneLength = 1,
                                       preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])

set.seed(849)
test_class_rec <- caret::train(x = rec_cls,
                               data = training,
                               method = "mlpWeightDecay", 
                               tuneGrid = grid,
                               trControl = cctrl1)


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
set.seed(1)
training <- SLC14_1(30)
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
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_reg_cv_model <- caret:::train(trainX, trainY, 
                                   method = "mlpWeightDecay",
                                   tuneGrid = grid, 
                                   trControl = rctrl1,
                                   preProc = c("center", "scale"))
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- caret:::train(y ~ ., data = training, 
                                  method = "mlpWeightDecay", 
                                  tuneGrid = grid,
                                  trControl = rctrl1,
                                  preProc = c("center", "scale"))
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_rand <- caret:::train(trainX, trainY, 
                               method = "mlpWeightDecay", 
                               trControl = rctrlR,
                               tuneLength = 4,
                               preProc = c("center", "scale"))

set.seed(849)
test_reg_loo_model <- caret:::train(trainX, trainY, 
                                    method = "mlpWeightDecay",
                                    tuneGrid = grid,
                                    trControl = rctrl2,
                                    preProc = c("center", "scale"))
set.seed(849)
test_reg_rec <- caret:::train(x = rec_reg,
                              data = training,
                              method = "mlpWeightDecay", 
                              tuneGrid = grid,
                              trControl = rctrl1)

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


