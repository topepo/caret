timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "rfRules"



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

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       verboseIter = TRUE,
                       seeds = seeds)
cctrl2 <- trainControl(method = "LOOCV",
                       seeds = seeds)
cctrl4 <- trainControl(method = "none",
                       seeds = seeds)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

grid <- expand.grid(mtry = c(1, 3), maxdepth = c(3, 7))

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "rfRules", 
                             trControl = cctrl1,
                             preProc = c("center", "scale"),
                             tuneGrid = grid,
                             ntree = 20)

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "rfRules", 
                            trControl = cctrl1,
                            preProc = c("center", "scale"),
                            tuneGrid = grid,
                            ntree = 20)


test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "rfRules", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         ntree = 20)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "rfRules", 
                              trControl = cctrl2,
                              preProc = c("center", "scale"),
                              tuneGrid = grid,
                              ntree = 20)
test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "rfRules", 
                               trControl = cctrl4,
                               preProc = c("center", "scale"),
                               tuneGrid = grid[2,],
                               ntree = 20)

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "rfRules", 
                        trControl = cctrl1,
                        tuneGrid = grid,
                        ntree = 20)


if(
  !isTRUE(
    all.equal(test_class_cv_model$results, 
              test_class_rec$results))
)
  stop("CV weights not giving the same results")

test_class_imp_rec <- varImp(test_class_rec)


test_class_pred_rec <- predict(test_class_rec, testing[, -ncol(testing)])

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
rctrl4 <- trainControl(method = "none", seeds = seeds)
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "rfRules", 
                           trControl = rctrl1,
                           preProc = c("center", "scale"),
                           tuneGrid = grid,
                           ntree = 20)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "rfRules", 
                          trControl = rctrl1,
                          preProc = c("center", "scale"),
                          tuneGrid = grid,
                          ntree = 20)
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "rfRules", 
                       trControl = rctrlR,
                       tuneLength = 4,
                       ntree = 20)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "rfRules",
                            trControl = rctrl2,
                            preProc = c("center", "scale"),
                            tuneGrid = grid,
                            ntree = 20)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "rfRules", 
                             trControl = rctrl4,
                             preProc = c("center", "scale"),
                             tuneGrid = grid[2,],
                             ntree = 20)
test_reg_none_pred <- predict(test_reg_none_model, testX)

set.seed(849)
test_reg_rec <- train(x = rec_reg,
                      data = training,
                      method = "rfRules", 
                      trControl = rctrl1,
                      tuneGrid = grid,
                      ntree = 20)

if(
  !isTRUE(
    all.equal(test_reg_cv_model$results, 
              test_reg_rec$results))
)
  stop("CV weights not giving the same results")

test_reg_imp_rec <- varImp(test_reg_rec)


test_reg_pred_rec <- predict(test_reg_rec, testing[, -ncol(testing)])

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

test_class_imp <- varImp(test_class_cv_model)
test_reg_imp <- varImp(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


