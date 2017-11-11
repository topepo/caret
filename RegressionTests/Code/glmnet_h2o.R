timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "glmnet_h2o"



library(h2o)
h2o.init()
h2o.no_progress()

#########################################################################

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
testX <- testing[, -ncol(testing)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "glmnet_h2o", 
                             trControl = cctrl1,
                             metric = "ROC",
                             preProc = c("center", "scale"),
                             tuneGrid = expand.grid(alpha = c(0, .5, 1),
                                                    lambda = c(.1, 1)),
                             seed = 1311)

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "glmnet_h2o", 
                            trControl = cctrl1,
                            metric = "ROC",
                            preProc = c("center", "scale"),
                            tuneGrid = expand.grid(alpha = c(0, .5, 1),
                                                   lambda = c(.1, 1)),
                            seed = 1311)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "glmnet_h2o", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         seed = 1311)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "glmnet_h2o", 
                              trControl = cctrl2,
                              metric = "ROC",
                              preProc = c("center", "scale"),
                              tuneGrid = expand.grid(alpha = c(0, .5, 1),
                                                     lambda = c(.1, 1)),
                              seed = 1311)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "glmnet_h2o", 
                               trControl = cctrl3,
                               metric = "ROC", 
                               preProc = c("center", "scale"),
                               tuneGrid = expand.grid(alpha = c(.11),
                                                      lambda = c(.5)),
                               seed = 1311)

test_class_none_pred <- predict(test_class_none_model, testX)

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "glmnet_h2o", 
                        trControl = cctrl1,
                        tuneGrid = expand.grid(alpha = c(0, .5, 1),
                                                    lambda = c(.1, 1)),
                        seed = 1311,
                        metric = "ROC")


if(
  !isTRUE(
    all.equal(test_class_cv_model$results, 
              test_class_rec$results))
)
  stop("CV weights not giving the same results")

test_class_imp_rec <- varImp(test_class_rec)


test_class_pred_rec <- predict(test_class_rec, testing[, -ncol(testing)])
test_class_prob_rec <- predict(test_class_rec, testing[, -ncol(testing)], 
                               type = "prob")

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

set.seed(45)
reg_dat_tr <- SLC14_1(100)
reg_dat_te <- SLC14_1(100)

rec_reg <- recipe(y ~ ., data = reg_dat_tr) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

reg_grid <- expand.grid(alpha = c(0, .5), lambda = c(.01, .001))

set.seed(849)
test_reg_cv_model <- train(reg_dat_tr[, -ncol(reg_dat_tr)], reg_dat_tr$y, method = "glmnet_h2o",
                           preProc = c("center", "scale"),
                           trControl = rctrl1,
                           tuneGrid = reg_grid,
                           seed = 1311)
test_reg_pred <- predict(test_reg_cv_model, reg_dat_te[, -ncol(reg_dat_te)])

set.seed(849)
test_reg_none_model <- train(reg_dat_tr[, -ncol(reg_dat_tr)], reg_dat_tr$y, 
                             method = "glmnet_h2o", 
                             trControl = rctrl3,
                             tuneGrid = test_reg_cv_model$bestTune,
                             preProc = c("center", "scale"),
                             seed = 1311)
test_reg_none_pred <- predict(test_reg_none_model, reg_dat_te[, -ncol(reg_dat_te)])

set.seed(849)
test_reg_rec <- train(x = rec_reg,
                      data = reg_dat_tr,
                      method = "glmnet_h2o", 
                      trControl = rctrl1,
                      tuneGrid = reg_grid,
                      seed = 1311)

if(
  !isTRUE(
    all.equal(test_reg_cv_model$results, 
              test_reg_rec$results))
)
  stop("CV weights not giving the same results")

test_reg_imp_rec <- varImp(test_reg_rec)


test_reg_pred_rec <- predict(test_reg_rec, reg_dat_te[, -ncol(reg_dat_te)])

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_reg_predictors1 <- predictors(test_reg_cv_model)

test_class_imp1 <- varImp(test_class_cv_model)
test_reg_imp1 <- varImp(test_reg_cv_model)


#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


