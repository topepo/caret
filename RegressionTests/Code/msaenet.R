timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "msaenet"



#########################################################################

set.seed(2)
training <- twoClassSim(100, linearVars = 2)
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
                             method = "msaenet", 
                             trControl = cctrl1,
                             metric = "ROC",
                             preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "msaenet", 
                            trControl = cctrl1,
                            metric = "ROC",
                            preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "msaenet", 
                         trControl = cctrlR,
                         tuneLength = 4)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "msaenet", 
                              trControl = cctrl2,
                              metric = "ROC",
                              preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "msaenet", 
                               trControl = cctrl3,
                               metric = "ROC", 
                               preProc = c("center", "scale"),
                               tuneGrid = expand.grid(alphas = .5,
                                                      nsteps = 2,
                                                      scale = 2))

test_class_none_pred <- predict(test_class_none_model, testX)

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "msaenet", 
                        trControl = cctrl1,
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

set.seed(3677)
reg_training <- SLC14_1(100)
reg_testing <- SLC14_1(500)
reg_trainX <- reg_training[, -ncol(reg_training)]
reg_testX <- reg_testing[, -ncol(reg_testing)]
reg_trainY <- reg_training$y

rec_reg <- recipe(y ~ ., data = reg_training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_reg_cv_model <- train(reg_trainX, reg_trainY, method = "msaenet",
                           preProc = c("center", "scale"),
                           trControl = rctrl1)
test_reg_pred <- predict(test_reg_cv_model, reg_testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., 
                          data = reg_training,
                          method = "msaenet",
                          preProc = c("center", "scale"),
                          trControl = rctrl1)
test_reg_pred_form <- predict(test_reg_cv_form, reg_testX)


set.seed(849)
test_reg_loo_model <- train(reg_trainX, reg_trainY, method = "msaenet",
                            preProc = c("center", "scale"),
                            trControl = rctrl2)

set.seed(849)
test_reg_none_model <- train(reg_trainX, reg_trainY, 
                             method = "msaenet", 
                             trControl = rctrl3,
                             tuneGrid = test_reg_cv_model$bestTune,
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, reg_trainX)

set.seed(849)
test_reg_rec <- train(x = rec_reg,
                      data = reg_training,
                      method = "msaenet", 
                      trControl = rctrl1)

test_reg_pred_rec <- predict(test_reg_rec, reg_testing[, -ncol(reg_testing)])

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


