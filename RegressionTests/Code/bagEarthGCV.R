timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "bagEarthGCV"



#########################################################################

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary,
                       seed = list(a = 1:9, b = 1:9, c = 1:9, d = 10))


seeds <- vector(mode = "list", length = 189)
for(i in 1:189) seeds[[i]] <- i:(i+3)
seeds[[189]] <- 1

cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary,
                       seeds= seeds)
cctrl3 <- trainControl(method = "oob")
cctrl4 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary,
                       seeds= seeds)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
cctrl5 <- trainControl(method = "cv",
                       classProbs = TRUE, summaryFunction = multiClassSummary,
                       seeds= seeds)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "bagEarthGCV", 
                             trControl = cctrl1,
                             tuneGrid = data.frame(degree = 1:2),
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             B = 10)

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "bagEarthGCV", 
                            trControl = cctrl1,
                            tuneGrid = data.frame(degree = 1:2),
                            metric = "ROC", 
                            preProc = c("center", "scale"),
                            B = 10)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "bagEarthGCV", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         preProc = c("center", "scale"),
                         glm = list(family = binomial, control = list(maxit = 50)))

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "bagEarthGCV", 
                              trControl = cctrl2,
                              tuneGrid = data.frame(degree = 1:2),
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              B = 10)
test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

set.seed(849)
test_class_oob_model <- train(trainX, trainY, 
                              method = "bagEarthGCV",
                              tuneGrid = data.frame(degree = 1:2), 
                              trControl = cctrl3,
                              B = 10)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "bagEarthGCV", 
                               trControl = cctrl4,
                               tuneLength = 1, 
                               preProc = c("center", "scale"),
                               B = 10)
test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "bagEarthGCV", 
                        trControl = cctrl1,
                        tuneGrid = data.frame(degree = 1:2),
                        metric = "ROC",
                        B = 10)


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


set.seed(849)
test_3class_cv_model <- train(iris[, 1:4], iris$Species, 
                              method = "bagEarthGCV", 
                              trControl = cctrl5,
                              tuneGrid = data.frame(degree = 1:2),
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              B = 10)

test_3class_pred <- predict(test_3class_cv_model, iris[1:5, 1:4])
test_3class_prob <- predict(test_3class_cv_model, iris[1:5, 1:4], 
                            type = "prob")

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

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       seed = list(a = 1:9, b = 1:9, c = 1:9, d = 10))

seeds <- vector(mode = "list", length = 189)
for(i in 1:189) seeds[[i]] <- i:(i+3)
seeds[[189]] <- 1

rctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
rctrl3 <- trainControl(method = "oob")
rctrl4 <- trainControl(method = "none", seeds = seeds)
rctrl5 <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")


set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "bagEarthGCV", 
                           trControl = rctrl1,
                           preProc = c("center", "scale"),
                           tuneGrid = data.frame(degree = 1:2),
                           B = 10)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "bagEarthGCV", 
                          trControl = rctrl1,
                          preProc = c("center", "scale"),
                          tuneGrid = data.frame(degree = 1:2),
                          B = 10)
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "bagEarthGCV", 
                       trControl = rctrl5,
                       tuneLength = 4)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "bagEarthGCV",
                            trControl = rctrl2,
                            preProc = c("center", "scale"),
                            tuneGrid = data.frame(degree = 1:2),
                            B = 10)

set.seed(849)
test_reg_oob_model <- train(trainX, trainY, 
                            method = "bagEarthGCV",
                            trControl = rctrl3,
                            preProc = c("center", "scale"),
                            tuneGrid = data.frame(degree = 1:2),
                            B = 10)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "bagEarthGCV", 
                             trControl = rctrl4,
                             tuneLength = 1,
                             preProc = c("center", "scale"),
                             B = 10)
test_reg_none_pred <- predict(test_reg_none_model, testX)

set.seed(849)
test_reg_rec <- train(x = rec_reg,
                      data = training,
                      method = "bagEarthGCV", 
                      trControl = rctrl1,
                      tuneGrid = data.frame(degree = 1:2),
                      B = 10)

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
test_3class_imp <- varImp(test_3class_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


