timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "brnn"



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

rec_reg <- recipe(y ~ ., data = training[, c(1:3, ncol(training))]) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all", seeds = seeds)
rctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
rctrl3 <- trainControl(method = "none", seeds = seeds)
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_reg_cv_model <- train(trainX[, 1:3], trainY, method = "brnn", 
                           tuneLength = 2,
                           trControl = rctrl1, verbose =FALSE)
test_reg_pred <- predict(test_reg_cv_model, testX[, 1:3])

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training[, c(1:3, ncol(training))], 
                          method = "brnn", 
                          tuneLength = 2,
                          trControl = rctrl1, verbose =FALSE)
test_reg_pred_form <- predict(test_reg_cv_form, testX[, 1:3])

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "brnn", 
                       trControl = rctrlR,
                       tuneLength = 4)

set.seed(849)
test_reg_loo_model <- train(trainX[, 1:3], trainY, method = "brnn", 
                            tuneLength = 2, trControl = rctrl2, 
                            verbose =FALSE)

set.seed(849)
test_reg_none_model <- train(trainX[, 1:3], trainY, 
                             method = "brnn", 
                             trControl = rctrl3,
                             tuneGrid = test_reg_cv_model$bestTune,
                             preProc = c("center", "scale"), 
                             verbose =FALSE)
test_reg_none_pred <- predict(test_reg_none_model, testX[, 1:3])

set.seed(849)
test_reg_rec <- train(x = rec_reg,
                      data = training,
                      method = "brnn", 
                      trControl = rctrl1, 
                      tuneLength = 2,
                      verbose =FALSE)

if(
  !isTRUE(
    all.equal(test_reg_cv_model$results, 
              test_reg_rec$results,
              tolerance = .001))
)
  stop("CV weights not giving the same results")


test_reg_pred_rec <- predict(test_reg_rec, testing[, -ncol(testing)])

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


