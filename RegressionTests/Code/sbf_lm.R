timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "sbf_lm"

#########################################################################

library(caret)
library(plyr)
library(recipes)
library(dplyr)
set.seed(2)
training <- SLC14_1(50)
testing <- SLC14_1(500)

trainX <- training[, -ncol(training)]
trainY <- training$y

rec_reg <- recipe(y ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

training$fact <- factor(sample(letters[1:3], size = nrow(training), replace = TRUE))
testing$fact <- factor(sample(letters[1:3], size = nrow(testing), replace = TRUE))

#########################################################################

rctrl1 <- sbfControl(method = "cv", number = 3, returnResamp = "all", functions = lmSBF)
rctrl2 <- sbfControl(method = "LOOCV", functions = lmSBF)

set.seed(849)
test_cv_model <- sbf(x = trainX, y = trainY,
                     sbfControl = rctrl1)

set.seed(849)
test_loo_model <- sbf(x = trainX, y = trainY,
                      sbfControl = rctrl2)

set.seed(849)
test_cv_model_form <- sbf(y ~ ., data = training,
                          sbfControl = rctrl1)

set.seed(849)
test_loo_model_form <- sbf(y ~ ., data = training,
                           sbfControl = rctrl2)

#########################################################################

test_cv_pred <- predict(test_cv_model, testX)
test_loo_pred <- predict(test_loo_model, testX)
# test_cv_pred_form <- predict(test_cv_model_form, testing[, colnames(testing) != "y"])
# test_loo_pred_form <- predict(test_loo_model_form, testing[, colnames(testing) != "y"])

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")

