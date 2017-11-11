timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "sbf_rf"

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

rctrl1 <- sbfControl(method = "cv", number = 3, returnResamp = "all", functions = rfSBF)
rctrl2 <- sbfControl(method = "LOOCV", functions = rfSBF)

set.seed(849)
test_cv_model <- sbf(x = trainX, y = trainY,
                     ntree = 20,
                     sbfControl = rctrl1)

set.seed(849)
test_loo_model <- sbf(x = trainX, y = trainY,
                      ntree = 20,
                      sbfControl = rctrl2)

set.seed(849)
test_cv_model_form <- sbf(y ~ ., data = training,
                          ntree = 20,
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

set.seed(2)
training_class <- twoClassSim(50)
testing_class <- twoClassSim(500)

trainX_class <- training_class[, -ncol(training_class)]
trainY_class <- training_class$Class
testX_class <- testing_class[, -ncol(testing_class)]
testY_class <- testing_class$Class

training_class$fact <- factor(sample(letters[1:3], size = nrow(training_class), replace = TRUE))
testing_class$fact <- factor(sample(letters[1:3], size = nrow(testing_class), replace = TRUE))

#########################################################################

cctrl1 <- sbfControl(method = "cv", number = 3, returnResamp = "all", functions = rfSBF)
cctrl2 <- sbfControl(method = "LOOCV", functions = rfSBF)

set.seed(849)
test_cv_model_class <- sbf(x = trainX_class, y = trainY_class,
                           ntree = 20,
                           sbfControl = cctrl1)

set.seed(849)
test_loo_model_class <- sbf(x = trainX_class, y = trainY_class,
                            ntree = 20,
                            sbfControl = cctrl2)

set.seed(849)
test_cv_model_form_class <- sbf(Class ~ ., data = training_class,
                                ntree = 20,
                                sbfControl = cctrl1)

set.seed(849)
test_loo_model_form_class <- sbf(Class ~ ., data = training_class,
                                 ntree = 20,
                                 sbfControl = cctrl2)

#########################################################################

test_cv_pred_class <- predict(test_cv_model_class, testX_class)
test_loo_pred_class <- predict(test_loo_model_class, testX_class)
test_cv_pred_form_class <- predict(test_cv_model_form_class, testing_class[, colnames(testing_class) != "Class"])
test_loo_pred_form_class <- predict(test_loo_model_form_class, testing_class[, colnames(testing_class) != "Class"])


#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")

