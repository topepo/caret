timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "rfe_gam"

#########################################################################

library(caret)
library(plyr)
library(recipes)
library(dplyr)
set.seed(2)
training <- SLC14_1(275)
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

rctrl1 <- rfeControl(method = "cv", number = 3, returnResamp = "all", functions = gamFuncs)
rctrl2 <- rfeControl(method = "LOOCV", functions = gamFuncs)

set.seed(849)
test_cv_model <- rfe(x = trainX, y = trainY,
                     sizes = 1:3,
                     rfeControl = rctrl1)

set.seed(849)
test_loo_model <- rfe(x = trainX, y = trainY,
                      sizes = 1:3,
                      rfeControl = rctrl2)

set.seed(849)
test_cv_model_form <- rfe(y ~ ., data = training,
                          sizes = 1:3,
                          rfeControl = rctrl1)

# set.seed(849)
# test_loo_model_form <- rfe(y ~ ., data = training,
#                            sizes = 1:3,
#                            rfeControl = rctrl2)

#########################################################################

test_cv_pred <- predict(test_cv_model, testX)
test_loo_pred <- predict(test_loo_model, testX)
# test_cv_pred_form <- predict(test_cv_model_form, testX)
# test_loo_pred_form <- predict(test_loo_model_form, testX)

#########################################################################

set.seed(2)
training_class <- twoClassSim(250)
testing_class <- twoClassSim(500)

trainX_class <- training_class[, -ncol(training_class)]
trainY_class <- training_class$Class
testX_class <- testing_class[, -ncol(testing_class)]
testY_class <- testing_class$Class

training_class$fact <- factor(sample(letters[1:3], size = nrow(training_class), replace = TRUE))
testing_class$fact <- factor(sample(letters[1:3], size = nrow(testing_class), replace = TRUE))
testX_class$fact <- testing_class$fact

#########################################################################

cctrl1 <- rfeControl(method = "cv", number = 3, returnResamp = "all", functions = gamFuncs)
cctrl2 <- rfeControl(method = "LOOCV", functions = gamFuncs)

set.seed(849)
test_cv_model_class <- rfe(x = trainX_class, y = trainY_class,
                           sizes = 1:3,
                           rfeControl = cctrl1)

set.seed(849)
test_loo_model_class <- rfe(x = trainX_class, y = trainY_class,
                            sizes = 1:3,
                            rfeControl = cctrl2)

set.seed(849)
test_cv_model_form_class <- rfe(Class ~ ., data = training_class,
                                sizes = 1:3,
                                rfeControl = cctrl1)

# set.seed(849)
# test_loo_model_form_class <- rfe(Class ~ ., data = training_class,
#                                  sizes = c(1, 5, 10, 15),
#                                  rfeControl = cctrl2)

#########################################################################

test_cv_pred_class <- predict(test_cv_model_class, testX_class)
test_loo_pred_class <- predict(test_loo_model_class, testX_class)
test_cv_pred_form_class <- predict(test_cv_model_form_class, testX_class)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")

