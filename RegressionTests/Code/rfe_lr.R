timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "rfe_lr"

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

cctrl1 <- rfeControl(method = "cv", number = 3, returnResamp = "all", functions = lrFuncs)
cctrl2 <- rfeControl(method = "LOOCV", functions = lrFuncs)

set.seed(849)
test_cv_model_class <- rfe(x = trainX_class, y = trainY_class,
                           sizes = c(1, 5, 10, 15),
                           rfeControl = cctrl1)

set.seed(849)
test_loo_model_class <- rfe(x = trainX_class, y = trainY_class,
                            sizes = c(1, 5, 10, 15),
                            rfeControl = cctrl2)

set.seed(849)
test_cv_model_form_class <- rfe(Class ~ ., data = training_class,
                                sizes = c(1, 5, 10, 15),
                                rfeControl = cctrl1)

set.seed(849)
test_loo_model_form_class <- rfe(Class ~ ., data = training_class,
                                 sizes = c(1, 5, 10, 15),
                                 rfeControl = cctrl2)

#########################################################################

test_cv_pred_class <- predict(test_cv_model_class, testX_class)
test_loo_pred_class <- predict(test_loo_model_class, testX_class)
# test_cv_pred_form_class <- predict(test_cv_model_form_class, testing_class[, colnames(testing_class) != "Class"])
# test_loo_pred_form_class <- predict(test_loo_model_form_class, testing_class[, colnames(testing_class) != "Class"])

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")

