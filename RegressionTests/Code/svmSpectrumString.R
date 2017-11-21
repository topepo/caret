timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "svmSpectrumString"


  

#########################################################################

library(kernlab)
data(reuters)
reuters <- matrix(reuters, ncol = 1)
colnames(reuters) <- "text"

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
cctrl2 <- trainControl(method = "LOOCV", savePredictions = TRUE)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_class_cv_model <- train(reuters, rlabels, 
                             method = "svmSpectrumString",
                             trControl = cctrl1)

test_class_pred <- predict(test_class_cv_model, reuters)

set.seed(849)
test_class_rand <- train(reuters, rlabels, 
                         method = "svmSpectrumString", 
                         trControl = cctrlR,
                         tuneLength = 4)

set.seed(849)
test_class_loo_model <- train(reuters, rlabels, 
                              method = "svmSpectrumString",
                              trControl = cctrl2)

set.seed(849)
test_class_none_model <- train(reuters, rlabels, 
                               method = "svmSpectrumString", 
                               trControl = cctrl3,
                               tuneGrid = test_class_cv_model$bestTune,
                               metric = "ROC")

test_class_none_pred <- predict(test_class_none_model, reuters)
test_class_none_prob <- predict(test_class_none_model, reuters, type = "prob")

test_levels <- levels(test_class_cv_model)
if(!all(levels(rlabels) %in% test_levels))
  cat("wrong levels")

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


