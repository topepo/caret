timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "trimming"

library(caret)
library(plyr)
library(recipes)
library(dplyr)
set.seed(1)
training <- SLC14_1(30)
testing <- SLC14_1(100)

set.seed(1)
cv_no_trimming <- train(y ~ ., 
                        data = training,
                        method = "lm",
                        preProc = c("center", "scale"),
                        trControl = trainControl(method = "cv", 
                                                 savePredictions = TRUE))

set.seed(1)
cv_bottom_only <- train(y ~ ., 
                        data = training,
                        method = "lm",
                        preProc = c("center", "scale"),
                        trControl = trainControl(method = "cv",
                                                 predictionBounds = c(0, NA), 
                                                 savePredictions = TRUE))

set.seed(1)
cv_boolean <- train(y ~ ., 
                    data = training,
                    method = "lm",
                    preProc = c("center", "scale"),
                    trControl = trainControl(method = "cv",
                                             predictionBounds = c(TRUE, TRUE), 
                                             savePredictions = TRUE))

set.seed(1)
cv_top_only <- train(y ~ ., 
                data = training,
                method = "lm",
                preProc = c("center", "scale"),
                trControl = trainControl(method = "cv",
                                         predictionBounds = c(NA, 25), 
                                         savePredictions = TRUE))

set.seed(1)
loo_no_trimming <- train(y ~ ., 
                         data = training,
                         method = "lm",
                         preProc = c("center", "scale"),
                         trControl = trainControl(method = "LOOCV", 
                                                  savePredictions = TRUE))

set.seed(1)
loo_bottom_only <- train(y ~ ., 
                         data = training,
                         method = "lm",
                         preProc = c("center", "scale"),
                         trControl = trainControl(method = "LOOCV",
                                                  predictionBounds = c(0, NA), 
                                                  savePredictions = TRUE))

set.seed(1)
loo_boolean <- train(y ~ ., 
                     data = training,
                     method = "lm",
                     preProc = c("center", "scale"),
                     trControl = trainControl(method = "LOOCV",
                                              predictionBounds = c(TRUE, TRUE), 
                                              savePredictions = TRUE))

set.seed(1)
loo_top_only <- train(y ~ ., 
                      data = training,
                      method = "lm",
                      preProc = c("center", "scale"),
                      trControl = trainControl(method = "LOOCV",
                                               predictionBounds = c(NA, 25), 
                                               savePredictions = TRUE))

test_external_cv_no_trimming  <- predict(cv_no_trimming, testing)
test_external_cv_bottom_only  <- predict(cv_bottom_only, testing)
test_external_cv_booleans     <- predict(cv_boolean, testing)
test_external_cv_top_only     <- predict(cv_top_only, testing)
test_external_loo_no_trimming <- predict(loo_no_trimming, testing)
test_external_loo_bottom_only <- predict(loo_bottom_only, testing)
test_external_loo_booleans    <- predict(loo_boolean, testing)
test_external_loo_top_only    <- predict(loo_top_only, testing)

test_internal_cv_no_trimming  <- cv_no_trimming$pred
test_internal_cv_bottom_only  <- cv_bottom_only$pred
test_internal_cv_booleans     <- cv_boolean$pred
test_internal_cv_top_only     <- cv_top_only$pred
test_internal_loo_no_trimming <- loo_no_trimming$pred
test_internal_loo_bottom_only <- loo_bottom_only$pred
test_internal_loo_booleans    <- loo_boolean$pred
test_internal_loo_top_only    <- loo_top_only$pred

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")

