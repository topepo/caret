timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "subsampling"

#########################################################################

set.seed(1)
training <- twoClassSim(500, linearVars = 2, intercept = -15)
testing <- twoClassSim(500, linearVars = 2, intercept = -15)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

down_ctrl <- trainControl(method = "cv", number = 3, returnResamp = "all",
                          classProbs = TRUE, 
                          sampling = "down",
                          summaryFunction = twoClassSummary)
up_ctrl <- down_ctrl
up_ctrl$sampling <- "up"
smote_ctrl <- down_ctrl
smote_ctrl$sampling <- "smote"
rose_ctrl <- down_ctrl
rose_ctrl$sampling <- "rose"

set.seed(849)
prior_down <- train(trainX, trainY, 
                   method = "lda", 
                   trControl = down_ctrl,
                   metric = "ROC", 
                   preProc = c("center", "scale"))

set.seed(849)
prior_up <- train(trainX, trainY, 
                 method = "lda", 
                 trControl = up_ctrl,
                 metric = "ROC", 
                 preProc = c("center", "scale"))

set.seed(849)
prior_smote <- train(trainX, trainY, 
                    method = "lda", 
                    trControl = smote_ctrl,
                    metric = "ROC", 
                    preProc = c("center", "scale"))

set.seed(849)
prior_rose <- train(trainX, trainY, 
                   method = "lda", 
                   trControl = rose_ctrl,
                   metric = "ROC", 
                   preProc = c("center", "scale"))


down_ctrl$sampling <- list(name = "down", func = getSamplingInfo("down")[[1]], first = FALSE)
up_ctrl$sampling <- list(name = "up", func = getSamplingInfo("up")[[1]], first = FALSE)
smote_ctrl$sampling <- list(name = "smote", func = getSamplingInfo("smote")[[1]], first = FALSE)
rose_ctrl$sampling <- list(name = "rose", func = getSamplingInfo("rose")[[1]], first = FALSE)

set.seed(849)
post_down <- train(trainX, trainY, 
                    method = "lda", 
                    trControl = down_ctrl,
                    metric = "ROC", 
                    preProc = c("center", "scale"))

set.seed(849)
post_up <- train(trainX, trainY, 
                  method = "lda", 
                  trControl = up_ctrl,
                  metric = "ROC", 
                  preProc = c("center", "scale"))

set.seed(849)
post_smote <- train(trainX, trainY, 
                     method = "lda", 
                     trControl = smote_ctrl,
                     metric = "ROC", 
                     preProc = c("center", "scale"))

set.seed(849)
post_rose <- train(trainX, trainY, 
                    method = "lda", 
                    trControl = rose_ctrl,
                    metric = "ROC", 
                    preProc = c("center", "scale"))




#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


