timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

modelZ <- "mxnetAdam"

## In case the package or one of its dependencies uses random numbers
## on startup so we'll pre-load the required libraries: 

for(i in getModelInfo(modelZ)[[1]]$library)
  do.call("requireNamespace", list(package = i))

#########################################################################
## Classification tests
set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
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

maGrid <- expand.grid( layer1= 3, layer2 = c(0,5), layer3 = 0, activation= 'relu', learningrate=1e-02, 
                       beta1=0.9, beta2=0.9999, dropout=c(0.05,0.20) )

set.seed(849)
test_class_cv_model <- train(trainX, trainY, method = modelZ, 
                             trControl = cctrl1, metric = "ROC", preProc = c("center", "scale"),  tuneGrid = maGrid)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob") 

set.seed(849)
test_class_rand <- train(trainX, trainY, method = modelZ, 
                         trControl = cctrlR, tuneLength = 4, preProc = c("center", "scale"))

set.seed(849)
test_class_loo_model <- train(trainX, trainY, method = modelZ, 
                              trControl = cctrl2, metric = "ROC", tuneGrid = maGrid, preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX, trainY, method = modelZ, trControl = cctrl3, 
                               tuneGrid = test_class_cv_model$bestTune, metric = "ROC", preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_rec <- train(x = rec_cls, data = training, method = modelZ,
                        trControl = cctrl1, metric = "ROC", tuneGrid = maGrid)

set.seed(849)
test_class_rec2 <- train(x = rec_cls, data = training, method = modelZ, num.round = 20,
                         trControl = cctrl1, metric = "ROC", tuneGrid = maGrid)

if(  !isTRUE(
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
## Regression tests
library(caret)
library(plyr)
library(recipes)
library(dplyr)
set.seed(1)
training <- SLC14_1(30)[, 18:21]
testing <- SLC14_1(100)[, 18:21]
trainX <- training[, -ncol(training)]
trainY <- training$y

rec_reg <- recipe(y ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) 

testX <- testing[, -ncol(training)]
testY <- testing$y 

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all", seeds = seeds)
rctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
rctrl3 <- trainControl(method = "none", seeds = seeds)

maGrid <- expand.grid( layer1= 13, layer2=5, layer3 =5, activation='tanh', learningrate=1e-02, 
                       beta1=0.9, beta2=0.9999, dropout=c(0.05,0.20) )

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, method = modelZ, trControl = rctrl1, tuneGrid = maGrid, 
                           preProc = c("center", "scale"), 
                           num.round= 33) # Try passing num.round on purpose
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, method = modelZ, trControl = rctrl1)
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, method = modelZ, trControl = rctrl2)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, method = modelZ, trControl = rctrl3,
                             tuneGrid = test_reg_cv_model$bestTune, preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, testX)

set.seed(849)
test_reg_rec <- train(x = rec_reg, data = training,method = modelZ, trControl = rctrl1, tuneGrid = maGrid,
                      num.round= 33) # Try passing num.round on purpose

if(
  !isTRUE(
    all.equal(test_reg_cv_model$results, test_reg_rec$results))
)
  stop("CV weights not giving the same results")

test_reg_imp_rec <- varImp(test_reg_rec)

test_reg_pred_rec <- predict(test_reg_rec, testing[, -ncol(testing)])

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")

##################