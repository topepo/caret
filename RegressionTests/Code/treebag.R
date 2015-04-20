library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "treebag"

#########################################################################

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary, 
                       seeds = seeds)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary, 
                       seeds = seeds)
cctrl3 <- trainControl(method = "oob")
cctrl4 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary, 
                       seeds = seeds)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "treebag", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             nbagg = 7)

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "treebag", 
                            trControl = cctrl1,
                            metric = "ROC", 
                            preProc = c("center", "scale"),
                            nbagg = 7)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "treebag", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              preProc = c("center", "scale"),
                              nbagg = 7)
test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

set.seed(849)
test_class_oob_model <- train(trainX, trainY, 
                              method = "treebag", 
                              trControl = cctrl3,
                              nbagg = 7,
                              keepX = TRUE)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "treebag", 
                               trControl = cctrl4,
                               tuneGrid = test_class_cv_model$bestTune,
                               metric = "ROC", 
                               preProc = c("center", "scale"),
                               nbagg = 7,
                               keepX = TRUE)

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")


#########################################################################

SLC14_1 <- function(n = 100) {
  dat <- matrix(rnorm(n*20, sd = 3), ncol = 20)
  foo <- function(x) x[1] + sin(x[2]) + log(abs(x[3])) + x[4]^2 + x[5]*x[6] + 
    ifelse(x[7]*x[8]*x[9] < 0, 1, 0) +
    ifelse(x[10] > 0, 1, 0) + x[11]*ifelse(x[11] > 0, 1, 0) + 
    sqrt(abs(x[12])) + cos(x[13]) + 2*x[14] + abs(x[15]) + 
    ifelse(x[16] < -1, 1, 0) + x[17]*ifelse(x[17] < -1, 1, 0) -
    2 * x[18] - x[19]*x[20]
  dat <- as.data.frame(dat)
  colnames(dat) <- paste0("Var", 1:ncol(dat))
  dat$y <- apply(dat[, 1:20], 1, foo) + rnorm(n, sd = 3)
  dat
}

set.seed(1)
training <- SLC14_1(30)
testing <- SLC14_1(100)
trainX <- training[, -ncol(training)]
trainY <- training$y
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all", seeds = seeds)
rctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
rctrl3 <- trainControl(method = "oob", seeds = seeds)
rctrl4 <- trainControl(method = "none", seeds = seeds)

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "treebag", 
                           trControl = rctrl1,
                           preProc = c("center", "scale"),
                           nbagg = 7)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "treebag", 
                          trControl = rctrl1,
                          preProc = c("center", "scale"),
                          nbagg = 7)
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "treebag",
                            trControl = rctrl2,
                            preProc = c("center", "scale"),
                            nbagg = 7)

set.seed(849)
test_reg_oob_model <- train(trainX, trainY, 
                            method = "treebag",
                            trControl = rctrl3,
                            preProc = c("center", "scale"),
                            nbagg = 7,
                            keepX = TRUE)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "treebag", 
                             trControl = rctrl4,
                             tuneGrid = test_reg_cv_model$bestTune,
                             preProc = c("center", "scale"),
                             nbagg = 7,
                             keepX = TRUE)
test_reg_none_pred <- predict(test_reg_none_model, testX)

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_reg_predictors1 <- predictors(test_reg_cv_model)

test_class_predictors2 <- predictors(test_class_cv_model$finalModel)
test_reg_predictors2 <- predictors(test_reg_cv_model$finalModel)

#########################################################################

test_class_imp <- varImp(test_class_cv_model)
test_reg_imp <- varImp(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


