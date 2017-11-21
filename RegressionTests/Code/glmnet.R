timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "glmnet"



#########################################################################

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
testX <- testing[, -ncol(testing)]
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

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "glmnet", 
                             trControl = cctrl1,
                             metric = "ROC",
                             preProc = c("center", "scale"),
                             tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                    .lambda = c((1:5)/10)))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "glmnet", 
                            trControl = cctrl1,
                            metric = "ROC",
                            preProc = c("center", "scale"),
                            tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                   .lambda = c((1:5)/10)))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "glmnet", 
                         trControl = cctrlR,
                         tuneLength = 4)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "glmnet", 
                              trControl = cctrl2,
                              metric = "ROC",
                              preProc = c("center", "scale"),
                              tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                     .lambda = c((1:5)/10)))

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "glmnet", 
                               trControl = cctrl3,
                               metric = "ROC", 
                               preProc = c("center", "scale"),
                               tuneGrid = expand.grid(.alpha = c(.11),
                                                      .lambda = c(.5)))

test_class_none_pred <- predict(test_class_none_model, testX)

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "glmnet", 
                        trControl = cctrl1,
                        metric = "ROC",
                        tuneGrid = expand.grid(alpha = seq(.05, 1, length = 15),
                                               lambda = c((1:5)/10)))


if(
  !isTRUE(
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

cctrl4 <- trainControl(method = "cv", number = 3, classProbs = TRUE)
cctrl5 <- trainControl(method = "LOOCV", classProbs = TRUE)
cctrl6 <- trainControl(method = "none", classProbs = TRUE)

set.seed(849)
test_class_cv_3_model <- train(Species ~ ., data = iris, 
                               method = "glmnet", 
                               trControl = cctrl4,
                               preProc = c("center", "scale"),
                               tuneGrid = expand.grid(alpha = seq(.05, 1, length = 15),
                                                      lambda = c((1:5)/10)))

test_class_pred <- predict(test_class_cv_3_model, iris[, 1:4])

set.seed(849)
test_class_loo_3_model <- train(Species ~ ., data = iris[sample(1:150, 20),], 
                                method = "glmnet", 
                                trControl = cctrl5,
                                preProc = c("center", "scale"),
                                tuneGrid = expand.grid(alpha = seq(.05, 1, length = 15),
                                                       lambda = c((1:5)/10)))

set.seed(849)
test_class_none_3_model <- train(Species ~ ., data = iris, 
                                 method = "glmnet", 
                                 trControl = cctrl6,
                                 preProc = c("center", "scale"),
                                 tuneGrid = test_class_cv_3_model$bestTune)

test_class_none_pred <- predict(test_class_none_3_model, iris[, 1:4])

#########################################################################

library(glmnet)
## From ?cv.glmnet 
set.seed(1010)
n=1000;p=30
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
colnames(x) <- paste0("x", 1:ncol(x))
beta=rnorm(nzc)
fx= x[,seq(nzc)] %*% beta
eps=rnorm(n)*5
y=drop(fx+eps)
set.seed(1011)
cvob1=cv.glmnet(x,y)

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_reg_cv_model <- train(x, y, method = "glmnet",
                           preProc = c("center", "scale"),
                           trControl = rctrl1,
                           tuneGrid = expand.grid(alpha = c(.5, 1),
                                                 lambda = cvob1$lambda))
test_reg_pred <- predict(test_reg_cv_model, x)

set.seed(849)
test_reg_loo_model <- train(x, y, method = "glmnet",
                            preProc = c("center", "scale"),
                            trControl = rctrl2,
                            tuneGrid = expand.grid(alpha = c(.5, 1),
                                                  lambda = cvob1$lambda))

set.seed(849)
test_reg_none_model <- train(x, y, 
                             method = "glmnet", 
                             trControl = rctrl3,
                             tuneGrid = test_reg_cv_model$bestTune,
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, x)

#########################################################################

library(Matrix)

rctrl1$returnData <- FALSE
rctrl2$returnData <- FALSE
rctrl3$returnData <- FALSE

mat <- matrix(sample(0:1, 
                     size = p*nrow(x),
                     replace = TRUE),
              ncol = p)
colnames(mat) <- paste0("x", 1:ncol(x))
rownames(mat) <- paste0(1:nrow(x))
x2 <- Matrix(mat, sparse = TRUE)

cvob2=cv.glmnet(x2,y)

set.seed(849)
test_sparse_cv_model <- train(x2, y, method = "glmnet",
                              trControl = rctrl1,
                              tuneGrid = expand.grid(alpha = c(.5, 1),
                                                    lambda = cvob2$lambda[-(1:5)]))
test_sparse_pred <- predict(test_sparse_cv_model, x2)

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "glmnet", 
                       trControl = rctrlR,
                       tuneLength = 4)

set.seed(849)
test_sparse_loo_model <- train(x2, y, method = "glmnet",
                               trControl = rctrl2,
                               tuneGrid = expand.grid(alpha = c(.5, 1),
                                                     lambda = cvob2$lambda[-(1:5)]))

set.seed(849)
test_sparse_none_model <- train(x2, y, 
                                method = "glmnet", 
                                trControl = rctrl3,
                                tuneGrid = test_sparse_cv_model$bestTune)
test_sparse_none_pred <- predict(test_sparse_none_model, x2)

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


