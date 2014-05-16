library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "glmnet"

#########################################################################

set.seed(545)

data(mdrr)
mdrrDescr <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .8)]

inTrain <- createDataPartition(mdrrClass)
trainX <- mdrrDescr[inTrain[[1]], ]
trainY <- mdrrClass[inTrain[[1]]]
testX <- mdrrDescr[-inTrain[[1]], ]
testY <- mdrrClass[-inTrain[[1]]]

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, summaryFunction = twoClassSummary)
set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "glmnet", 
                             trControl = cctrl1,
                             metric = "ROC",
                             preProc = c("center", "scale"),
                             tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                    .lambda = c((1:5)/10)))

test_class_pred <- predict(test_class_cv_model, testX)

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
                               tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                      .lambda = c((1:5)/10)))

test_class_pred <- predict(test_class_cv_3_model, iris[, 1:4])

set.seed(849)
test_class_loo_3_model <- train(Species ~ ., data = iris[sample(1:150, 20),], 
                                method = "glmnet", 
                                trControl = cctrl5,
                                preProc = c("center", "scale"),
                                tuneGrid = expand.grid(.alpha = seq(.05, 1, length = 15),
                                                       .lambda = c((1:5)/10)))

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
n=1000;p=100
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
beta=rnorm(nzc)
fx= x[,seq(nzc)] %*% beta
eps=rnorm(n)*5
y=drop(fx+eps)
set.seed(1011)
cvob1=cv.glmnet(x,y)

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")

set.seed(849)
test_reg_cv_model <- train(x, y, method = "glmnet",
                           preProc = c("center", "scale"),
                           trControl = rctrl1,
                           tuneGrid = data.frame(.alpha = c(.5, 1),
                                                 .lambda = cvob1$lambda))
test_reg_pred <- predict(test_reg_cv_model, x)

set.seed(849)
test_reg_loo_model <- train(x, y, method = "glmnet",
                            preProc = c("center", "scale"),
                            trControl = rctrl2,
                            tuneGrid = data.frame(.alpha = c(.5, 1),
                                                  .lambda = cvob1$lambda))

set.seed(849)
test_reg_none_model <- train(x, y, 
                             method = "glmnet", 
                             trControl = rctrl3,
                             tuneGrid = test_reg_cv_model$bestTune,
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, x)

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


