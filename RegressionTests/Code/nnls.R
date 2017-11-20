timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "nnls"



#########################################################################


set.seed(251)
X <- matrix(rnorm(300*5), ncol = 5)
colnames(X) <- paste0("Var", 1:ncol(X)) 
y <- .2*X[,1] + .5*X[,2] + rnorm(nrow(X))

training <- as.data.frame(X[1:200,])
training$y <- y[1:200]
testing <- as.data.frame(X[201:300,])
testing$y <- y[201:300]
trainX <- X[1:200,]
trainY <- y[1:200]
testX <- X[201:300,]
testY <- y[201:300]

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")



set.seed(849)
test_reg_cv_model <- train(trainX, trainY, method = "nnls", trControl = rctrl1)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, method = "nnls", trControl = rctrl1)
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, method = "nnls", trControl = rctrl2)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "nnls", 
                             trControl = rctrl3,
                             tuneLength = 1,
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, testX)

#########################################################################

test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

test_reg_imp <- varImp(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


