library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "rknn"

#########################################################################

set.seed(2)
training <- twoClassSim(50, noiseVars = 2)
testing <- twoClassSim(500, noiseVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

cctrl1 <- trainControl(method = "cv", number = 3)
cctrl2 <- trainControl(method = "LOOCV")
cctrl3 <- trainControl(method = "none")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "rknnBel", 
                             trControl = cctrl1, 
                             tuneLength = 2,
                             preProc = c("center", "scale"), 
                             seed = 135)

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "rknnBel", 
                            trControl = cctrl1, 
                            tuneLength = 2,
                            preProc = c("center", "scale"), 
                            seed = 135)
test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "rknnBel", 
                              trControl = cctrl2, 
                              tuneLength = 2,
                              preProc = c("center", "scale"), 
                              seed = 135)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "rknnBel", 
                               trControl = cctrl3,
                               tuneGrid = test_class_cv_model$bestTune, 
                               preProc = c("center", "scale"), 
                               seed = 135)

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

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

grid <- expand.grid(k = c(5, 10), d = c(1, 2), mtry = 2)

set.seed(1)
training <- SLC14_1(50)
testing <- SLC14_1(100)
trainX <- training[, -ncol(training)]
trainY <- training$y
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "rknnBel", 
                           trControl = rctrl1, 
                           tuneGrid = grid,
                           preProc = c("center", "scale"), 
                           seed = 135)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "rknnBel", 
                          trControl = rctrl1, 
                          tuneGrid = grid,
                          preProc = c("center", "scale"), 
                          seed = 135)
test_reg_pred_form <- predict(test_reg_cv_form, testX)


set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "rknnBel",
                            trControl = rctrl2, 
                            tuneGrid = grid,
                            preProc = c("center", "scale"), 
                            seed = 135)

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "rknnBel", 
                             trControl = rctrl3,
                             tuneGrid = test_reg_cv_model$bestTune,
                             preProc = c("center", "scale"), 
                             seed = 135)
test_reg_none_pred <- predict(test_reg_none_model, testX)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

# q("no")


