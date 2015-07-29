library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "bag"

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
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary, 
                       seeds = seeds)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary,
                       seeds = seeds)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")


set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "bag", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                             bagControl = bagControl(fit = ldaBag$fit,
                                                     predict = ldaBag$pred,
                                                     aggregate = ldaBag$aggregate))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                             method = "bag", 
                             trControl = cctrl1,
                             metric = "ROC", 
                             tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                             bagControl = bagControl(fit = ldaBag$fit,
                                                     predict = ldaBag$pred,
                                                     aggregate = ldaBag$aggregate))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "bag", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         preProc = c("center", "scale"),
                         bagControl = bagControl(fit = ldaBag$fit,
                                                 predict = ldaBag$pred,
                                                 aggregate = ldaBag$aggregate))

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "bag", 
                              trControl = cctrl2,
                              metric = "ROC", 
                              tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                              bagControl = bagControl(fit = ldaBag$fit,
                                                      predict = ldaBag$pred,
                                                      aggregate = ldaBag$aggregate),
                              preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "bag", 
                               trControl = cctrl3,
                               metric = "ROC", 
                               bagControl = bagControl(fit = ldaBag$fit,
                                                       predict = ldaBag$pred,
                                                       aggregate = ldaBag$aggregate),
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)

#########################################################################

test_class_imp <- varImp(test_class_cv_model)

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

rctrl1 <- trainControl(method = "cv", number = 3, 
                       returnResamp = "all",
                       seed = seeds)
rctrl2 <- trainControl(method = "LOOCV", seed = seeds)
rctrl3 <- trainControl(method = "none", seed = seeds)
rctrl4 <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")


set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "bag", 
                           trControl = rctrl1,
                           tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                           bagControl = bagControl(fit = ctreeBag$fit,
                                                   predict = ctreeBag$pred,
                                                   aggregate = ctreeBag$aggregate))

test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                           method = "bag", 
                           trControl = rctrl1,
                           tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                           bagControl = bagControl(fit = ctreeBag$fit,
                                                   predict = ctreeBag$pred,
                                                   aggregate = ctreeBag$aggregate))

test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "bag", 
                       trControl = rctrl4,
                       tuneLength = 4,
                       bagControl = bagControl(fit = ctreeBag$fit,
                                               predict = ctreeBag$pred,
                                               aggregate = ctreeBag$aggregate))

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "bag", 
                            trControl = rctrl2,
                            tuneGrid = data.frame(vars = seq(1, 15, by = 2)),
                            bagControl = bagControl(fit = ctreeBag$fit,
                                                    predict = ctreeBag$pred,
                                                    aggregate = ctreeBag$aggregate))


set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "bag", 
                             trControl = rctrl3,
                             bagControl = bagControl(fit = ctreeBag$fit,
                                                     predict = ctreeBag$pred,
                                                     aggregate = ctreeBag$aggregate))

test_reg_none_pred <- predict(test_reg_none_model, testX)

#########################################################################

test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

sInfo <- sessionInfo()

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


