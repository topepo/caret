library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "logicBag"

#########################################################################
gobinary <- function(x){
  out <- apply(x, 2, function(x) ifelse(x > mean(x), 1, 0))
  colnames(out) <- colnames(x)
  out
} 

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
training[, -ncol(training)] <- gobinary(training[, -ncol(training)])
testing[, -ncol(testing)] <- gobinary(testing[, -ncol(testing)])
trainX <- training[, -ncol(training)]
trainY <- training$Class
testX <- testing[, -ncol(testing)]
testY <- testing$Class

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all", seeds = seeds)
cctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
cctrl3 <- trainControl(method = "none", seeds = seeds)
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "logicBag", 
                             trControl = cctrl1,
                             tuneGrid = expand.grid(ntrees = 2:3,
                                                    nleaves = 2^(4:5)),
                             B = 3,
                             seed = 1)

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "logicBag", 
                            trControl = cctrl1,
                            tuneGrid = expand.grid(ntrees = 2:3,
                                                   nleaves = 2^(4:5)),
                            B = 3,
                            seed = 1)

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_rand <- train(trainX, trainY, 
                         method = "logicBag", 
                         trControl = cctrlR,
                         tuneLength = 4,
                         B = 3,
                         seed = 1)

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "logicBag", 
                              trControl = cctrl2,
                              tuneGrid = expand.grid(ntrees = 2:3,
                                                     nleaves = 2^(4:5)),
                              B = 3,
                              seed = 1)

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "logicBag", 
                               trControl = cctrl3,
                               tuneGrid = test_class_cv_model$bestTune,
                               B = 3,
                               seed = 1)

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

set.seed(1)
training <- SLC14_1(300)
testing <- SLC14_1(100)
training[, -ncol(training)] <- gobinary(training[, -ncol(training)])
testing[, -ncol(testing)] <- gobinary(testing[, -ncol(testing)])
trainX <- training[, -ncol(training)]
trainY <- training$y
testX <- testing[, -ncol(testing)]
testY <- testing$y

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all", seeds = seeds)
rctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
rctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, 
                           method = "logicBag", 
                           trControl = rctrl1,
                           tuneGrid = expand.grid(ntrees = 2:3,
                                                  nleaves = 2^(4:5)),
                           B = 3,
                           seed = 1)
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "logicBag", 
                          trControl = rctrl1,
                          tuneGrid = expand.grid(ntrees = 2:3,
                                                 nleaves = 2^(4:5)),
                          B = 3,
                          seed = 1)
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_rand <- train(trainX, trainY, 
                       method = "logicBag", 
                       trControl = rctrlR,
                       tuneLength = 4,
                       B = 3,
                       seed = 1)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, 
                            method = "logicBag",
                            trControl = rctrl2,
                            tuneGrid = expand.grid(ntrees = 2:3,
                                                   nleaves = 2^(4:5)),
                            B = 3,
                            seed = 1)

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)
test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


