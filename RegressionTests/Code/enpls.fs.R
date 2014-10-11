library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "enpls.fs"

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

rctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
rctrl2 <- trainControl(method = "LOOCV")
rctrl3 <- trainControl(method = "none")

set.seed(849)
test_reg_cv_model <- train(trainX, trainY, method = "enpls.fs", trControl = rctrl1,
                           preProc = c("center", "scale"))
test_reg_pred <- predict(test_reg_cv_model, testX)

set.seed(849)
test_reg_cv_form <- train(y ~ ., data = training, 
                          method = "enpls.fs", trControl = rctrl1,
                          tuneGrid = data.frame(maxcomp = 5, threshold = 1:2),
                          preProc = c("center", "scale"))
test_reg_pred_form <- predict(test_reg_cv_form, testX)

set.seed(849)
test_reg_loo_model <- train(trainX, trainY, method = "enpls.fs", 
                            trControl = rctrl2,
                            tuneGrid = data.frame(maxcomp = 5, threshold = 1:2),
                            preProc = c("center", "scale"))

set.seed(849)
test_reg_none_model <- train(trainX, trainY, 
                             method = "enpls.fs", 
                             trControl = rctrl3,
                             tuneGrid = data.frame(maxcomp = 5, threshold = 2),
                             preProc = c("center", "scale"))
test_reg_none_pred <- predict(test_reg_none_model, testX)

#########################################################################

test_reg_predictors1 <- predictors(test_reg_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")


