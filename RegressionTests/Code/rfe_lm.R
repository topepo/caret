library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "rfe_lm"

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

set.seed(2)
training <- SLC14_1(50)
testing <- SLC14_1(500)

trainX <- training[, -ncol(training)]
trainY <- training$y
testX <- trainX[, -ncol(training)]
testY <- trainX$y 

training$fact <- factor(sample(letters[1:3], size = nrow(training), replace = TRUE))
testing$fact <- factor(sample(letters[1:3], size = nrow(testing), replace = TRUE))

#########################################################################

rctrl1 <- rfeControl(method = "cv", number = 3, returnResamp = "all", functions = lmFuncs)
rctrl2 <- rfeControl(method = "LOOCV", functions = lmFuncs)

set.seed(849)
test_cv_model <- rfe(x = trainX, y = trainY,
                     sizes = c(1, 5, 10, 15),
                     rfeControl = rctrl1)

set.seed(849)
test_loo_model <- rfe(x = trainX, y = trainY,
                      sizes = c(1, 5, 10, 15),
                      rfeControl = rctrl2)

set.seed(849)
test_cv_model_form <- rfe(y ~ ., data = training,
                          sizes = c(1, 5, 10, 15),
                          rfeControl = rctrl1)

set.seed(849)
test_loo_model_form <- rfe(y ~ ., data = training,
                           sizes = c(1, 5, 10, 15),
                           rfeControl = rctrl2)

#########################################################################

test_cv_pred <- predict(test_cv_model, testX)
test_loo_pred <- predict(test_loo_model, testX)
# test_cv_pred_form <- predict(test_cv_model_form, testing[, colnames(testing) != "y"])
# test_loo_pred_form <- predict(test_loo_model_form, testing[, colnames(testing) != "y"])


#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")

