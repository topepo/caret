library(caret)

model <- "trimming"

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

set.seed(1)
test1 <- train(y ~ ., 
               data = training,
               method = "lm",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "cv", 
                                        savePredictions = TRUE))

set.seed(1)
test2 <- train(y ~ ., 
               data = training,
               method = "lm",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "cv",
                                        predictionBounds = c(0, NA), 
                                        savePredictions = TRUE))

set.seed(1)
test3 <- train(y ~ ., 
               data = training,
               method = "lm",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "cv",
                                        predictionBounds = c(TRUE, TRUE), 
                                        savePredictions = TRUE))

set.seed(1)
test4 <- train(y ~ ., 
               data = training,
               method = "lm",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "cv", 
                                        savePredictions = TRUE))

set.seed(1)
test5 <- train(y ~ ., 
               data = training,
               method = "lm",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "cv",
                                        predictionBounds = c(0, NA), 
                                        savePredictions = TRUE))

set.seed(1)
test6 <- train(y ~ ., 
               data = training,
               method = "lm",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "cv",
                                        predictionBounds = c(TRUE, TRUE), 
                                        savePredictions = TRUE))

set.seed(1)
test7 <- train(y ~ ., 
               data = training,
               method = "lm",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "LOOCV", 
                                        savePredictions = TRUE))

set.seed(1)
test8 <- train(y ~ ., 
               data = training,
               method = "lm",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "LOOCV",
                                        predictionBounds = c(0, NA), 
                                        savePredictions = TRUE))

set.seed(1)
test9 <- train(y ~ ., 
               data = training,
               method = "lm",
               preProc = c("center", "scale"),
               trControl = trainControl(method = "LOOCV",
                                        predictionBounds = c(TRUE, TRUE), 
                                        savePredictions = TRUE))

set.seed(1)
test10 <- train(y ~ ., 
                data = training,
                method = "lm",
                preProc = c("center", "scale"),
                trControl = trainControl(method = "LOOCV", 
                                         savePredictions = TRUE))

set.seed(1)
test11 <- train(y ~ ., 
                data = training,
                method = "lm",
                preProc = c("center", "scale"),
                trControl = trainControl(method = "LOOCV",
                                         predictionBounds = c(0, NA), 
                                         savePredictions = TRUE))

set.seed(1)
test12 <- train(y ~ ., 
                data = training,
                method = "lm",
                preProc = c("center", "scale"),
                trControl = trainControl(method = "LOOCV",
                                         predictionBounds = c(TRUE, TRUE), 
                                         savePredictions = TRUE))

test_ext_pred <- data.frame(p1 = predict(test1, testing),
                            p2 = predict(test2, testing),
                            p3 = predict(test3, testing),
                            p4 = predict(test4, testing),
                            p5 = predict(test5, testing),
                            p6 = predict(test6, testing),
                            p7 = predict(test7, testing),
                            p8 = predict(test8, testing),
                            p9 = predict(test9, testing),
                            p10 = predict(test10, testing),
                            p11 = predict(test11, testing),
                            p12 = predict(test12, testing))
test_1_pred <- test1$pred
test_2_pred <- test2$pred
test_3_pred <- test3$pred
test_4_pred <- test4$pred
test_5_pred <- test5$pred
test_6_pred <- test6$pred
test_7_pred <- test7$pred
test_8_pred <- test8$pred
test_9_pred <- test9$pred
test_10_pred <- test10$pred
test_11_pred <- test11$pred
test_12_pred <- test12$pred

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")

