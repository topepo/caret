timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "knnImpute"

## test for a single missing value in a numeric predictor
iris_miss_1 <- iris[, -5]
iris_miss_1[1,1] <- NA
pp_1_test = preProcess(iris_miss_1, method = "knnImpute")
set.seed(1)
test_1_result <- predict(pp_1_test, iris_miss_1)

## test for a single missing values in all numeric predictors
iris_miss_2 <- iris[, -5]
iris_miss_2[1,1] <- NA
iris_miss_2[2,2] <- NA
iris_miss_2[3,3] <- NA
iris_miss_2[4,4] <- NA
pp_2_test = preProcess(iris_miss_2, method = "knnImpute")
set.seed(1)
test_2_result <- predict(pp_2_test, iris_miss_2)

## test for a single missing values in categorical predictor
iris_miss_3 <- iris
iris_miss_3[1,5] <- NA

set.seed(1)
test_3_result <- train(Sepal.Length ~ . , data = iris_miss_3, 
                       method = "lm", 
                       preProc = "knnImpute",
                       na.action = na.pass)


## test for a single missing values in multiple numeric predictors
iris_miss_4 <- iris[, -5]
iris_miss_4[1, 1:2] <- NA
pp_4_test = preProcess(iris_miss_4, method = "knnImpute")
set.seed(1)
test_4_result <- predict(pp_4_test, iris_miss_4)

## test for a single missing values in all numeric predictors
iris_miss_5 <- iris[, -5]
iris_miss_5[1,] <- NA
pp_5_test = preProcess(iris_miss_5, method = "knnImpute")
set.seed(1)
test_5_result <- try(predict(pp_5_test, iris_miss_5), silent = TRUE)


## each row has at least 1 missing value
iris_miss_6 <- iris[, -5]
index <- 0
for(i in 1:nrow(iris_miss_6)) {
  index <- if(index < ncol(iris_miss_6)) index + 1 else 1
  iris_miss_6[i, index] <- NA
}
pp_6_test = preProcess(iris_miss_6, method = "knnImpute")
set.seed(1)
test_6_result <- try(predict(pp_6_test, iris_miss_6), silent = TRUE)


tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))
