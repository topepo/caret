context("Test update.train")

library(caret)

test_that('update.train correctly updated bestTune parameter', {
  set.seed(1)
  data(iris)
  TrainData <- iris[,1:4]
  TrainClasses <- iris[,5]
  
  knnFit1 <- train(TrainData, TrainClasses,
                   method = "knn",
                   preProcess = c("center", "scale"),
                   tuneLength = 2,
                   trControl = trainControl(method = "cv"))
  expect_equal(knnFit1$bestTune$k, 7)
  
  knnFit2 <- update(knnFit1, list(.k = 3))
  expect_equal(knnFit2$bestTune$k, 3)
  
  knnFit3 <- update(knnFit1, forceRefit = TRUE)
  expect_equal(knnFit1$bestTune$k, knnFit3$bestTune$k)
})