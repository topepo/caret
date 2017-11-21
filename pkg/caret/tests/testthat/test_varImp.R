context('varImp')

test_that("high level tests", {

  data(iris)
  TrainData <- iris[,1:4]
  TrainClasses <- iris[,5]

  expect_silent(
    knnFit1 <- train(TrainData, TrainClasses,
                   method = "knn",
                   preProcess = c("center", "scale"),
                   tuneLength = 10,
                   trControl = trainControl(method = "cv"))
  )

  expect_silent(vv <- varImp(knnFit1))

  expect_true(ncol(vv$importance) == length(levels(TrainClasses)))
  expect_true(nrow(vv$importance) == ncol(TrainData))

})



