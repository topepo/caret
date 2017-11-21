library(caret)

test_that('test train control progressCallback', {
  foo <- function(train_dat){
    trControl = trainControl(progressCallback = 1)
  }
  expect_error(foo())
})

test_that('test train control showProgress=TRUE', {
  foo <- function(){
    ctrl <- caret::trainControl(method = "repeatedcv", showProgress=TRUE)
    set.seed(1337)
    model <- caret::train(Species ~ ., data = iris, method = "rf", trControl = ctrl)
  }
  expect_output(foo(), ".*100%$")
})

test_that('test train control showProgress=FALSE', {
  foo <- function(){
    ctrl <- caret::trainControl(method = "repeatedcv", showProgress=FALSE)
    set.seed(1337)
    model <- caret::train(Species ~ ., data = iris, method = "rf", trControl = ctrl)
  }
  expect_silent(foo())
})
