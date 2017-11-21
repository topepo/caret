
library(caret)

context('Testing varImp')

test_that('glmnet varImp returns non-negative values', {
  skip_on_cran()
  skip_if_not_installed('glmnet')
  set.seed(1)
  dat <- SLC14_1(200)

  reg <- train(y ~ ., data = dat,
               method = "glmnet",
               tuneGrid = data.frame(lambda = .1, alpha = .5),
               trControl = trainControl(method = "none"))

  # this checks that some coefficients are negative
  coefs <- predict(reg$finalModel, s=0.1, type="coef")
  expect_lt(0, sum(0 > coefs))
  # now check that all elements of varImp are nonnegative,
  # in spite of negative coefficients
  vis <- varImp(reg, s=0.1, scale=F)$importance
  expect_true(all(vis >= 0))
})
