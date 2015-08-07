library(caret)
library(testthat)

test_that('train classification', {
  skip_on_cran()
  set.seed(1)
  tr_dat <- twoClassSim(200)
  newdata <- twoClassSim(200)
  
  newdata$additionalCol <- newdata$Linear03
  
  object <- train(Class ~ ., data = tr_dat,
                        method = "rpart",
                        tuneGrid = data.frame(cp = 0.22),
                        preProc = c("center", "bagImpute"),
                        trControl = trainControl(method = "none", 
                                                 classProbs = TRUE,
                                                 trim = FALSE))
  expect_warning(predict(object, newdata))
  
})