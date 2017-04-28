library(caret)
library(testthat)

test_that('train classification prediction', {
  skip_on_cran()
  set.seed(1)
  tr_dat <- twoClassSim(200)
  newdata1 <- twoClassSim(200)
  newdata2 <- twoClassSim(200)
  
  newdata1$additionalCol <- newdata1$Linear03
  newdata2 <- newdata2[, -1]
  
  object <- train(Class ~ ., data = tr_dat,
                        method = "rpart",
                        tuneGrid = data.frame(cp = 0.22),
                        preProc = c("center", "bagImpute"),
                        trControl = trainControl(method = "none", 
                                                 classProbs = TRUE,
                                                 trim = FALSE))
  expect_warning(predict(object, newdata1))
  expect_error(predict(object, newdata2))
})