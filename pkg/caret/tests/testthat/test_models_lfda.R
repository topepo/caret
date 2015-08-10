library(caret)
library(testthat)

test_that('test lfda model training and prediction', {
  skip_on_cran()
  set.seed(1)
  tr_dat <- twoClassSim(200)
  te_dat <- twoClassSim(200)
  
  lfda.model <- lfda(x=tr_dat[,-16],y=tr_dat[,16],r=3)
  
  transform.metric <- lfda.model$T
  transformed.train <- lfda.model$Z

  transformed.test <- predict(lfda.model, newdata=te_dat[,-16])
  
  # check dimensions
  expect_that(dim(transformed.train)[2], equals(3))
  expect_that(dim(transformed.test), equals(dim(transformed.train)))
  expect_that(dim(transform.metric)[2], equals(dim(transformed.train)[2]))
  expect_that(dim(tr_dat)[1], equals(dim(transformed.train)[1]))
})

test_that('test lfda model visualization function', {
  skip_on_cran()
  set.seed(1)
  tr_dat <- twoClassSim(200)

  lfda.model <- lfda(x=tr_dat[,-16],y=tr_dat[,16],r=3)
  
  expect_that(plot.lfda(lfda.model, tr_dat$Class), not(throws_error()))
})

