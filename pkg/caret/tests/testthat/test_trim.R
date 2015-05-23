context('Base model trimming works')

library(rpart)
library(ipred)
library(plyr)

###################################################################
## rpart tests

test_that("trimmed rpart regression produces identical predicted values", {
  skip_on_cran()
  set.seed(1)
  train_dat <- SLC14_1(100)
  train_dat$factor_var <- factor(sample(letters[1:2], nrow(train_dat), replace = TRUE))
  test_dat <- SLC14_1(1000)
  test_dat$factor_var <- factor(sample(letters[1:2], nrow(test_dat), replace = TRUE))

  library(rpart)
  rpart_full <- rpart(y ~ ., data = train_dat)
  rpart_trim <- getModelInfo('rpart', regex=FALSE)[[1]]$trim(rpart_full)
  expect_identical(predict(rpart_full, test_dat), predict(rpart_trim, test_dat))
  expect_less_than(object.size(rpart_trim), object.size(rpart_full))
})

test_that("trimmed rpart classification produces identical predicted values", {
  skip_on_cran()
  set.seed(1)
  train_dat <- twoClassSim(100)
  train_dat$factor_var <- factor(sample(letters[1:2], nrow(train_dat), replace = TRUE))
  test_dat <- twoClassSim(1000)
  test_dat$factor_var <- factor(sample(letters[1:2], nrow(test_dat), replace = TRUE))

  library(rpart)
  rpart_full <- rpart(Class ~ ., data = train_dat)
  rpart_trim <- getModelInfo('rpart', regex=FALSE)[[1]]$trim(rpart_full)
  expect_identical(predict(rpart_full, test_dat)[, "Class1"], predict(rpart_trim, test_dat)[, "Class1"])
  expect_less_than(object.size(rpart_trim), object.size(rpart_full))
})

###################################################################
## bagging tests

test_that("trimmed bagging regression produces identical predicted values", {
  skip_on_cran()
  set.seed(1)
  train_dat <- SLC14_1(100)
  train_dat$factor_var <- factor(sample(letters[1:2], nrow(train_dat), replace = TRUE))
  test_dat <- SLC14_1(1000)
  test_dat$factor_var <- factor(sample(letters[1:2], nrow(test_dat), replace = TRUE))

  library(rpart)
  bag_full <- bagging(y ~ ., data = train_dat)
  bag_trim <- getModelInfo('treebag', regex=FALSE)[[1]]$trim(bag_full)
  expect_identical(predict(bag_full, test_dat), predict(bag_trim, test_dat))
  expect_less_than(object.size(bag_trim), object.size(bag_full))
})
