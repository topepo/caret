context('Test model object trimming')

library(rpart)
library(ipred)

###################################################################
## rpart tests functions

check_rpart_reg <- function() {
  skip_on_cran()
  set.seed(1)
  train_dat <- SLC14_1(100)
  train_dat$factor_var <- factor(sample(letters[1:2], nrow(train_dat), replace = TRUE))
  test_dat <- SLC14_1(1000)
  test_dat$factor_var <- factor(sample(letters[1:2], nrow(test_dat), replace = TRUE))
  
  library(rpart)
  rpart_full <- rpart(y ~ ., data = train_dat)
  rpart_trim <- caret:::trim(rpart_full)
  predict(rpart_full, test_dat) - predict(rpart_trim, test_dat)
}

check_rpart_class <- function() {
  skip_on_cran()
  set.seed(1)
  train_dat <- twoClassSim(100)
  train_dat$factor_var <- factor(sample(letters[1:2], nrow(train_dat), replace = TRUE))
  test_dat <- twoClassSim(1000)
  test_dat$factor_var <- factor(sample(letters[1:2], nrow(test_dat), replace = TRUE))
  
  library(rpart)
  rpart_full <- rpart(Class ~ ., data = train_dat)
  rpart_trim <- caret:::trim(rpart_full)
  predict(rpart_full, test_dat)[, "Class1"] - predict(rpart_trim, test_dat)[, "Class1"]
}

###################################################################
## bagging tests functions

check_bag_reg <- function() {
  skip_on_cran()
  set.seed(1)
  train_dat <- SLC14_1(100)
  train_dat$factor_var <- factor(sample(letters[1:2], nrow(train_dat), replace = TRUE))
  test_dat <- SLC14_1(1000)
  test_dat$factor_var <- factor(sample(letters[1:2], nrow(test_dat), replace = TRUE))
  
  library(rpart)
  bag_full <- bagging(y ~ ., data = train_dat)
  bag_trim <- caret:::trim(bag_full)
  predict(bag_full, test_dat) - predict(bag_trim, test_dat)
}

###################################################################
## Tests

test_that("trimmed rpart regression produces identical predicted values", {
  expect_that(sum(check_rpart_reg()), equals(0))
})

test_that("trimmed rpart classification produces identical predicted values", {
  expect_that(sum(check_rpart_class()), equals(0))
})

test_that("trimmed bagging regression produces identical predicted values", {
  expect_that(sum(check_bag_reg()), equals(0))
})


