library(caret)

test_that('bad class levels', {
  skip_on_cran()
  set.seed(1)
  dat <- twoClassSim(100)
  dat$Class <- factor(ifelse(dat$Class == "Class1", "1", "0"))
  foo <- function(train_dat)
    train(Class ~ ., data = train_dat, method = "rpart",
          metric = "ROC",
          trControl = trainControl(classProbs = TRUE, summaryFunction = twoClassSummary))
  expect_error(foo(dat))
})

test_that('no class probs with ROC', {
  skip_on_cran()
  set.seed(1)
  dat <- twoClassSim(100)
  foo <- function(train_dat)
    train(Class ~ ., data = train_dat, method = "rpart",
          metric = "ROC",
          trControl = trainControl(summaryFunction = twoClassSummary))
  expect_error(foo(dat))
})

test_that('numeric y and classification', {
  skip_on_cran()
  set.seed(1)
  dat <- twoClassSim(100)
  dat$Class <- ifelse(dat$Class == "Class1", 1, 0)
  foo <- function(train_dat)
    train(Class ~ ., data = train_dat, method = "rpart")
  expect_warning(foo(dat))
})

test_that('3+ classes and twoClassSummary', {
  skip_on_cran()
  foo <- function() {
    data(oil)
    train(x = fattyAcids, y = oilType, method = "rpart",
          metric = "ROC",
          trControl = trainControl(classProbs = TRUE,
                                   summaryFunction = twoClassSummary))
  }
  expect_error(foo())
})