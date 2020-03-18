library(caret)
library(recipes)
library(dplyr)
library(testthat)

set.seed(1)
dat <- twoClassSim(100)
dat_tb <- as_tibble(dat)
a <- dat[,5]
y <- dat[["Class"]]
df <- data.frame(a, y, stringsAsFactors = TRUE)
rec <- recipe(y ~ .,data = df)

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

test_that('train runs on tibbles and recipes with glm', {
  expect_error(
    train(
      rec,
      data = as_tibble(df),
      method = "glm",
      family = "binomial",
      metric = "ROC",
      trControl = ctrl
    ),
    regexp = NA
  )
})

test_that('train runs on tibbles and formulas with glm', {

  expect_error(
    train(
      y ~ .,
      data = as_tibble(df),
      method = "glm",
      family = "binomial",
      metric = "ROC",
      trControl = ctrl
    ),
    regexp = NA
  )
})

test_that('train runs on tibbles and recipes with glm', {
  expect_error(
    train(
      rec,
      data = as_tibble(df),
      method = "glm",
      family = "binomial",
      metric = "ROC",
      trControl = ctrl
    ),
    regexp = NA
  )
})


test_that('downsampling on tibble', {
  expect_error(
    caret:::parse_sampling("down")$func(dat_tb[, 1], dat_tb$Class),
    regexp = NA
  )
})

test_that('upsampling on tibble', {
  expect_error(
    caret:::parse_sampling("up")$func(dat_tb[, 1], dat_tb$Class),
    regexp = NA
  )
})

# check these manually to avoid more dependencies
# caret:::parse_sampling("smote")$func(dat_tb[, 1], dat_tb$Class)
# caret:::parse_sampling("rose")$func(dat_tb[, 1], dat_tb$Class)


