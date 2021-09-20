
library(testthat)
library(caret)
library(themis)

context('upsampling with recipes')

# ------------------------------------------------------------------------------

set.seed(2542)
dat <- twoClassSim(200, intercept = 6)

# ------------------------------------------------------------------------------

rec <-
  recipe(Class ~ TwoFactor1 + TwoFactor2 + Linear01, data = dat) %>%
  themis::step_upsample(Class, seed = 534)


test_that("model test", {
  mod <- train(rec, dat, method = "knn", trControl = trainControl(method = "cv"))
  expect_equivalent(
    rep(max(table(dat$Class)), 2),
    as.vector(table(mod$finalModel$learn$y))
  )
})
