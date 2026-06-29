test_that("model test", {
  skip_on_cran()
  skip_if_not_installed("themis")
  withr::local_seed(2542)
  dat <- twoClassSim(200, intercept = 6)

  rec <-
    recipe(Class ~ TwoFactor1 + TwoFactor2 + Linear01, data = dat) %>%
    themis::step_upsample(Class, seed = 534)

  mod <- train(
    rec,
    dat,
    method = "knn",
    trControl = trainControl(method = "cv")
  )
  expect_equal(
    rep(max(table(dat$Class)), 2),
    as.vector(table(mod$finalModel$learn$y)),
    ignore_attr = TRUE
  )
})
