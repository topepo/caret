
context('misc functions')


test_that("R2 and RMSE are calculating correctly", {

  pred <- runif(25)
  obs <- runif(25)

  expect_equal(R2(pred, obs), cor(obs, pred)^2)
  expect_equal(RMSE(pred, obs), sqrt(mean((pred - obs)^2)))

})
