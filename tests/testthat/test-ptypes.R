# Shared data fixtures (mtcars_*, sac_*) live in helper-ptypes.R

test_that("ptypes for formulas", {
  skip_on_cran()
  none <- trainControl(method = "none")

  f_plain <- train(mpg ~ ., data = mtcars, method = "lm", trControl = none)
  f_oned <- train(mpg ~ wt, data = mtcars, method = "lm", trControl = none)
  f_inter <- train(mpg ~ (.)^2, data = mtcars, method = "lm", trControl = none)
  f_dmmy <- train(price ~ ., data = sac_train, method = "lm", trControl = none)

  expect_equal(f_plain$ptype, mtcars_0)
  expect_equal(f_oned$ptype, wt_0)
  expect_equal(f_inter$ptype, mtcars_0)
  expect_equal(f_dmmy$ptype, sac_0)
})

test_that("ptypes for x/y interface", {
  skip_on_cran()
  none <- trainControl(method = "none")

  xy_plain <- train(
    x = mtcars_x,
    y = mtcars$mpg,
    method = "lm",
    trControl = none
  )
  xy_oned <- train(
    x = mtcars[, "wt", drop = FALSE],
    y = mtcars$mpg,
    method = "lm",
    trControl = none
  )
  xy_dmmy <- train(
    x = sac_x_train,
    y = sac_y_train,
    method = "lm",
    trControl = none
  )

  expect_equal(xy_plain$ptype, mtcars_0)
  expect_equal(xy_oned$ptype, wt_0)
  expect_equal(xy_dmmy$ptype, sac_0)
})
