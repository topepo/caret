context('ptypes')

data("Sacramento", package = "caret")

mtcars_0 <- mtcars[0, -1]
sac_0 <- Sacramento[0, -7]
wt_0 <- mtcars[0, 6, drop = FALSE]

# ------------------------------------------------------------------------------

none <- trainControl(method = "none")

f_plain <- train(mpg ~ ., data = mtcars, method = "lm", trControl = none)
f_oned <- train(mpg ~ wt, data = mtcars, method = "lm", trControl = none)
f_inter <- train(mpg ~ (.)^2, data = mtcars, method = "lm", trControl = none)
f_dmmy <- train(price ~ ., data = Sacramento, method = "lm", trControl = none)

xy_plain <- train(x = mtcars[, -1], y = mtcars$mpg, method = "lm", trControl = none)
xy_oned <- train(x = mtcars[, "wt", drop = FALSE], y = mtcars$mpg, method = "lm", trControl = none)
xy_dmmy <- train(x = Sacramento[, -7], y = Sacramento$price, method = "lm", trControl = none)

# ------------------------------------------------------------------------------

test_that("ptypes for formulas", {
  expect_equal(f_plain$ptype, mtcars_0)
  expect_equal(f_oned$ptype, wt_0)
  expect_equal(f_inter$ptype, mtcars_0)
  expect_equal(f_dmmy$ptype, sac_0)
})

test_that("ptypes for formulas", {
  expect_equal(xy_plain$ptype, mtcars_0)
  expect_equal(xy_oned$ptype, wt_0)
  expect_equal(xy_dmmy$ptype, sac_0)
})
