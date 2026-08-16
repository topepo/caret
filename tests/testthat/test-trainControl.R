# Tests for trainControl (R/trainControl.R).

test_that("resampling method 'none' doesn't conflict with default tuneLength", {
  skip_on_cran()

  data(BloodBrain)

  expect_snapshot(
    train(
      bbbDescr,
      logBBB,
      method = "earth",
      tuneLength = 2,
      trControl = trainControl(method = "none")
    ),
    error = TRUE
  )

  expect_snapshot(
    train(
      mpg ~ cyl + disp,
      data = mtcars,
      method = "gam",
      tuneLength = 2,
      trControl = trainControl(method = "none")
    ),
    error = TRUE
  )
})
