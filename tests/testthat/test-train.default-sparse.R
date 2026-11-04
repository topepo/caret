test_that("caret can return sparse Matrix object", {
  skip_on_cran()
  skip_if_not_installed("glmnet")
  skip_if_not_installed("Matrix")

  # Seeded: glmnet warns when a lambda does not converge, and whether that
  # happens depends on the resampling draw, so without a seed the
  # expect_no_warning() calls below are a lottery.
  set.seed(2811)

  x <- Matrix::Matrix(as.matrix(mtcars)[, -1], sparse = TRUE)
  y <- mtcars$mpg
  expect_no_warning(train(x, y, method = "glmnet"))
  expect_no_warning(train(as.matrix(x), y, method = "glmnet"))

  cls_y <- factor(rep_len(letters[1:2], nrow(mtcars)))
  ctrl <- trainControl(
    method = "cv",
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  expect_no_error(
    train(x, cls_y, method = "glmnet", metric = "ROC", trControl = ctrl)
  )
  expect_no_error(
    train(
      as.matrix(x),
      cls_y,
      method = "glmnet",
      metric = "ROC",
      trControl = ctrl
    )
  )
})
