context("Testing sparse Matrix")

test_that("caret can return sparse Matrix object", {
  skip_on_cran()
  skip_if_not_installed('glmnet')
  skip_if_not_installed('Matrix')

  x <- Matrix::Matrix(as.matrix(mtcars)[, -1], sparse=TRUE)
  y <- mtcars$mpg
  expect_warning(train(x, y, method="glmnet"), NA)
  expect_warning(train(as.matrix(x), y, method="glmnet"), NA)
})
