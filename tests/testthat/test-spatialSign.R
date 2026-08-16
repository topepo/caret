test_that("errors working", {
  skip_on_cran()
  # vector
  expect_snapshot(spatialSign(iris$Species), error = TRUE)

  # matrix
  expect_snapshot(spatialSign(as.matrix(iris)), error = TRUE)

  # data.frame
  expect_snapshot(spatialSign(iris), error = TRUE)
})

test_that("results match", {
  skip_on_cran()
  x = -100:100
  expect_equal(spatialSign(x), x / sqrt(sum(x^2)))

  i4 <- spatialSign(iris[, 1:4])
  expect_equal(
    as.matrix(i4),
    t(apply(iris[, 1:4], 1, spatialSign)),
    ignore_attr = TRUE
  )
})


test_that("high level tests", {
  skip_on_cran()
  i4 <- spatialSign(iris[, 1:4])

  expect_identical(colnames(i4), names(iris[1:4]))
  expect_shape(i4, dim = dim(iris[1:4]))
})


test_that("missing data", {
  skip_on_cran()
  iris[c(1, 51, 101), 1] <- NA

  i5 <- spatialSign(iris[, 1:4])

  exp_res <- iris[, 1:4] /
    apply(iris[, 1:4], 1, function(x) sqrt(sum(x^2, na.rm = TRUE)))

  expect_equal(i5, as.matrix(exp_res), ignore_attr = TRUE)
})
