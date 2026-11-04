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

test_that("spatialSign returns zeros for an all-zero vector", {
  # the projection is undefined at the origin, so the result is left at zero
  expect_identical(spatialSign(c(0, 0, 0)), c(0, 0, 0))
})

test_that("spatialSign keeps the orientation of a one-cell matrix", {
  # a single row of a single column would otherwise be transposed by apply()
  res <- spatialSign(matrix(5, nrow = 1, ncol = 1))
  expect_shape(res, dim = c(1L, 1L))
  expect_identical(res[1, 1], 1)
})

test_that("spatialSign rejects data frames that are not numeric", {
  # a complex column passes the character/factor screen but still does not
  # give a numeric matrix
  expect_snapshot(spatialSign(data.frame(a = c(1 + 2i, 3 + 1i))), error = TRUE)
})
