# Tests for the univariate Box-Cox estimator BoxCoxTrans (R/BoxCoxTrans.R).
# The shared skew_y fixture lives in helper-transforms.R; its twin expoTrans
# is tested in test-expoTrans.R.

test_that("BoxCoxTrans estimates a lambda and transforms the data", {
  bc <- BoxCoxTrans(skew_y)
  expect_s3_class(bc, "BoxCoxTrans")
  expect_identical(bc$n, 120L)

  # lognormal data is well modelled by lambda = 0 (the log transform); lambda is
  # an estimate, so expect_equal
  expect_equal(bc$lambda, 0)
  # at lambda 0 the transform is exactly log(), so this is bit-identical
  expect_identical(unname(predict(bc, skew_y)), log(skew_y))

  # a NA lambda means no transformation is applied
  bc_na <- bc
  bc_na$lambda <- NA
  expect_identical(predict(bc_na, skew_y), skew_y)
})

test_that("BoxCoxTrans applies the power transform for a non-zero lambda", {
  bc <- BoxCoxTrans(skew_y)

  # a non-zero lambda uses the (y^lambda - 1) / lambda branch; lambda = 0.5 is
  # the square-root-like transform
  bc$lambda <- 0.5
  expect_identical(
    unname(predict(bc, skew_y)),
    (skew_y^0.5 - 1) / 0.5
  )

  # lambda = -1 is the inverse transform
  bc$lambda <- -1
  expect_identical(
    unname(predict(bc, skew_y)),
    (skew_y^-1 - 1) / -1
  )
})

test_that("BoxCoxTrans validates its input", {
  expect_snapshot(BoxCoxTrans(factor(letters)), error = TRUE)
  expect_snapshot(predict(BoxCoxTrans(skew_y), "abc"), error = TRUE)
  # non-positive values in newdata warn
  expect_snapshot(predict(BoxCoxTrans(skew_y), c(-1, 1, 2)))
})

test_that("print.BoxCoxTrans identifies the transformation", {
  # the printed lambda is an estimated float, so match the stable header
  expect_output(print(BoxCoxTrans(skew_y)), "Box-Cox Transformation")
})
