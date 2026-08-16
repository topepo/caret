# Tests for the univariate exponential-transformation estimator expoTrans
# (R/expoTrans.R) and its manly/manlyLik helpers. The shared skew_y fixture
# lives in helper-transforms.R; its twin BoxCoxTrans is tested in
# test-BoxCoxTrans.R.

test_that("expoTrans estimates a lambda and transforms the data", {
  et <- expoTrans(skew_y)
  expect_s3_class(et, "expoTrans")
  expect_type(et$lambda, "double")
  expect_length(predict(et, skew_y), length(skew_y))
})

test_that("expoTrans validates its newdata", {
  expect_snapshot(predict(expoTrans(skew_y), "abc"), error = TRUE)
})

test_that("print.expoTrans identifies the transformation", {
  expect_output(print(expoTrans(skew_y)), "Exponential Transformation")
})

test_that("manly applies the transform and reduces to the identity at 0", {
  # lambda = 0 leaves the data unchanged
  expect_identical(caret:::manly(c(1, 2, 3), 0), c(1, 2, 3))
  # the log-likelihood is a finite number
  expect_true(is.finite(caret:::manlyLik(0.5, c(1, 2, 3))))
})
