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

test_that("expoTrans refuses missing data when na.rm is FALSE", {
  y <- c(skew_y[1:10], NA)
  expect_snapshot(expoTrans(y, na.rm = FALSE), error = TRUE)
  # with na.rm the missing value is simply ignored
  et <- expoTrans(y)
  expect_identical(et$n, 10L)
})

test_that("expoTrans skips estimation for too few unique values", {
  const <- rep(2, 20)
  et <- expoTrans(const)
  expect_true(is.na(et$lambda))
  # with no lambda the data passes through untouched
  expect_identical(predict(et, const), const)
  expect_snapshot(print(et))
})

test_that("expoTrans.numeric handles the same edge cases", {
  y <- c(skew_y[1:10], NA)
  expect_snapshot(caret:::expoTrans.numeric(y, na.rm = FALSE), error = TRUE)
  et <- caret:::expoTrans.numeric(rep(3, 20))
  expect_true(is.na(et$lambda))
})

test_that("manlyLik guards against non-finite likelihoods", {
  # an enormous lambda overflows the transform, so the likelihood is clamped
  out <- caret:::manlyLik(500, c(1, 2, 3))
  expect_true(is.finite(out))
  expect_identical(out, .Machine$double.xmax)
})

test_that("expoTrans.default guards its input like the numeric method", {
  # the default method is reached through the generic only for non-numeric
  # classes, so call it directly
  y <- c(skew_y[1:10], NA)
  expect_snapshot(caret:::expoTrans.default(y, na.rm = FALSE), error = TRUE)

  et <- caret:::expoTrans.default(rep(4, 20))
  expect_true(is.na(et$lambda))
})
