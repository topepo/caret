# Tests for the univariate power-transformation estimators BoxCoxTrans
# (R/BoxCoxTrans.R) and expoTrans (R/expoTrans.R). They are twins - each takes a
# numeric vector, estimates a lambda, and provides print/predict methods - so
# they are tested together. The shared skew_y fixture lives in
# helper-transforms.R.

# --- BoxCoxTrans ------------------------------------------------------------

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

# --- expoTrans --------------------------------------------------------------

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

# --- manly helpers (used by expoTrans) --------------------------------------

test_that("manly applies the transform and reduces to the identity at 0", {
  # lambda = 0 leaves the data unchanged
  expect_identical(caret:::manly(c(1, 2, 3), 0), c(1, 2, 3))
  # the log-likelihood is a finite number
  expect_true(is.finite(caret:::manlyLik(0.5, c(1, 2, 3))))
})
