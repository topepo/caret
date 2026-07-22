# Fixtures (flc_simple, flc_full, flc_multi) live in helper-collinearity.R

# --- findLinearCombos -------------------------------------------------------

test_that("findLinearCombos finds a single dependency", {
  res <- findLinearCombos(flc_simple)
  expect_equal(res$linearCombos, list(c(3, 1, 2)))
  expect_equal(res$remove, 3)
})

test_that("findLinearCombos reports nothing for a full-rank matrix", {
  res <- findLinearCombos(flc_full)
  expect_equal(res$linearCombos, list())
  expect_null(res$remove)
})

test_that("findLinearCombos resolves multiple dependencies iteratively", {
  res <- findLinearCombos(flc_multi)
  expect_equal(res$linearCombos, list(c(3, 1, 2), c(6, 1, 4, 5)))
  expect_equal(res$remove, c(3, 6))
})

test_that("findLinearCombos coerces non-matrix input", {
  res <- findLinearCombos(as.data.frame(flc_simple))
  expect_equal(res$remove, 3)
})

# --- enumLC methods ---------------------------------------------------------

test_that("enumLC errors for unsupported objects", {
  expect_error(caret:::enumLC("nope"), "does not support")
})

test_that("enumLC works for lm and formula objects", {
  set.seed(1)
  x1 <- rnorm(20)
  x2 <- rnorm(20)
  y <- x1 + x2 + rnorm(20)
  # the formula method routes through the lm method, and both end up in
  # internalEnumLC; a full-rank fit has no linear combinations
  expect_type(caret:::enumLC(y ~ x1 + x2), "list")
  expect_type(caret:::enumLC(lm(y ~ x1 + x2)), "list")
})
