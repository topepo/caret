# Fixtures (md_a, md_b, md_x) live in helper-maxDissim.R

# --- minDiss / sumDiss ------------------------------------------------------

test_that("minDiss and sumDiss reduce a vector, ignoring NAs", {
  expect_equal(minDiss(c(3, 1, 2)), 1)
  expect_equal(sumDiss(c(3, 1, 2)), 6)
  expect_equal(minDiss(c(NA, 2, 5)), 2)
  expect_equal(sumDiss(c(NA, 2, 5)), 7)
})

# --- maxDissim --------------------------------------------------------------

test_that("maxDissim selects the most dissimilar points in order", {
  # farthest single point is p3 (distance 10 from the origin)
  expect_equal(maxDissim(md_a, md_b, n = 1), 3)
  # then, by the minimum-distance objective, p2 is the next most dissimilar
  expect_equal(maxDissim(md_a, md_b, n = 2), c(3, 2))
})

test_that("maxDissim can return row names instead of indices", {
  expect_equal(maxDissim(md_a, md_b, n = 1, useNames = TRUE), "p3")
})

test_that("maxDissim warns and falls back to indices without row names", {
  b_noname <- md_b
  rownames(b_noname) <- NULL
  expect_warning(
    res <- maxDissim(md_a, b_noname, n = 1, useNames = TRUE),
    "Cannot use rownames"
  )
  expect_equal(res, 3)
})

test_that("maxDissim supports alternative objective functions", {
  # with the sum objective the ties resolve to p1 as the second pick
  expect_equal(maxDissim(md_a, md_b, n = 2, obj = sumDiss), c(3, 1))
})

test_that("maxDissim can subsample candidates with randomFrac", {
  set.seed(1)
  res <- maxDissim(md_a, md_b, n = 2, randomFrac = 0.9)
  expect_length(res, 2)
  expect_true(all(res %in% seq_len(nrow(md_b))))
})

test_that("maxDissim prints progress when verbose", {
  expect_output(maxDissim(md_a, md_b, n = 1, verbose = TRUE), "adding")
})

test_that("maxDissim validates its arguments", {
  expect_error(
    maxDissim(md_a, md_b[1, , drop = FALSE]),
    "at least 2 samples"
  )
  expect_error(
    maxDissim(md_a[, 1, drop = FALSE], md_b),
    "same number of columns"
  )
  expect_error(maxDissim(md_a, md_b, n = 99), "n must be less than")
  expect_error(maxDissim(md_a, md_b, randomFrac = 2), "randomFrac must be in")
  expect_error(maxDissim(md_a, md_b, randomFrac = 0), "randomFrac must be in")
})

# --- splitter / splitByDissim (internal) ------------------------------------

test_that("splitByDissim returns a subsample of row indices", {
  res <- caret:::splitByDissim(md_x, p = 0.5, start = 1)
  expect_true(is.numeric(res))
  expect_true(1 %in% res)
  expect_true(all(res %in% seq_len(nrow(md_x))))
})

test_that("splitter picks a random start when none is supplied", {
  set.seed(2)
  res <- caret:::splitter(md_x, p = 0.5)
  expect_true(is.numeric(res))
  expect_true(all(res %in% seq_len(nrow(md_x))))
})

test_that("splitByDissim stratifies by a factor outcome", {
  # a character outcome exercises the as.factor() coercion path
  y <- rep(c("a", "b"), each = nrow(md_x) / 2)
  # start needs one index per group (rows 1-5 are "a", 6-10 are "b")
  res <- caret:::splitByDissim(md_x, p = 0.5, y = y, start = c(1, 6))
  expect_true(is.numeric(res))
  expect_true(all(res %in% seq_len(nrow(md_x))))
})
