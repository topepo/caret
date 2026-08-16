# Fixtures (maxdiss_base, maxdiss_pool, maxdiss_split_x) live in helper-maxDissim.R

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
  expect_equal(maxDissim(maxdiss_base, maxdiss_pool, n = 1), 3)
  # then, by the minimum-distance objective, p2 is the next most dissimilar
  expect_equal(maxDissim(maxdiss_base, maxdiss_pool, n = 2), c(3, 2))
})

test_that("maxDissim can return row names instead of indices", {
  expect_equal(
    maxDissim(maxdiss_base, maxdiss_pool, n = 1, useNames = TRUE),
    "p3"
  )
})

test_that("maxDissim warns and falls back to indices without row names", {
  pool_noname <- maxdiss_pool
  rownames(pool_noname) <- NULL
  expect_snapshot_warning(
    res <- maxDissim(maxdiss_base, pool_noname, n = 1, useNames = TRUE)
  )
  expect_equal(res, 3)
})

test_that("maxDissim supports alternative objective functions", {
  # with the sum objective the ties resolve to p1 as the second pick
  expect_equal(
    maxDissim(maxdiss_base, maxdiss_pool, n = 2, obj = sumDiss),
    c(3, 1)
  )
})

test_that("maxDissim can subsample candidates with randomFrac", {
  set.seed(1)
  res <- maxDissim(maxdiss_base, maxdiss_pool, n = 2, randomFrac = 0.9)
  expect_length(res, 2)
  expect_in(res, seq_len(nrow(maxdiss_pool)))
})

test_that("maxDissim prints progress when verbose", {
  expect_snapshot(maxDissim(maxdiss_base, maxdiss_pool, n = 1, verbose = TRUE))
})

test_that("maxDissim validates its arguments", {
  expect_snapshot(
    maxDissim(maxdiss_base, maxdiss_pool[1, , drop = FALSE]),
    error = TRUE
  )
  expect_snapshot(
    maxDissim(maxdiss_base[, 1, drop = FALSE], maxdiss_pool),
    error = TRUE
  )
  expect_snapshot(maxDissim(maxdiss_base, maxdiss_pool, n = 99), error = TRUE)
  expect_snapshot(
    maxDissim(maxdiss_base, maxdiss_pool, randomFrac = 2),
    error = TRUE
  )
  expect_snapshot(
    maxDissim(maxdiss_base, maxdiss_pool, randomFrac = 0),
    error = TRUE
  )
})

# --- splitter / splitByDissim (internal) ------------------------------------

test_that("splitByDissim returns a subsample of row indices", {
  res <- caret:::splitByDissim(maxdiss_split_x, p = 0.5, start = 1)
  expect_true(is.numeric(res))
  expect_contains(res, 1)
  expect_in(res, seq_len(nrow(maxdiss_split_x)))
})

test_that("splitter picks a random start when none is supplied", {
  set.seed(2)
  res <- caret:::splitter(maxdiss_split_x, p = 0.5)
  expect_true(is.numeric(res))
  expect_in(res, seq_len(nrow(maxdiss_split_x)))
})

test_that("splitByDissim stratifies by a factor outcome", {
  # a character outcome exercises the as.factor() coercion path
  y <- rep(c("a", "b"), each = nrow(maxdiss_split_x) / 2)
  # start needs one index per group (rows 1-5 are "a", 6-10 are "b")
  res <- caret:::splitByDissim(maxdiss_split_x, p = 0.5, y = y, start = c(1, 6))
  expect_true(is.numeric(res))
  expect_in(res, seq_len(nrow(maxdiss_split_x)))
})
