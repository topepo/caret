# Fixtures (corr_R1, corr_R2) live in helper-collinearity.R

# --- findCorrelation --------------------------------------------------------

test_that("findCorrelation (exact) flags the expected columns", {
  expect_equal(findCorrelation(corr_R1, cutoff = 0.6, exact = TRUE), c(1, 5, 4))
})

test_that("findCorrelation (fast) flags the expected columns", {
  expect_equal(
    findCorrelation(corr_R1, cutoff = 0.6, exact = FALSE),
    c(4, 5, 1, 3)
  )
})

test_that("findCorrelation can return column names", {
  expect_equal(
    findCorrelation(corr_R1, cutoff = 0.6, exact = TRUE, names = TRUE),
    c("x1", "x5", "x4")
  )
})

test_that("findCorrelation handles negative correlations", {
  expect_equal(findCorrelation(corr_R2, cutoff = 0.65), c(3, 4))
})

test_that("findCorrelation returns nothing when nothing exceeds the cutoff", {
  expect_length(findCorrelation(corr_R2, cutoff = 0.99), 0)
})

test_that("findCorrelation errors when names are requested but unavailable", {
  m <- corr_R2
  dimnames(m) <- NULL
  expect_snapshot(findCorrelation(m, names = TRUE), error = TRUE)
})

test_that("findCorrelation prints details when verbose", {
  expect_snapshot(findCorrelation(corr_R2, cutoff = 0.65, verbose = TRUE))
  # the exact method prints which column it flags at each comparison
  expect_snapshot(
    findCorrelation(corr_R1, cutoff = 0.6, exact = TRUE, verbose = TRUE)
  )
  # the fast method has its own verbose output
  expect_snapshot(
    findCorrelation(corr_R1, cutoff = 0.6, exact = FALSE, verbose = TRUE)
  )
  # the exact method reports when every correlation is below the cutoff
  expect_snapshot(findCorrelation(corr_R2, cutoff = 0.99, verbose = TRUE))
})

# --- internal helpers -------------------------------------------------------

test_that("findCorrelation_fast rejects missing values", {
  m <- corr_R1
  m[1, 2] <- NA
  expect_snapshot(caret:::findCorrelation_fast(m, cutoff = 0.6), error = TRUE)
})

test_that("findCorrelation_exact rejects non-symmetric and singleton input", {
  ns <- corr_R1
  ns[1, 2] <- 0.1 # break symmetry
  expect_snapshot(caret:::findCorrelation_exact(ns), error = TRUE)
  expect_snapshot(
    caret:::findCorrelation_exact(matrix(1, 1, 1)),
    error = TRUE
  )
})
