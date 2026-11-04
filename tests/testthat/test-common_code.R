# Tests for the helpers in R/common_code.R that gafs() and safs() share. The
# searches themselves are covered in test-gafs.R and test-safs.R; these are the
# pieces that can be pinned down on their own.

test_that("same_args compares two sets of variable names", {
  expect_true(caret:::same_args(c("a", "b"), c("b", "a")))
  # different lengths cannot match
  expect_false(caret:::same_args(c("a", "b"), "a"))
  # same length, different contents
  expect_false(caret:::same_args(c("a", "b"), c("a", "c")))
})

test_that("index2vec turns positions into an indicator vector", {
  expect_identical(index2vec(1:2, vars = 5), c(1, 1, 0, 0, 0))
  # the signed encoding uses -1 for the variables that are left out
  expect_identical(index2vec(1:2, vars = 5, sign = TRUE), c(1, 1, -1, -1, -1))
})

test_that("jack_sim measures the overlap between two subsets", {
  a <- index2vec(1:3, vars = 5)
  b <- index2vec(2:4, vars = 5)
  # two variables in common out of the four that either subset uses
  expect_equal(caret:::jack_sim(a, b), 50)
  # identical subsets overlap completely
  expect_equal(caret:::jack_sim(a, a), 100)
})

test_that("jack_sim uses the first row of a matrix", {
  a <- rbind(index2vec(1:3, vars = 5), index2vec(1, vars = 5))
  b <- rbind(index2vec(2:4, vars = 5), index2vec(5, vars = 5))

  # a population of subsets arrives as a matrix; only the first is compared
  expect_equal(caret:::jack_sim(a, b), 50)
  # either argument can be the matrix
  expect_equal(caret:::jack_sim(a, index2vec(2:4, vars = 5)), 50)
  expect_equal(caret:::jack_sim(index2vec(1:3, vars = 5), b), 50)
})

test_that("change_text describes how a subset changed", {
  # growing and shrinking are both written relative to the old size
  expect_identical(caret:::change_text(1:3, 1:5, p = 10), " (3+2, 60.0%)")
  expect_identical(caret:::change_text(1:5, 1:3, p = 10), " (5-2, 60.0%)")
  # the size is given on its own when it did not change
  expect_identical(caret:::change_text(1:3, 2:4, p = 10), " (3, 50.0%)")
})

test_that("change_text can report the sizes instead of the difference", {
  expect_identical(
    caret:::change_text(1:3, 1:5, p = 10, show_diff = FALSE),
    " ( 3-> 5, 60.0%)"
  )
})

test_that("predictors lists the variables a search settled on", {
  # both methods read the same element of the fitted object
  ga <- structure(list(best_vars = c("x1", "x3")), class = "gafs")
  expect_identical(predictors(ga), c("x1", "x3"))

  sa <- structure(list(best_vars = "x2"), class = "safs")
  expect_identical(predictors(sa), "x2")
})
