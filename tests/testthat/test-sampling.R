# Tests for the class-balancing helpers downSample and upSample
# (R/sampling.R). Both take predictors and a factor outcome and return a
# balanced data set, either stacked or as a list.

test_that("downSample shrinks every class to the smallest one", {
  set.seed(9702)
  x <- data.frame(a = rnorm(30), b = rnorm(30))
  y <- factor(rep(c("one", "two", "three"), times = c(5, 10, 15)))

  out <- downSample(x, y)
  expect_s3_class(out, "data.frame")
  # the outcome is appended under the default name
  expect_named(out, c("a", "b", "Class"))
  expect_all_equal(as.vector(table(out$Class)), 5L)
})

test_that("downSample can rename the outcome or return a list", {
  set.seed(1522)
  x <- data.frame(a = rnorm(20))
  y <- factor(rep(c("one", "two"), times = c(5, 15)))

  named <- downSample(x, y, yname = "outcome")
  expect_named(named, c("a", "outcome"))

  as_list <- downSample(x, y, list = TRUE)
  expect_named(as_list, c("x", "y"))
  expect_s3_class(as_list$y, "factor")
  # the predictors keep their data frame form
  expect_named(as_list$x, "a")
})

test_that("downSample converts matrix input and keeps the list form", {
  set.seed(4113)
  x <- matrix(rnorm(40), ncol = 2, dimnames = list(NULL, c("a", "b")))
  y <- factor(rep(c("one", "two"), times = c(5, 15)))

  out <- downSample(x, y, list = TRUE)
  expect_named(out, c("x", "y"))
  expect_identical(nrow(out$x), 10L)
})

test_that("downSample returns the data untouched for a numeric outcome", {
  x <- data.frame(a = rnorm(10))
  expect_snapshot_warning(out <- downSample(x, 1:10))
  expect_named(out, c("x", "y"))
  expect_identical(out$y, 1:10)
})

test_that("upSample grows every class to the largest one", {
  set.seed(6849)
  x <- data.frame(a = rnorm(30), b = rnorm(30))
  y <- factor(rep(c("one", "two", "three"), times = c(5, 10, 15)))

  out <- upSample(x, y)
  expect_named(out, c("a", "b", "Class"))
  expect_all_equal(as.vector(table(out$Class)), 15L)
})

test_that("upSample can rename the outcome or return a list", {
  set.seed(2977)
  x <- data.frame(a = rnorm(20))
  y <- factor(rep(c("one", "two"), times = c(5, 15)))

  named <- upSample(x, y, yname = "outcome")
  expect_named(named, c("a", "outcome"))

  as_list <- upSample(x, y, list = TRUE)
  expect_named(as_list, c("x", "y"))
  expect_identical(nrow(as_list$x), 30L)
})

test_that("upSample converts matrix input", {
  set.seed(8065)
  x <- matrix(rnorm(40), ncol = 2, dimnames = list(NULL, c("a", "b")))
  y <- factor(rep(c("one", "two"), times = c(5, 15)))

  out <- upSample(x, y, list = TRUE)
  expect_identical(nrow(out$x), 30L)
})

test_that("upSample returns the data untouched for a numeric outcome", {
  x <- data.frame(a = rnorm(10))
  expect_snapshot_warning(out <- upSample(x, 1:10))
  expect_named(out, c("x", "y"))
})
