# Tests for createResample (R/createResample.R), which draws bootstrap
# samples of the row indices.

test_that("createResample draws bootstrap samples of the right size", {
  set.seed(3517)
  out <- createResample(1:20, times = 3)
  expect_type(out, "list")
  expect_named(out, paste0("Resample", 1:3))
  expect_all_true(lengths(out) == 20)
  # sampling is with replacement, so indices repeat
  expect_in(unlist(out), 1:20)
  expect_lt(length(unique(out[[1]])), 20)
})

test_that("createResample can return a matrix", {
  set.seed(9268)
  out <- createResample(1:15, times = 2, list = FALSE)
  expect_shape(out, dim = c(15L, 2L))
  expect_identical(colnames(out), paste0("Resample", 1:2))
})

test_that("createResample samples the survival times of a Surv outcome", {
  set.seed(6180)
  y <- fake_surv(c(5, 6, 7, 8, 9, 10), c(1, 0, 1, 1, 0, 1))
  out <- createResample(y, times = 2)
  expect_named(out, paste0("Resample", 1:2))
  expect_all_true(lengths(out) == 6)
})
