# Tests for the package-level odds and ends in R/aaa.R.

test_that("is_cran_check reads the NOT_CRAN environment variable", {
  # the test suite itself runs with NOT_CRAN = "true", which is what makes
  # skip_on_cran() let the model fits through
  withr::local_envvar(NOT_CRAN = "true")
  expect_false(caret:::is_cran_check())

  # anything else counts as a check, including the variable being unset
  withr::local_envvar(NOT_CRAN = "false")
  expect_true(caret:::is_cran_check())

  withr::local_envvar(NOT_CRAN = NA)
  expect_true(caret:::is_cran_check())
})

test_that(".onUnload asks for caret's shared object to be unloaded", {
  # Really unloading it mid-suite would take the rest of the tests with it, so
  # the hook is called with a path that holds no shared object: nothing is
  # unloaded either way. How the refusal is reported depends on whether caret
  # was installed or loaded by load_all(), so both streams are captured and the
  # outcome is not asserted - what matters is that the compiled code still runs.
  invisible(capture.output(
    invisible(capture.output(
      suppressWarnings(try(
        caret:::.onUnload("/nonexistent/library/path"),
        silent = TRUE
      )),
      type = "message"
    ))
  ))
  expect_length(findCorrelation(diag(3), verbose = FALSE), 0)
})

test_that("best picks the row that optimises the metric", {
  x <- data.frame(RMSE = c(3, 1, 2), Rsquared = c(0.1, 0.9, 0.5))
  expect_identical(best(x, "RMSE", maximize = FALSE), 2L)
  expect_identical(best(x, "Rsquared", maximize = TRUE), 2L)
  # ties go to the first candidate
  expect_identical(best(data.frame(RMSE = c(1, 1)), "RMSE", FALSE), 1L)
})

test_that("defaultSummary coerces a character outcome to a factor", {
  # the class levels have to come from `lev`, since a character column cannot
  # say which classes were possible but unobserved
  dat <- data.frame(
    obs = c("a", "a", "b"),
    pred = factor(c("a", "b", "b"), levels = c("a", "b")),
    stringsAsFactors = FALSE
  )
  out <- defaultSummary(dat, lev = c("a", "b"))
  expect_named(out, c("Accuracy", "Kappa"))
  expect_equal(unname(out["Accuracy"]), 2 / 3)

  # a factor outcome is used as it stands
  dat$obs <- factor(dat$obs, levels = c("a", "b"))
  expect_equal(defaultSummary(dat, lev = c("a", "b")), out)
})
