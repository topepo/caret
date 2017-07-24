library(caret)
library(testthat)

context("sampling options")
load(system.file("models", "sampling.RData", package = "caret"))

test_that('check appropriate sampling calls by name', {
  skip_on_cran()
  arg_names <- c("up", "down", "rose", "smote")
  arg_funcs <- sampling_methods
  arg_first <- c(TRUE, FALSE)

  ## test that calling by string gives the right result
  for(i in arg_names) {
    out <- caret:::parse_sampling(i, check_install = FALSE)
    expected <- list(name = i,
                     func = sampling_methods[[i]],
                     first = TRUE)
    expect_equivalent(out, expected)
  }
})

test_that('check appropriate sampling calls by function', {
  skip_on_cran()
  arg_names <- c("up", "down", "rose", "smote")
  arg_funcs <- sampling_methods
  arg_first <- c(TRUE, FALSE)

  ## test that calling by function gives the right result
  for(i in arg_names) {
    out <- caret:::parse_sampling(sampling_methods[[i]], 
                                  check_install = FALSE)
    expected <- list(name = "custom",
                     func = sampling_methods[[i]],
                     first = TRUE)
    expect_equivalent(out, expected)
  }
})

test_that('check bad sampling name', {
  skip_on_cran()
  expect_error(caret:::parse_sampling("what?"))
})

test_that('check bad first arg', {
  skip_on_cran()
  expect_error(
    caret:::parse_sampling(
      list(name = "yep", func = sampling_methods[["up"]], first = 2), 
      check_install = FALSE)
    )
})

test_that('check bad func arg', {
  skip_on_cran()
  expect_error(
    caret:::parse_sampling(
      list(name = "yep", func = I, first = 2), 
      check_install = FALSE)
    )
})

test_that('check incomplete list', {
  skip_on_cran()
  expect_error(caret:::parse_sampling(list(name = "yep"), check_install = FALSE))
})

test_that('check call', {
  skip_on_cran()
  expect_error(caret:::parse_sampling(14, check_install = FALSE))
})

###################################################################
##

test_that('check getting all methods', {
  skip_on_cran()
  expect_equivalent(getSamplingInfo(), sampling_methods)
})

test_that('check getting one method', {
  skip_on_cran()
  arg_names <- c("up", "down", "rose", "smote")
  for(i in arg_names) {
    out <- getSamplingInfo(i, regex = FALSE)
    expected <- list(sampling_methods[[i]])
    names(expected) <- i
    expect_equivalent(out, expected)
  }
})

test_that('check missing method', {
  skip_on_cran()
  expect_error(getSamplingInfo("plum"))
})

