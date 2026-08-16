test_that("handles situation when there's a single column per transformation", {
  skip_on_cran()
  # For the data below, the list of transformations used internally by
  # "preProcess" contains a single related attribute
  # for each transformation; namely, `method=list(center="y", ignore="x")`.
  # At certain point such setting didn't work properly.
  x <- data.frame(x = factor(c("a", "a")), y = c(1, 2), stringsAsFactors = TRUE)
  model <- caret::preProcess(method = "center", x = x)
  preprocessed <- predict(model, x)
  expect_equal(
    preprocessed,
    data.frame(x = c("a", "a"), y = c(-0.5, 0.5), stringsAsFactors = TRUE)
  )
})

# ------------------------------------------------------------------------------

test_that("invHyperbolicSineFunc is the inverse hyperbolic sine", {
  expect_identical(caret:::invHyperbolicSineFunc(0), 0)
  # computed as log(x + sqrt(x^2 + 1)), so equal (not bit-identical to asinh)
  expect_equal(caret:::invHyperbolicSineFunc(1), asinh(1))
})

test_that("convert_method rewrites a legacy character method into a list", {
  # older preProcess objects stored `method` as a character vector; each entry
  # maps to the variable names it applies to
  old <- list(
    method = c(
      "center",
      "scale",
      "YeoJohnson",
      "expoTrans",
      "BoxCox",
      "medianImpute",
      "pca",
      "spatialSign"
    ),
    mean = c(a = 1, b = 2),
    std = c(a = 1, b = 1),
    yj = c(a = 1),
    et = c(a = 1),
    bc = c(a = 1),
    median = c(a = 1)
  )
  new <- caret:::convert_method(old)
  expect_type(new$method, "list")
  expect_identical(new$method$center, c("a", "b"))
  expect_identical(new$method$scale, c("a", "b"))
  expect_named(new$method, old$method)
})

test_that("check_for_wildcards reports and strips PCA/ICA wildcards", {
  opts <- list(pca = "_PC_", center = c("x1", "_PC_"))
  expect_snapshot(res <- caret:::check_for_wildcards(opts))
  # the wildcard tokens are removed from the returned options
  expect_false("_PC_" %in% unlist(res$opts))
})
