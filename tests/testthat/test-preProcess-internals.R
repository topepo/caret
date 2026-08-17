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

# ------------------------------------------------------------------------------
# pre_process_options

test_that("pre_process_options accepts a per-predictor list of methods", {
  vars <- c(a = "numeric", b = "numeric", f = "string")
  opts <- caret:::pre_process_options(
    list(center = c("a", "b"), scale = "a"),
    vars
  )$opts
  expect_setequal(opts$center, c("a", "b"))
  expect_identical(opts$scale, "a")
  # non-numeric predictors are always ignored
  expect_contains(opts$ignore, "f")
})

test_that("pre_process_options rejects unknown methods and missing fields", {
  vars <- c(a = "numeric", b = "numeric")
  expect_snapshot(
    caret:::pre_process_options(list(bogus = "a"), vars),
    error = TRUE
  )
  expect_snapshot(
    caret:::pre_process_options(list(center = "nope"), vars),
    error = TRUE
  )
})

test_that("pre_process_options moves non-numeric predictors to ignore", {
  vars <- c(a = "numeric", f = "string")
  opts <- caret:::pre_process_options(list(center = c("a", "f")), vars)$opts
  expect_identical(opts$center, "a")
  expect_contains(opts$ignore, "f")
})

test_that("pre_process_options drops group transformations of one predictor", {
  vars <- c(a = "numeric", b = "numeric")
  expect_snapshot_warning(
    opts <- caret:::pre_process_options(
      list(pca = "a", center = "b"),
      vars
    )$opts
  )
  expect_null(opts$pca)

  expect_snapshot_warning(
    ica <- caret:::pre_process_options(list(ica = "a", center = "b"), vars)$opts
  )
  expect_null(ica$ica)

  expect_snapshot_warning(
    ss <- caret:::pre_process_options(
      list(spatialSign = "a", center = "b"),
      vars
    )$opts
  )
  expect_null(ss$spatialSign)
})

test_that("pre_process_options warns when PCA and ICA overlap", {
  vars <- c(a = "numeric", b = "numeric", c = "numeric", d = "numeric")
  # only "c" is shared, so PCA keeps enough fields to survive
  expect_snapshot_warning(
    opts <- caret:::pre_process_options(
      list(pca = c("a", "b", "c"), ica = c("c", "d")),
      vars
    )$opts
  )
  # the overlapping field is left to ICA alone
  expect_setequal(opts$pca, c("a", "b"))
  expect_disjoint(opts$pca, opts$ica)
})

test_that("pre_process_options rejects conflicting options", {
  vars <- c(a = "numeric", b = "numeric")
  # a predictor can only have one imputation method
  expect_snapshot(
    caret:::pre_process_options(
      list(knnImpute = "a", medianImpute = c("a", "b")),
      vars
    ),
    error = TRUE
  )
  # ranging is inconsistent with centering
  expect_snapshot(
    caret:::pre_process_options(list(range = "a", center = "b"), vars),
    error = TRUE
  )
})

test_that("pre_process_options adds the centering that other methods need", {
  vars <- c(a = "numeric", b = "numeric")

  # PCA, ICA and the spatial sign all centre and scale first ...
  pca <- caret:::pre_process_options(list(pca = c("a", "b")), vars)$opts
  expect_setequal(pca$center, c("a", "b"))
  expect_setequal(pca$scale, c("a", "b"))

  ss <- caret:::pre_process_options(list(spatialSign = c("a", "b")), vars)$opts
  expect_setequal(ss$center, c("a", "b"))

  # ... unless the data is being ranged instead
  ranged <- caret:::pre_process_options(
    list(pca = c("a", "b"), range = c("a", "b")),
    vars
  )$opts
  expect_null(ranged$center)
  expect_setequal(ranged$range, c("a", "b"))

  # knn imputation needs every numeric predictor on the same scale
  knn <- caret:::pre_process_options(list(knnImpute = "a"), vars)$opts
  expect_setequal(knn$center, c("a", "b"))
})

# ------------------------------------------------------------------------------
# check_for_wildcards

test_that("check_for_wildcards reports the PCA and ICA wildcards", {
  opts <- list(spatialSign = "_PC_", center = c("a", "_IC_"))
  expect_snapshot(res <- caret:::check_for_wildcards(opts, verbose = TRUE))
  expect_identical(res$wildcards$PCA, "spatialSign")
  # the wildcard placeholders are removed from the options
  expect_identical(res$opts$center, "a")
})

test_that("check_for_wildcards rejects wildcards in the wrong methods", {
  # a wildcard only makes sense for methods applied after PCA/ICA
  opts <- list(pca = "_PC_", ica = "_IC_")
  expect_snapshot(res <- caret:::check_for_wildcards(opts, verbose = TRUE))
  expect_length(res$opts$pca, 0)
  expect_length(res$opts$ica, 0)
})
