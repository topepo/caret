# `check.medianImpute()` lives in helper-preProcess.R

test_that("median Impute works for matrix with named columns", {
  skip_on_cran()
  # Tested data matrix
  set.seed(9019)
  x <- matrix(rnorm(20, mean = 10, sd = 5), nrow = 4)
  x[2, 1] <- x[3, 4] <- x[2, 5] <- x[4, 5] <- NA
  x[, 3] <- NA
  colnames(x) <- paste0("Var.", 1:ncol(x))

  check.medianImpute(x)
})

test_that("median Impute works for data.frames", {
  skip_on_cran()
  # Tested data matrix
  set.seed(9019)
  x <- matrix(rnorm(20, mean = 10, sd = 5), nrow = 4)
  x[2, 1] <- x[3, 4] <- x[2, 5] <- x[4, 5] <- NA
  x[, 3] <- NA
  colnames(x) <- paste0("Var.", 1:ncol(x))

  check.medianImpute(as.data.frame(x, stringsAsFactors = TRUE))
})

test_that("correlation filter", {
  skip_on_cran()
  expect_equal(
    preProcess(iris, "corr")$method,
    list(ignore = "Species", remove = "Petal.Length")
  )
})

test_that("preProcess print method", {
  skip_on_cran()
  expect_snapshot(
    print(preProcess(iris[, 1:4], method = c("center", "scale", "pca")))
  )
  expect_snapshot(print(preProcess(iris[, 1:4], method = "range")))
})

# ------------------------------------------------------------------------------
# verbose reporting

test_that("preProcess reports its progress when verbose", {
  skip_on_cran()

  set.seed(4180)
  dat <- data.frame(
    a = rlnorm(40),
    b = rlnorm(40),
    c = rnorm(40),
    zv = rep(1, 40)
  )
  # zero variance, correlation, Box-Cox and the centring/scaling steps all
  # announce themselves; the counts are integers so the snapshot is stable
  expect_snapshot(
    pp <- preProcess(
      dat,
      method = c("zv", "BoxCox", "center", "scale"),
      verbose = TRUE
    )
  )
  expect_s3_class(pp, "preProcess")
})

test_that("preProcess adds a zero-variance filter for the correlation filter", {
  skip_on_cran()

  set.seed(9524)
  dat <- data.frame(a = rnorm(30), flat = rep(1, 30))
  dat$b <- dat$a + rnorm(30, sd = 0.01)
  # findCorrelation's verbose output prints rounded correlation means, whose
  # last digit varies by platform
  expect_snapshot(
    pp <- preProcess(dat, method = "corr", verbose = TRUE),
    transform = mask_decimals
  )
  # the zero-variance column is filtered out before the correlations, and the
  # redundant copy of "a" is then dropped
  expect_setequal(pp$method$remove, c("flat", "b"))
})

test_that("preProcess reports near-zero variance and conditional filters", {
  skip_on_cran()

  set.seed(6741)
  dat <- data.frame(
    a = rnorm(60),
    nzv = c(rep(1, 59), 2)
  )
  expect_snapshot(pp <- preProcess(dat, method = "nzv", verbose = TRUE))
  expect_contains(pp$method$remove, "nzv")
})

test_that("preProcess reports the other transformations when verbose", {
  skip_on_cran()

  set.seed(2098)
  dat <- data.frame(a = rlnorm(40), b = rlnorm(40), c = rlnorm(40))
  expect_snapshot(
    pp <- preProcess(
      dat,
      method = c("YeoJohnson", "invHyperbolicSine"),
      verbose = TRUE
    )
  )
  expect_s3_class(pp, "preProcess")

  expect_snapshot(
    pp2 <- preProcess(dat, method = "expoTrans", verbose = TRUE)
  )
  expect_s3_class(pp2, "preProcess")
})

test_that("preProcess reports imputation and dimension reduction", {
  skip_on_cran()
  skip_if_not_installed("RANN")

  set.seed(8752)
  dat <- data.frame(a = rnorm(40), b = rnorm(40), c = rnorm(40))
  dat$a[c(3, 9)] <- NA

  expect_snapshot(
    pp <- preProcess(dat, method = c("knnImpute", "pca"), verbose = TRUE)
  )
  expect_s3_class(pp, "preProcess")
})

test_that("preProcess requires a matrix or data frame", {
  expect_snapshot(preProcess(1:10), error = TRUE)
})

# ------------------------------------------------------------------------------
# print.preProcess

test_that("print.preProcess summarises the transformations", {
  skip_on_cran()

  set.seed(3311)
  dat <- data.frame(a = rlnorm(40), b = rlnorm(40), c = rlnorm(40))

  # Box-Cox lambdas are estimated floats, so mask them
  expect_snapshot(
    print(preProcess(dat, method = c("BoxCox", "center", "scale"))),
    transform = mask_decimals
  )
  expect_snapshot(
    print(preProcess(dat, method = "YeoJohnson")),
    transform = mask_decimals
  )
})

test_that("print.preProcess describes PCA and ICA components", {
  skip_on_cran()

  set.seed(1466)
  dat <- as.data.frame(matrix(rnorm(40 * 6), ncol = 6))

  # a variance threshold and a fixed component count print differently
  expect_snapshot(
    print(preProcess(dat, method = "pca", thresh = 0.8)),
    transform = mask_decimals
  )
  expect_snapshot(print(preProcess(dat, method = "pca", pcaComp = 2)))

  skip_if_not_installed("fastICA")
  set.seed(1466)
  expect_snapshot(print(preProcess(dat, method = "ica", n.comp = 2)))
})

test_that("print.preProcess notes the wildcard spatial sign", {
  skip_on_cran()

  set.seed(7295)
  dat <- as.data.frame(matrix(rnorm(40 * 6), ncol = 6))
  # "_PC_" asks for the spatial sign to be applied to the PCA scores
  pp <- preProcess(
    dat,
    method = list(pca = names(dat), spatialSign = "_PC_"),
    pcaComp = 2
  )
  expect_snapshot(print(pp))
})

test_that("print.preProcess summarises many Box-Cox lambdas", {
  skip_on_cran()

  set.seed(5087)
  dat <- as.data.frame(matrix(rlnorm(40 * 12), ncol = 12))
  # more than ten transformations are printed as a summary
  expect_snapshot(
    print(preProcess(dat, method = "BoxCox")),
    transform = mask_decimals
  )
  expect_snapshot(
    print(preProcess(dat, method = "YeoJohnson")),
    transform = mask_decimals
  )
})

# ------------------------------------------------------------------------------
# matrix input and degenerate predictors

test_that("preProcess filters a matrix as well as a data frame", {
  skip_on_cran()

  set.seed(1053)
  x <- cbind(
    a = rnorm(40),
    flat = rep(1, 40),
    nearly_flat = c(rep(1, 39), 2)
  )
  pp <- preProcess(x, method = c("zv", "nzv"))
  expect_setequal(pp$method$remove, c("flat", "nearly_flat"))
  expect_named(as.data.frame(predict(pp, x)), "a")
})

test_that("preProcess warns about predictors it cannot scale or range", {
  skip_on_cran()

  set.seed(6628)
  dat <- data.frame(a = rnorm(30), flat = rep(2, 30))
  # a constant predictor has no spread to scale by
  expect_snapshot_warning(pp <- preProcess(dat, method = "scale"))
  expect_s3_class(pp, "preProcess")

  # and none to stretch to a range
  expect_snapshot_warning(pp2 <- preProcess(dat, method = "range"))
  expect_s3_class(pp2, "preProcess")
})

test_that("preProcess reports conditionally zero-variance predictors", {
  skip_on_cran()

  # "flat_in_a" is constant within the first class only
  dat <- data.frame(
    a = c(1, 2, 3, 4, 5, 6, 7, 8),
    flat_in_a = c(5, 5, 5, 5, 1, 2, 3, 4)
  )
  y <- factor(rep(c("one", "two"), each = 4))
  expect_snapshot(
    pp <- preProcess(dat, method = "conditionalX", outcome = y, verbose = TRUE)
  )
  expect_contains(pp$method$remove, "flat_in_a")
})

test_that("the correlation filter needs more than one predictor", {
  skip_on_cran()

  # a single predictor gives a 1x1 correlation matrix, which findCorrelation
  # rejects rather than treating the filter as a no-op
  dat <- data.frame(a = c(1, 2, 3, 4, 5, 6))
  expect_snapshot(preProcess(dat, method = "corr"), error = TRUE)
})

test_that("preProcess drops transformations that fail for every predictor", {
  skip_on_cran()

  # Box-Cox needs positive values with some spread; these have neither, so
  # every estimate fails and the method is dropped
  dat <- data.frame(a = c(-1, -2, -3, -4, -5), b = c(-2, -3, -4, -5, -6))
  expect_snapshot(pp <- preProcess(dat, method = "BoxCox", verbose = TRUE))
  expect_false("BoxCox" %in% names(pp$method))
})

test_that("preProcess passes row.norm through to fastICA", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  set.seed(4498)
  dat <- as.data.frame(matrix(rnorm(40 * 4), ncol = 4))
  pp <- preProcess(dat, method = "ica", n.comp = 2, row.norm = TRUE)
  expect_s3_class(pp, "preProcess")
  expect_identical(ncol(predict(pp, dat)), 2L)
})
