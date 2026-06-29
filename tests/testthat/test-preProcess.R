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
