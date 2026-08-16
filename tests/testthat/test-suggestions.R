# Tests for suggestions(), a pure lookup that maps a model name to the
# pre-processing steps caret suggests, as a named logical vector
# c(center, scale, nzv, corr).

test_that("suggestions flags centering, scaling and nzv for distance models", {
  # knn wants centering, scaling and near-zero-variance filtering
  expect_identical(
    caret:::suggestions("knn"),
    c(center = TRUE, scale = TRUE, nzv = TRUE, corr = FALSE)
  )
  expect_identical(
    caret:::suggestions("svmRadial"),
    c(center = TRUE, scale = TRUE, nzv = TRUE, corr = FALSE)
  )
})

test_that("suggestions returns model-specific combinations", {
  # glmnet wants centering/scaling but not the nzv filter
  expect_identical(
    caret:::suggestions("glmnet"),
    c(center = TRUE, scale = TRUE, nzv = FALSE, corr = FALSE)
  )
  # linear models want only the nzv filter
  expect_identical(
    caret:::suggestions("lm"),
    c(center = FALSE, scale = FALSE, nzv = TRUE, corr = FALSE)
  )
})

test_that("suggestions returns all FALSE for an unrecognised model", {
  expect_identical(
    caret:::suggestions("rf"),
    c(center = FALSE, scale = FALSE, nzv = FALSE, corr = FALSE)
  )
})

test_that("suggestions never recommends the correlation filter", {
  # corr is initialised FALSE and never set, so it is FALSE for every model
  models <- c("knn", "glmnet", "lm", "lda", "nnet", "pls", "rf")
  expect_all_false(
    vapply(models, function(m) caret:::suggestions(m)["corr"], logical(1))
  )
})
