# Tests for the model-metadata utilities in R/modelLookup.R: modelLookup,
# getModelInfo, checkInstall and missing_packages. These are pure lookups (no
# model fitting), so the results are deterministic.

test_that("modelLookup returns the tuning parameters for a model", {
  ml <- modelLookup("knn")
  expect_identical(
    colnames(ml),
    c("model", "parameter", "label", "forReg", "forClass", "probModel")
  )
  expect_identical(unique(ml$model), "knn")
  expect_identical(ml$parameter, "k")
})

test_that("modelLookup with no argument lists every model", {
  all <- modelLookup()
  expect_s3_class(all, "data.frame")
  expect_gt(nrow(all), 100)
  expect_in(c("knn", "rf", "glm"), all$model)
})

test_that("modelLookup errors for an unknown model", {
  expect_snapshot(modelLookup("nnnotamodel"), error = TRUE)
})

test_that("getModelInfo returns the model definition", {
  # an exact lookup returns one model with fit/predict functions
  info <- getModelInfo("knn", regex = FALSE)
  expect_named(info, "knn")
  expect_type(info$knn$fit, "closure")
  expect_type(info$knn$predict, "closure")

  # a regex lookup can match several models
  expect_gte(length(getModelInfo("rpart")), 1)

  # a pattern that matches nothing is an error
  expect_snapshot(getModelInfo("zzznomatchxyz"), error = TRUE)
})

test_that("checkInstall is silent for installed packages and errors otherwise", {
  expect_null(caret:::checkInstall("stats"))
  # in a non-interactive session a missing package is a hard error
  expect_snapshot(caret:::checkInstall("nopkg99xyz"), error = TRUE)
})

test_that("missing_packages reports nothing when the model's deps are present", {
  # knn only needs base packages, so nothing is missing
  expect_null(caret:::missing_packages(getModelInfo("knn", regex = FALSE)))
})
