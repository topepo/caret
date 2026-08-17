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

test_that("checkInstall offers to install missing packages interactively", {
  # `caret.interactive` makes the prompt reachable from a test session; the
  # prompt and the installer are both replaced so nothing is downloaded
  withr::local_options(caret.interactive = TRUE)

  asked <- NULL
  installed <- NULL
  local_mocked_bindings(
    install_prompt = function(msg) {
      asked <<- msg
      1L
    },
    install_missing = function(pkg) {
      installed <<- pkg
      invisible(pkg)
    },
    .package = "caret"
  )

  expect_null(caret:::checkInstall(c("stats", "nopkg99xyz")))
  expect_identical(installed, "nopkg99xyz")
  # the message names the missing package and uses the singular
  expect_match(asked, "1 package is needed")
  expect_match(asked, "nopkg99xyz")
})

test_that("checkInstall reports several missing packages in the plural", {
  withr::local_options(caret.interactive = TRUE)

  asked <- NULL
  local_mocked_bindings(
    install_prompt = function(msg) {
      asked <<- msg
      1L
    },
    install_missing = function(pkg) invisible(pkg),
    .package = "caret"
  )

  caret:::checkInstall(c("nopkg99xyz", "nopkg98xyz"))
  expect_match(asked, "2 packages are needed")
})

test_that("checkInstall errors when the install is declined", {
  withr::local_options(caret.interactive = TRUE)
  local_mocked_bindings(
    install_prompt = function(msg) 2L,
    install_missing = function(pkg) stop("should not be called"),
    .package = "caret"
  )

  expect_snapshot(caret:::checkInstall("nopkg99xyz"), error = TRUE)
})

test_that("is_interactive follows the option and then the session", {
  withr::local_options(caret.interactive = TRUE)
  expect_true(caret:::is_interactive())

  withr::local_options(caret.interactive = FALSE)
  expect_false(caret:::is_interactive())

  # with no option set it defers to the session, which is not interactive here
  withr::local_options(caret.interactive = NULL)
  expect_identical(caret:::is_interactive(), interactive())
})

test_that("install_prompt shows the message and returns the menu choice", {
  local_mocked_bindings(menu = function(choices) 1L, .package = "caret")
  expect_snapshot(choice <- caret:::install_prompt("Install them now?"))
  expect_identical(choice, 1L)
})

test_that("install_missing routes CRAN and Bioconductor packages separately", {
  cran <- NULL
  bioc <- NULL
  local_mocked_bindings(
    install.packages = function(pkgs, ...) {
      cran <<- c(cran, pkgs)
      invisible(NULL)
    },
    .package = "caret"
  )
  local_mocked_bindings(
    install = function(pkgs, ...) {
      bioc <<- c(bioc, pkgs)
      invisible(NULL)
    },
    .package = "BiocManager"
  )

  # a CRAN-only request never reaches BiocManager
  caret:::install_missing(c("nopkg99xyz", "nopkg98xyz"))
  expect_setequal(cran, c("nopkg99xyz", "nopkg98xyz"))
  expect_null(bioc)

  # affy is a Bioconductor package, so the two are split
  cran <- NULL
  caret:::install_missing(c("nopkg99xyz", "affy"))
  expect_identical(cran, "nopkg99xyz")
  expect_identical(bioc, "affy")
})

test_that("install_missing installs BiocManager when it is absent", {
  cran <- NULL
  local_mocked_bindings(
    install.packages = function(pkgs, ...) {
      cran <<- c(cran, pkgs)
      invisible(NULL)
    },
    has_biocmanager = function() FALSE,
    .package = "caret"
  )
  local_mocked_bindings(
    install = function(pkgs, ...) invisible(NULL),
    .package = "BiocManager"
  )

  caret:::install_missing("affy")
  expect_identical(cran, "BiocManager")
})
