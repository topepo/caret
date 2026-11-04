# Tests for the model-specific varImp() methods (R/varImp.R). Each method is a
# thin dispatcher that loads the model's registry code and calls its varImp
# element, so one small fit per model class exercises it. The heavy or
# awkward dependencies (glmnet, RWeka's Java models, the archived partDSA and
# FCNN4R) are exercised through mocked registry code and fake fits from
# helper-varImp.R instead.

test_that("glmnet varImp returns non-negative values", {
  # the registry code is read straight from the model database, so this needs
  # no glmnet install; the fake fit's coefficients are negative
  code <- getModelInfo("glmnet", regex = FALSE)[[1]]
  fake <- structure(list(lambdaOpt = 0.1), class = "fake_glmnet")

  vis <- code$varImp(fake, lambda = 0.1)
  expect_s3_class(vis, "data.frame")
  # importances are the absolute coefficients, so they stay non-negative
  expect_all_true(vis$Overall >= 0)
  expect_equal(vis$Overall, c(3, 1.5, 0.5))
  # the intercept is not a predictor
  expect_identical(rownames(vis), c("a", "b", "c"))
})

test_that("glmnet varImp handles multi-response fits and a missing lambda", {
  code <- getModelInfo("glmnet", regex = FALSE)[[1]]

  # a multi-response fit returns one coefficient matrix per class, which are
  # combined into one column each
  multi <- structure(list(), class = "fake_glmnet_multi")
  vis <- code$varImp(multi, lambda = 0.1)
  expect_named(vis, c("first", "second"))
  expect_identical(rownames(vis), c("a", "b"))
  expect_equal(vis$second, c(4, 1))

  # without a lambda, and with none stored on the fit, the code gives up
  expect_snapshot(
    code$varImp(structure(list(), class = "fake_glmnet")),
    error = TRUE
  )
})

test_that("varImp works for lm and glm fits", {
  skip_on_cran()

  lm_fit <- lm(mpg ~ cyl + disp + hp, data = mtcars)
  vi <- varImp(lm_fit)
  expect_s3_class(vi, "data.frame")
  expect_named(vi, "Overall")
  # the importance is the absolute t-statistic
  expect_equal(
    vi$Overall,
    unname(abs(coef(summary(lm_fit))[-1, "t value"])),
    tolerance = 1e-8
  )

  glm_fit <- glm(am ~ hp + wt, data = mtcars, family = binomial)
  vi_glm <- varImp(glm_fit)
  expect_s3_class(vi_glm, "data.frame")
  expect_identical(rownames(vi_glm), c("hp", "wt"))
})

test_that("varImp works for rpart fits", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  fit <- rpart::rpart(Species ~ ., data = iris)
  vi <- varImp(fit)
  expect_s3_class(vi, "data.frame")
  expect_in(rownames(vi), colnames(iris))

  # competing splits can be excluded
  vi2 <- varImp(fit, competes = FALSE)
  expect_s3_class(vi2, "data.frame")
})

test_that("varImp works for randomForest and RRF fits", {
  skip_on_cran()
  skip_if_not_installed("randomForest")

  set.seed(921)
  rf <- randomForest::randomForest(iris[, 1:4], iris$Species, ntree = 5)
  vi <- varImp(rf)
  expect_s3_class(vi, "data.frame")
  expect_identical(rownames(vi), colnames(iris[, 1:4]))

  # loading RRF re-registers randomForest S3 methods, which prints a note
  suppressMessages(skip_if_not_installed("RRF"))
  set.seed(921)
  rrf <- RRF::RRF(iris[, 1:4], iris$Species, ntree = 5)
  vi2 <- varImp(rrf)
  expect_s3_class(vi2, "data.frame")
})

test_that("varImp works for cforest fits", {
  skip_on_cran()
  skip_if_not_installed("party")

  set.seed(7503)
  fit <- party::cforest(
    Species ~ .,
    data = iris,
    controls = party::cforest_unbiased(ntree = 5, mtry = 2)
  )
  vi <- varImp(fit)
  expect_s3_class(vi, "data.frame")
})

test_that("varImp works for gbm fits", {
  skip_on_cran()
  skip_if_not_installed("gbm")
  # the registry code calls relative.influence() unqualified
  suppressMessages(withr::local_package("gbm"))

  set.seed(2216)
  fit <- gbm::gbm(
    mpg ~ cyl + disp + hp + wt,
    data = mtcars,
    distribution = "gaussian",
    n.trees = 10,
    n.minobsinnode = 3,
    verbose = FALSE
  )
  vi <- varImp(fit, numTrees = 10)
  expect_s3_class(vi, "data.frame")
  expect_named(vi, "Overall")
})

test_that("varImp works for C5.0 and Cubist fits", {
  skip_on_cran()
  skip_if_not_installed("C50")

  fit <- C50::C5.0(iris[, 1:4], iris$Species)
  vi <- varImp(fit)
  expect_s3_class(vi, "data.frame")

  skip_if_not_installed("Cubist")
  cb <- Cubist::cubist(mtcars[, -1], mtcars$mpg)
  vi2 <- varImp(cb)
  expect_s3_class(vi2, "data.frame")
})

test_that("varImp works for bagged tree fits (classbagg, regbagg)", {
  skip_on_cran()
  skip_if_not_installed("ipred")

  set.seed(5930)
  cls <- ipred::bagging(Species ~ ., data = iris, nbagg = 3)
  vi <- varImp(cls)
  expect_s3_class(vi, "data.frame")

  set.seed(5930)
  reg <- ipred::bagging(mpg ~ ., data = mtcars, nbagg = 3)
  vi2 <- varImp(reg)
  expect_s3_class(vi2, "data.frame")
})

test_that("varImp works for earth, bagEarth and bagFDA fits", {
  skip_on_cran()
  skip_if_not_installed("earth")

  fit <- earth::earth(mpg ~ ., data = mtcars)
  vi <- varImp(fit)
  expect_s3_class(vi, "data.frame")
  # the rss statistic is an alternative importance measure
  vi_rss <- varImp(fit, value = "rss")
  expect_s3_class(vi_rss, "data.frame")

  # the bagEarth/bagFDA registry importance code resolves plyr's `.` helper
  # from the search path
  suppressMessages(withr::local_package("plyr"))

  set.seed(3341)
  be <- bagEarth(mtcars[, -1], mtcars$mpg, B = 2)
  expect_s3_class(varImp(be), "data.frame")

  skip_if_not_installed("mda")
  set.seed(3341)
  bf <- bagFDA(Species ~ ., data = iris, B = 2)
  expect_s3_class(varImp(bf), "data.frame")
})

test_that("varImp works for fda fits", {
  skip_on_cran()
  skip_if_not_installed("mda")
  skip_if_not_installed("earth")

  fit <- mda::fda(Species ~ ., data = iris, method = earth::earth)
  vi <- varImp(fit)
  expect_s3_class(vi, "data.frame")
})

test_that("varImp works for pls (mvr) and plsda fits", {
  skip_on_cran()
  skip_if_not_installed("pls")

  fit <- pls::plsr(mpg ~ ., data = mtcars, ncomp = 2)
  # the registry code attaches pls, which masks caret::R2 with a message
  vi <- suppressMessages(varImp(fit))
  expect_s3_class(vi, "data.frame")

  pd <- plsda(iris[, 1:4], iris$Species, ncomp = 2)
  vi2 <- suppressMessages(varImp(pd))
  expect_s3_class(vi2, "data.frame")
})

test_that("varImp works for gam fits from mgcv and gam", {
  skip_on_cran()
  skip_if_not_installed("mgcv")

  fit <- mgcv::gam(mpg ~ s(hp) + cyl, data = mtcars)
  vi <- varImp(fit)
  expect_s3_class(vi, "data.frame")

  skip_if_not_installed("gam")
  # the importance comes from the smooth-term anova, so the fit needs s()
  # terms; gam must be attached for s() to resolve inside gam::gam()
  suppressMessages(withr::local_package("gam"))
  fit2 <- gam::gam(mpg ~ s(hp) + s(wt), data = mtcars)
  expect_s3_class(fit2, "Gam")
  vi2 <- varImp(fit2)
  expect_s3_class(vi2, "data.frame")
})

test_that("varImp works for multinom fits", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  set.seed(8524)
  fit <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)
  vi <- varImp(fit)
  expect_s3_class(vi, "data.frame")
})

# --- neural networks and GarsonWeights ----------------------------------------

test_that("varImp uses Garson's method for classification nnet fits", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  # three output units: the hidden-to-output labels are h*->o1, o2, o3
  set.seed(6098)
  fit <- nnet::nnet(
    Species ~ .,
    data = iris,
    size = 2,
    trace = FALSE,
    maxit = 30
  )
  vi <- varImp(fit)
  # with one importance column per class, the registry returns the raw
  # Garson matrix
  expect_true(is.matrix(vi))
  expect_identical(rownames(vi), colnames(iris[, 1:4]))
  # importances are percentages per output
  expect_all_true(as.vector(colSums(vi) > 99))
})

test_that("varImp uses Garson's method for regression nnet fits", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  # one output unit: the hidden-to-output labels have no output index, and the
  # single response column is named Y1
  set.seed(1866)
  fit <- nnet::nnet(
    mpg ~ cyl + hp + wt,
    data = mtcars,
    size = 2,
    linout = TRUE,
    trace = FALSE,
    maxit = 30
  )
  vi <- varImp(fit)
  expect_s3_class(vi, "data.frame")
  expect_length(vi, 1)
})

test_that("GarsonWeights falls back to generic names without coefnames", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  # a matrix-interface fit with unnamed predictors has no coefnames, so the
  # rows fall back to X1, X2, ...
  set.seed(9660)
  x <- unname(as.matrix(mtcars[, c("cyl", "hp")]))
  fit <- nnet::nnet(
    x,
    mtcars$mpg,
    size = 2,
    linout = TRUE,
    trace = FALSE,
    maxit = 30
  )
  imp <- caret:::GarsonWeights(fit)
  expect_identical(rownames(imp), c("X1", "X2"))
  expect_identical(colnames(imp), "Y1")
})

test_that("varImp.avNNet averages the importances of its committee", {
  skip_on_cran()
  skip_if_not_installed("nnet")

  set.seed(2740)
  # suppress the one-time "executing %dopar% sequentially" foreach notice
  fit <- suppressWarnings(avNNet(
    Species ~ .,
    data = iris,
    size = 2,
    repeats = 2,
    trace = FALSE,
    maxit = 30
  ))
  vi <- varImp(fit)
  expect_s3_class(vi, "data.frame")
  # the committee nnets are fit on an unnamed model matrix, so the importances
  # carry the generic X1, ... names
  expect_setequal(rownames(vi), paste0("X", 1:4))
})

test_that("GarsonWeights_FCNN4R computes importances from the fake network", {
  fit <- make_fcnn_fit()

  imp <- caret:::GarsonWeights_FCNN4R(fit)
  expect_identical(dim(imp), c(2L, 1L))
  expect_identical(rownames(imp), c("X1", "X2"))
  expect_identical(colnames(imp), "Y1")
  # Garson importances are percentages
  expect_equal(sum(imp[, 1]), 100)

  named <- caret:::GarsonWeights_FCNN4R(
    fit,
    xnames = c("a", "b"),
    ynames = "out"
  )
  expect_identical(rownames(named), c("a", "b"))
  expect_identical(colnames(named), "out")
})

# --- dispatchers for burdensome packages (mocked registry) ---------------------

test_that("the JRip and PART dispatchers forward to the registry code", {
  local_mocked_bindings(
    varImpDependencies = function(libName) recording_registry(),
    .package = "caret"
  )

  jrip <- structure(list(), class = "JRip")
  out <- varImp(jrip)
  expect_identical(out$object, jrip)

  part <- structure(list(), class = "PART")
  out2 <- varImp(part)
  expect_identical(out2$object, part)
})

test_that("the glmnet dispatcher forwards the lambda argument", {
  local_mocked_bindings(
    varImpDependencies = function(libName) recording_registry(),
    .package = "caret"
  )

  fit <- structure(list(), class = "glmnet")
  out <- varImp(fit, lambda = 0.1)
  expect_identical(out$object, fit)
  expect_identical(out$lambda, 0.1)
})

test_that("the dsa dispatcher forwards the cuts argument", {
  local_mocked_bindings(
    varImpDependencies = function(libName) recording_registry(),
    .package = "caret"
  )

  dsa <- structure(list(), class = "dsa")
  out <- varImp(dsa, cuts = 3)
  expect_identical(out$object, dsa)
  expect_identical(out$cuts, 3)
})

test_that("varImp.pamrtrained pulls the threshold and data from a train fit", {
  # the method reaches into $bestTune and $finalModel$xData, i.e. it expects
  # the enclosing train object rather than the bare pamr fit
  local_mocked_bindings(
    varImpDependencies = function(libName) recording_registry(),
    .package = "caret"
  )

  fake <- structure(
    list(
      bestTune = list(threshold = 2.5),
      finalModel = list(xData = mtcars[, 1:3])
    ),
    class = "pamrtrained"
  )
  out <- varImp(fake)
  expect_identical(out$threshold, 2.5)
  expect_identical(out$data, mtcars[, 1:3])
})
