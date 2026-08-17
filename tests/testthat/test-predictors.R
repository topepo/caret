# Tests for the predictors() methods (R/predictors.R), which report the
# variables a fitted model actually uses.

test_that("predictors.formula and predictors.terms strip the outcome", {
  expect_identical(predictors(mpg ~ cyl + disp), c("cyl", "disp"))
  # interactions and transformations reduce to the underlying variables
  expect_identical(predictors(y ~ a * b + log(c)), c("a", "b", "c"))

  trms <- terms(mpg ~ cyl + hp, data = mtcars)
  expect_identical(predictors(trms), c("cyl", "hp"))
  # a missing terms object gives NA
  expect_identical(caret:::predictors.terms(NULL), NA)
})

test_that("predictors.list maps over its elements", {
  out <- predictors(list(one = mpg ~ cyl, two = y ~ a + b))
  expect_named(out, c("one", "two"))
  expect_identical(out$two, c("a", "b"))
})

test_that("hasTerms and basicVars work", {
  expect_true(caret:::hasTerms(lm(mpg ~ cyl, data = mtcars)))
  expect_false(caret:::hasTerms(list(x = 1)))

  # basicVars finds the raw variables inside derived terms
  expect_identical(
    caret:::basicVars(
      c("medv", "crim", "zn", "age"),
      c("crim", "I(age^2)", "zn")
    ),
    c("crim", "zn", "age")
  )
})

test_that("predictors.train uses the registry code when available", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  set.seed(9152)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "rpart",
    tuneLength = 1,
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_in(predictors(fit), colnames(iris))

  # without registry code, the terms are used when present ...
  no_code <- fit
  no_code$modelInfo$predictors <- NULL
  expect_identical(predictors(no_code), colnames(iris[, 1:4]))

  # ... and NA is returned without them
  no_terms <- no_code
  no_terms$terms <- NULL
  expect_identical(predictors(no_terms), NA)
})

test_that("predictors.default finds registry code from the model class", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  fit <- rpart::rpart(Species ~ ., data = iris)
  expect_in(predictors(fit), colnames(iris))
})

test_that("predictors.default resolves the two gam flavours", {
  skip_on_cran()
  skip_if_not_installed("mgcv")

  # an mgcv fit carries an 'optimizer' element
  fit <- mgcv::gam(mpg ~ s(hp) + cyl, data = mtcars)
  expect_setequal(predictors(fit), c("hp", "cyl"))

  # a gam-class object without one is routed to the gamLoess registry code
  fake <- structure(
    list(terms = terms(mpg ~ hp + wt, data = mtcars)),
    class = "gam"
  )
  expect_identical(predictors(fake), c("hp", "wt"))
})

test_that("predictors.default falls back to terms for unknown classes", {
  unknown <- structure(
    list(terms = terms(mpg ~ cyl + disp, data = mtcars)),
    class = "someunknownclass"
  )
  expect_identical(predictors(unknown), c("cyl", "disp"))

  no_terms <- structure(list(x = 1), class = "someunknownclass")
  expect_identical(predictors(no_terms), NA)
})

test_that("predictors.train looks up registry code when modelInfo is absent", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  set.seed(2350)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "rpart",
    tuneLength = 1,
    trControl = trainControl(method = "cv", number = 3)
  )
  fit$modelInfo <- NULL
  expect_in(predictors(fit), colnames(iris))
})

test_that("predictors.default handles registry code without a predictors element", {
  # the ada registry entry has no predictors function, so the terms are used
  fake <- structure(
    list(terms = terms(mpg ~ cyl + wt, data = mtcars)),
    class = "ada"
  )
  expect_identical(predictors(fake), c("cyl", "wt"))

  no_terms <- structure(list(x = 1), class = "ada")
  expect_identical(predictors(no_terms), NA)
})
