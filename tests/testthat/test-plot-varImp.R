# Tests for the variable-importance plot methods. plot.varImp.train and
# ggplot.varImp.train (R/plot.varImp.train.R) and the underlying dotPlot
# (R/dotplot.varImp.train.R) all draw a varImp.train object, so they are tested
# together. The fixture builder lives in helper-plot-varImp.R.

test_that("the varImp plot methods build lattice and ggplot objects", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  vi <- varimp_fixture()
  expect_s3_class(plot(vi), "trellis")
  expect_s3_class(ggplot(vi), "ggplot")
  expect_s3_class(caret:::dotPlot(vi), "trellis")
})

test_that("plot.varImp.train honours the 'top' argument", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  vi <- varimp_fixture()
  expect_s3_class(plot(vi, top = 5), "trellis")
  expect_s3_class(ggplot(vi, top = 5), "ggplot")
})

test_that("the varImp plot methods handle a single predictor", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  vi <- varimp_fixture(formula = Class ~ TwoFactor1)
  expect_s3_class(plot(vi), "trellis")
  expect_s3_class(ggplot(vi), "ggplot")
})

test_that("the varImp plot methods handle per-class importances", {
  skip_on_cran()

  # knn has no model-specific importance, so caret computes one column per
  # class; the plots then condition on the class
  set.seed(2)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneLength = 2,
    trControl = trainControl(method = "cv", number = 3)
  )
  vi <- varImp(fit)
  expect_gt(ncol(vi$importance), 1)
  expect_s3_class(plot(vi), "trellis")
  expect_s3_class(ggplot(vi), "ggplot")
  # dotPlot groups the classes when there are three or more importance columns
  expect_s3_class(caret:::dotPlot(vi), "trellis")
})
