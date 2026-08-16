# Tests for dotPlot (R/dotplot.varImp.train.R), which draws a varImp.train
# object. The fixture builder lives in helper-plot-varImp.R; the plot and
# ggplot methods are tested in test-plot.varImp.train.R.

test_that("dotPlot builds a lattice object from a varImp.train", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  vi <- varimp_fixture()
  expect_s3_class(caret:::dotPlot(vi), "trellis")
})

test_that("dotPlot groups the classes for per-class importances", {
  skip_on_cran()

  # knn has no model-specific importance, so caret computes one column per
  # class; dotPlot groups the classes when there are three or more importance
  # columns
  set.seed(2)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneLength = 2,
    trControl = trainControl(method = "cv", number = 3)
  )
  vi <- varImp(fit)
  expect_s3_class(caret:::dotPlot(vi), "trellis")
})
