# Tests for plot.varImp.train and ggplot.varImp.train (R/plot.varImp.train.R),
# which draw a varImp.train object. The fixture builder lives in
# helper-plot-varImp.R; the related dotPlot() is tested in
# test-dotplot.varImp.train.R.

test_that("the varImp plot methods build lattice and ggplot objects", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  vi <- varimp_fixture()
  expect_s3_class(plot(vi), "trellis")
  expect_s3_class(ggplot(vi), "ggplot")
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

  # with a single importance row, the range computation warns via base max()
  vi <- varimp_fixture(formula = Class ~ TwoFactor1)
  expect_snapshot_warning(p_lattice <- plot(vi))
  expect_s3_class(p_lattice, "trellis")
  expect_snapshot_warning(p_gg <- ggplot(vi))
  expect_s3_class(p_gg, "ggplot")
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
})
