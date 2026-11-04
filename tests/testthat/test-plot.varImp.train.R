# Tests for plot.varImp.train and ggplot.varImp.train (R/plot.varImp.train.R),
# which draw a varImp.train object. The fixture builder lives in
# helper-plot-varImp.R; the related dotPlot() is tested in
# test-dotplot.varImp.train.R.

test_that("the varImp plot methods build lattice and ggplot objects", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  vi <- varimp_fixture()
  expect_s3_class(draw_trellis(plot(vi)), "trellis")
  expect_s3_class(ggplot(vi), "ggplot")
})

test_that("plot.varImp.train honours the 'top' argument", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  vi <- varimp_fixture()
  expect_s3_class(draw_trellis(plot(vi, top = 5)), "trellis")
  expect_s3_class(ggplot(vi, top = 5), "ggplot")
})

test_that("the varImp plot methods handle a single predictor", {
  skip_on_cran()
  skip_if_not_installed("rpart")

  # with a single importance row, the range computation warns via base max()
  vi <- varimp_fixture(formula = Class ~ TwoFactor1)
  expect_snapshot_warning(p_lattice <- plot(vi))
  expect_s3_class(draw_trellis(p_lattice), "trellis")
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
  expect_s3_class(draw_trellis(plot(vi)), "trellis")
  expect_s3_class(ggplot(vi), "ggplot")
})

test_that("plot.varImp.train collapses a two-column importance table", {
  # some models report one column per class of a two-class problem, holding the
  # same numbers twice; only the first column is plotted
  vi <- varimp_fixture()
  vi$importance <- data.frame(
    one = vi$importance[[1]],
    two = vi$importance[[1]],
    row.names = rownames(vi$importance)
  )

  drawn <- draw_trellis(plot(vi))
  expect_s3_class(drawn, "trellis")
  # a single panel, since the class conditioning is gone
  expect_identical(dim(drawn), 1L)

  # ggplot takes the same route
  expect_s3_class(ggplot2::ggplot(vi), "ggplot")
})

test_that("plot.varImp.train draws signed importances as needles with groups", {
  # pam is the model whose importances carry a sign; the plot splits them into
  # positive and negative groups and draws them with panel.needle()
  vi <- varimp_fixture()
  vi$model <- "pam"
  vi$importance[[1]] <- vi$importance[[1]] - mean(vi$importance[[1]])

  drawn <- plot(vi)
  expect_s3_class(drawn, "trellis")
  # drawing is what runs panel.needle, including its grouped branch
  draw_trellis(drawn)

  # and per-class signed importances, so the panel function sees several panels
  multi <- varimp_fixture(
    formula = Species ~ .,
    data = engine_three_class()
  )
  multi$model <- "pam"
  multi$importance <- multi$importance - 0.5
  draw_trellis(plot(multi))
})

test_that("panel.needle draws vertically as well as horizontally", {
  vi <- varimp_fixture()

  # `horizontal = FALSE` swaps the axes, which is a separate branch of the
  # panel function
  drawn <- dotplot(
    Feature ~ Importance,
    data = data.frame(
      Importance = vi$importance[[1]],
      Feature = factor(rownames(vi$importance))
    ),
    panel = panel.needle,
    horizontal = FALSE
  )
  draw_trellis(drawn)
  expect_s3_class(drawn, "trellis")
})
