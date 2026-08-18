# Tests for featurePlot(), which builds a lattice display of the predictors
# against the outcome. The plot type depends on whether the outcome is a factor
# (strip / box / density / pairs / ellipse) or numeric (scatter / pairs). The
# tests check that a trellis object is returned for each type.

test_that("featurePlot builds each classification plot type", {
  x <- iris[, 1:4]
  y <- iris$Species

  expect_s3_class(draw_trellis(featurePlot(x, y, "strip")), "trellis")
  expect_s3_class(draw_trellis(featurePlot(x, y, "box")), "trellis")
  expect_s3_class(draw_trellis(featurePlot(x, y, "density")), "trellis")
  expect_s3_class(draw_trellis(featurePlot(x, y, "pairs")), "trellis")
  # a factor outcome defaults to a strip plot
  expect_s3_class(draw_trellis(featurePlot(x, y)), "trellis")
})

test_that("featurePlot builds the ellipse plot", {
  skip_if_not_installed("ellipse")
  expect_s3_class(
    draw_trellis(featurePlot(iris[, 1:4], iris$Species, "ellipse")),
    "trellis"
  )
})

test_that("featurePlot builds each regression plot type", {
  x <- iris[, 2:4]
  y <- iris$Sepal.Length

  expect_s3_class(draw_trellis(featurePlot(x, y, "scatter")), "trellis")
  expect_s3_class(draw_trellis(featurePlot(x, y, "pairs")), "trellis")
  # a numeric outcome defaults to a scatter plot
  expect_s3_class(draw_trellis(featurePlot(x, y)), "trellis")
})

test_that("featurePlot coerces a matrix of predictors to a data frame", {
  expect_s3_class(
    featurePlot(as.matrix(iris[, 1:4]), iris$Species, "box"),
    "trellis"
  )
})

test_that("featurePlot handles a single predictor", {
  one <- iris[, 1, drop = FALSE]
  # the univariate plot types work with a single feature
  expect_s3_class(
    draw_trellis(featurePlot(one, iris$Species, "strip")),
    "trellis"
  )
  expect_s3_class(
    draw_trellis(featurePlot(one, iris$Species, "density")),
    "trellis"
  )
  expect_s3_class(
    draw_trellis(featurePlot(one, iris$Sepal.Width, "scatter")),
    "trellis"
  )
  # a scatterplot matrix needs at least two predictors
  expect_snapshot(featurePlot(one, iris$Species, "pairs"), error = TRUE)
})
