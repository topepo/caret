context("Test ggplot")
skip_if_not_installed("kernlab")

test_that("ggplot.train correctly orders factors", {
  library(caret)
  library(kernlab)
  data(mtcars)
  m <- train(
    mpg ~ cyl + disp,
    data = mtcars,
    method = "svmRadial",
    tuneGrid = expand.grid(C = 1:2, sigma = c(0.0001, 0.01, 1))
  )
  g <- ggplot(m, plotType = "level")

  # Test plot data
  obj_sigma <- as.numeric(levels(g$data$sigma))
  obj_C <- as.numeric(levels(g$data$c))
  expect_equal(obj_sigma, sort(obj_sigma))
  expect_equal(obj_C, sort(obj_C))

  # Test axes' labels on a built plot
  build <- ggplot2::ggplot_build(g)
  obj_x <- as.numeric(build$layout$panel_ranges[[1]]$x.labels)
  obj_y <- as.numeric(build$layout$panel_ranges[[1]]$y.labels)
  expect_equal(obj_x, sort(obj_x))
  expect_equal(obj_y, sort(obj_y))
})

test_that("ggplot.train correctly orders facets' labels", {
  library(caret)
  library(kernlab)
  data(mtcars)
  m <- suppressWarnings(train(
    mpg ~ cyl + disp,
    data = mtcars,
    method = "svmPoly",
    tuneGrid = expand.grid(
      degree = c(0.0001, 0.01, 1),
      scale = c(0.0001, 0.01, 1),
      C = c(0.0001, 0.01, 1)
    )
  ))
  g <- ggplot(m, plotType = "level", nameInStrip = TRUE)

  # Test plot data
  obj_C <- as.numeric(gsub(
    'Cost: ',
    '',
    levels(g$data$C)
  ))
  expect_equal(obj_C, sort(obj_C))

  # Test axes' labels on a built plot
  build <- ggplot2::ggplot_build(g)
  obj_labels <- as.numeric(gsub(
    'Cost: ',
    '',
    levels(build$layout$panel_layout$C)
  ))
  expect_equal(obj_labels, sort(obj_labels))
})
