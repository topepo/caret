test_that("ggplot.train correctly orders factors", {
  skip_on_cran()
  skip_if_not_installed("kernlab")
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
  skip_on_cran()
  skip_if_not_installed("kernlab")
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

# ------------------------------------------------------------------------------

test_that("ggplot.train returns a scatter plot and the underlying data", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneLength = 4,
    trControl = trainControl(method = "cv", number = 3)
  )

  expect_s3_class(ggplot(fit, plotType = "scatter"), "ggplot")
  expect_s3_class(ggplot(fit, output = "ggplot"), "ggplot")
  # output = "data" returns the tidied results frame instead of a plot
  expect_s3_class(ggplot(fit, output = "data"), "data.frame")
})

test_that("ggplot.train validates output and needs a varying parameter", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneLength = 4,
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_snapshot(ggplot(fit, output = "nope"), error = TRUE)

  # a model with no varying tuning parameter cannot be plotted
  set.seed(1)
  reg <- train(
    y ~ .,
    data = SLC14_1(80),
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_snapshot(ggplot(reg), error = TRUE)
})

test_that("ggplot.train draws random-search results", {
  skip_on_cran()

  # single tuning parameter
  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneLength = 5,
    trControl = trainControl(method = "cv", number = 3, search = "random")
  )
  expect_s3_class(ggplot(fit), "ggplot")
})

test_that("ggplot.train draws random-search results for several parameters", {
  skip_on_cran()
  skip_if_not_installed("earth")

  # two tuning parameters exercise the faceting path
  set.seed(1)
  fit <- suppressWarnings(suppressMessages(train(
    y ~ .,
    data = SLC14_1(100),
    method = "earth",
    tuneLength = 6,
    trControl = trainControl(method = "cv", number = 3, search = "random")
  )))
  expect_s3_class(ggplot(fit), "ggplot")
})

test_that("ggplot.rfe plots the feature-selection profile", {
  skip_on_cran()

  set.seed(1)
  dat <- twoClassSim(120)
  rf <- rfe(
    dat[, 1:8],
    dat$Class,
    sizes = c(2, 4),
    rfeControl = rfeControl(functions = lrFuncs, method = "cv", number = 3)
  )
  expect_s3_class(ggplot(rf), "ggplot")
})
