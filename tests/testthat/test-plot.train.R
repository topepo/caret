# Tests for plot.train. The function builds lattice objects, so the tests check
# that a trellis object is returned for each plot type (and cover the data-prep
# and error paths); the visual output itself isn't asserted.

test_that("plot.train draws scatter and line plots for a tuned model", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneLength = 4,
    trControl = trainControl(method = "cv", number = 3)
  )

  expect_s3_class(plot(fit, plotType = "scatter"), "trellis")
  expect_s3_class(plot(fit, plotType = "line"), "trellis")
  # an unknown plot type is rejected
  expect_snapshot(plot(fit, plotType = "nope"), error = TRUE)
})

test_that("plot.train draws a level plot for two tuning parameters", {
  skip_on_cran()
  skip_if_not_installed("earth")

  set.seed(1)
  dat <- SLC14_1(80)
  fit <- suppressWarnings(suppressMessages(train(
    y ~ .,
    data = dat,
    method = "earth",
    tuneGrid = expand.grid(nprune = 2:4, degree = 1:2),
    trControl = trainControl(method = "cv", number = 3)
  )))

  expect_s3_class(plot(fit, plotType = "level"), "trellis")
  expect_s3_class(plot(fit, plotType = "scatter"), "trellis")
})

test_that("plot.train errors when no tuning parameter varies", {
  skip_on_cran()

  set.seed(1)
  dat <- SLC14_1(80)
  fit <- train(
    y ~ .,
    data = dat,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )

  expect_snapshot(plot(fit), error = TRUE)
})
