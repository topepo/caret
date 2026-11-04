# Tests for the lattice plot methods on train objects (densityplot / histogram /
# stripplot / xyplot). They build trellis objects from the resampled results, so
# the tests check that a trellis object comes back and snapshot the deterministic
# data-ignored warning and the LOOCV error. Fixture builder lives in
# helper-lattice-train.R.

test_that("the resampling plot methods return trellis objects", {
  skip_on_cran()

  fit <- lattice_train_fit()
  expect_s3_class(draw_trellis(densityplot(fit)), "trellis")
  expect_s3_class(draw_trellis(histogram(fit)), "trellis")
  expect_s3_class(draw_trellis(stripplot(fit)), "trellis")
  expect_s3_class(draw_trellis(xyplot(fit)), "trellis")
  # a non-default metric is accepted
  expect_s3_class(draw_trellis(densityplot(fit, metric = "Kappa")), "trellis")
})

test_that("the plot methods warn when 'data' is supplied", {
  skip_on_cran()

  fit <- lattice_train_fit()
  expect_snapshot(invisible(densityplot(fit, data = iris)))
  expect_snapshot(invisible(histogram(fit, data = iris)))
  expect_snapshot(invisible(stripplot(fit, data = iris)))
  expect_snapshot(invisible(xyplot(fit, data = iris)))
})

test_that("the plot methods reject LOOCV/oob resampling", {
  skip_on_cran()

  fit <- lattice_train_fit(method = "LOOCV", tuneLength = 1)
  expect_snapshot(densityplot(fit), error = TRUE)
  expect_snapshot(histogram(fit), error = TRUE)
  expect_snapshot(stripplot(fit), error = TRUE)
  expect_snapshot(xyplot(fit), error = TRUE)
})

test_that("the resampling plots respect the horizontal option", {
  skip_on_cran()

  dat <- engine_three_class()
  set.seed(4471)
  fit <- train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5, 7)),
    trControl = trainControl(method = "cv", number = 3, returnResamp = "all")
  )

  # `horizontal` swaps which side of the formula the metric goes on
  for (h in c(TRUE, FALSE)) {
    drawn <- stripplot(fit, horizontal = h)
    expect_s3_class(drawn, "trellis")
    draw_trellis(drawn)
  }
})

test_that("the resampling plots need a varying tuning parameter", {
  skip_on_cran()

  reg <- engine_regression(40)
  set.seed(9081)
  # glm has no tuning parameters, so there is nothing to plot the metric against
  fit <- train(
    y ~ .,
    data = reg,
    method = "glm",
    trControl = trainControl(method = "cv", number = 3, returnResamp = "all")
  )

  expect_snapshot(xyplot(fit), error = TRUE)
  # the distribution plots do not need one
  draw_trellis(densityplot(fit))
  draw_trellis(histogram(fit))
})
