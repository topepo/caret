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
