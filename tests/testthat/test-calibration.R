# Tests for calibration curves. The computation (calibration.formula / calibCalc)
# is exercised directly and the plot methods are smoke-tested; the shared
# lift_data fixture (a Class ~ prob layout) lives in helper-lift.R.

test_that("calibration requires a formula", {
  expect_snapshot(calibration(lift_data), error = TRUE)
})

test_that("calibration computes a reliability table for one model", {
  cal <- calibration(Class ~ prob1, data = lift_data)
  expect_s3_class(cal, "calibration")
  expect_identical(
    colnames(cal$data),
    c("calibModelVar", "bin", "Percent", "Lower", "Upper", "Count", "midpoint")
  )
})

test_that("calibration handles several models and custom cuts", {
  cal <- calibration(Class ~ prob1 + prob2, data = lift_data)
  expect_identical(
    sort(as.character(unique(cal$data$calibModelVar))),
    c("prob1", "prob2")
  )
  # cuts controls the number of probability bins
  by_count <- calibration(Class ~ prob1, data = lift_data, cuts = 5)
  expect_identical(nrow(by_count$data), 5L)
})

test_that("calibration needs a factor on the left-hand side", {
  expect_snapshot(calibration(prob1 ~ prob2, data = lift_data), error = TRUE)
})

test_that("calibration plot methods build lattice and ggplot objects", {
  cal <- calibration(Class ~ prob1, data = lift_data)
  expect_s3_class(draw_trellis(xyplot(cal)), "trellis")
  expect_s3_class(ggplot(cal), "ggplot")
})

test_that("print.calibration reports the models and event", {
  cal <- calibration(Class ~ prob1, data = lift_data)
  expect_snapshot(print(cal))
})

test_that("the calibration plots handle several models", {
  cal <- calibration(Class ~ prob1 + prob2, data = lift_data)

  # several models are dodged and coloured rather than drawn plainly
  gg <- ggplot(cal)
  expect_s3_class(gg, "ggplot")
  built <- ggplot2::ggplot_build(gg)
  expect_contains(names(built$data[[2]]), "colour")

  # the lattice version groups by model, and plot() is the same as xyplot()
  expect_s3_class(draw_trellis(xyplot(cal)), "trellis")
  expect_s3_class(draw_trellis(plot(cal)), "trellis")
})

test_that("calibration normalises the cut points it is given", {
  # cuts that do not span the unit interval are extended to it, so the first
  # bin starts at zero and the last one ends at one
  cal <- calibration(Class ~ prob1, data = lift_data, cuts = c(0.25, 0.5, 0.75))
  bins <- as.character(cal$data$bin)
  expect_identical(bins[1], "[0,0.25]")
  expect_identical(bins[length(bins)], "(0.75,1]")
})

test_that("the calibration plot passes lattice options through", {
  cal <- calibration(Class ~ prob1, data = lift_data)
  # supplying lattice.options exercises the branch that sets and restores them
  drawn <- xyplot(cal, lattice.options = list(default.theme = list()))
  expect_s3_class(draw_trellis(drawn), "trellis")
})
