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
