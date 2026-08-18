# Tests for lift/gain curves. The computation (lift.formula / liftCalc) is
# exercised directly; the lattice plot methods are not tested beyond their
# argument validation. Fixture lift_data lives in helper-lift.R.

# ------------------------------------------------------------------------------

test_that("lift requires a formula", {
  expect_snapshot(lift(lift_data), error = TRUE)
})

test_that("lift computes a gain table for one model", {
  lf <- lift(Class ~ prob1, data = lift_data)
  expect_s3_class(lf, "lift")
  # first factor level ("yes") is the event; half the samples are events
  expect_identical(lf$class, "yes")
  expect_identical(lf$pct, 50)
  expect_identical(lf$probNames, "prob1")
  expect_identical(
    colnames(lf$data),
    c(
      "liftModelVar",
      "cuts",
      "events",
      "n",
      "Sn",
      "Sp",
      "EventPct",
      "CumEventPct",
      "lift",
      "CumTestedPct"
    )
  )
  # prob1 ranks perfectly, so 100% of events are eventually found
  expect_identical(max(lf$data$CumEventPct), 100)
})

test_that("lift handles several models on the right-hand side", {
  lf <- lift(Class ~ prob1 + prob2, data = lift_data)
  expect_identical(lf$probNames, c("prob1", "prob2"))
  expect_identical(
    as.character(sort(unique(lf$data$liftModelVar))),
    c("prob1", "prob2")
  )
})

test_that("lift relabels models and honours custom cuts", {
  # a named labels vector renames the model in the output
  relabelled <- lift(
    Class ~ prob1,
    data = lift_data,
    labels = c(prob1 = "Model A")
  )
  expect_identical(
    as.character(unique(relabelled$data$liftModelVar)),
    "Model A"
  )

  # cuts can be a count or an explicit set of cut-offs
  by_count <- lift(Class ~ prob1, data = lift_data, cuts = 5)
  expect_identical(nrow(by_count$data), 5L)
  by_values <- lift(Class ~ prob1, data = lift_data, cuts = c(0.25, 0.5, 0.75))
  expect_identical(nrow(by_values$data), 5L)
})

test_that("lift validates the formula and labels", {
  # left-hand side must be a factor of classes
  expect_snapshot(lift(prob1 ~ prob2, data = lift_data), error = TRUE)
  # one label is required per right-hand-side term
  expect_snapshot(
    lift(Class ~ prob1, data = lift_data, labels = c("a", "b")),
    error = TRUE
  )
})

test_that("xyplot.lift rejects an unknown plot type", {
  lf <- lift(Class ~ prob1, data = lift_data)
  expect_snapshot(caret:::xyplot.lift(lf, plot = "nope"), error = TRUE)
})

test_that("print.lift reports the models and event rate", {
  lf <- lift(Class ~ prob1, data = lift_data)
  expect_snapshot(print(lf))
})

# ------------------------------------------------------------------------------
# plot methods
#
# The panel functions (panel.lift2, plotRef) only run when the trellis object
# is drawn, so every lattice object here is printed via draw_trellis().

test_that("xyplot.lift draws gain and lift curves", {
  lf <- lift(Class ~ prob1, data = lift_data)

  gain <- xyplot(lf)
  expect_s3_class(draw_trellis(gain), "trellis")

  lift_curve <- xyplot(lf, plot = "lift")
  expect_s3_class(draw_trellis(lift_curve), "trellis")

  # plot() is a thin wrapper around xyplot()
  expect_s3_class(draw_trellis(plot(lf)), "trellis")
})

test_that("xyplot.lift honours supplied axis labels", {
  lf <- lift(Class ~ prob1, data = lift_data)

  labelled <- xyplot(lf, xlab = "tested", ylab = "found")
  expect_s3_class(draw_trellis(labelled), "trellis")

  labelled_lift <- xyplot(lf, plot = "lift", xlab = "cut", ylab = "value")
  expect_s3_class(draw_trellis(labelled_lift), "trellis")
})

test_that("xyplot.lift marks reference points on the gain curve", {
  lf <- lift(Class ~ prob1, data = lift_data)

  # `values` draws a guide at each requested percentage of events found.
  # Interpolating those guides calls approx(), which warns about the tied gain
  # values that every lift curve has.
  with_values <- xyplot(lf, values = c(50, 80))
  expect_s3_class(suppressWarnings(draw_trellis(with_values)), "trellis")
})

test_that("xyplot.lift draws reference points for several models", {
  lf <- lift(Class ~ prob1 + prob2, data = lift_data)
  expect_length(lf$probNames, 2)

  # with more than one model the reference lines use the superpose style
  expect_s3_class(
    suppressWarnings(draw_trellis(xyplot(lf, values = 60))),
    "trellis"
  )
  expect_s3_class(draw_trellis(xyplot(lf, plot = "lift")), "trellis")
})

test_that("the lift plot methods reject an unknown plot type", {
  lf <- lift(Class ~ prob1, data = lift_data)
  expect_snapshot(xyplot(lf, plot = "nope"), error = TRUE)
  expect_snapshot(ggplot(lf, plot = "nope"), error = TRUE)
})

test_that("ggplot.lift builds gain and lift curves", {
  lf <- lift(Class ~ prob1, data = lift_data)

  gain <- ggplot(lf)
  expect_s3_class(gain, "ggplot")
  # building the plot runs the layer computations
  expect_s3_class(ggplot2::ggplot_build(gain), "ggplot_built")

  lift_curve <- ggplot(lf, plot = "lift")
  expect_s3_class(ggplot2::ggplot_build(lift_curve), "ggplot_built")
})

test_that("ggplot.lift adds reference points and handles several models", {
  one <- lift(Class ~ prob1, data = lift_data)
  expect_s3_class(
    suppressWarnings(ggplot2::ggplot_build(ggplot(one, values = c(50, 80)))),
    "ggplot_built"
  )

  two <- lift(Class ~ prob1 + prob2, data = lift_data)
  expect_s3_class(ggplot2::ggplot_build(ggplot(two)), "ggplot_built")
  expect_s3_class(
    suppressWarnings(ggplot2::ggplot_build(ggplot(two, values = 60))),
    "ggplot_built"
  )
  expect_s3_class(
    ggplot2::ggplot_build(ggplot(two, plot = "lift")),
    "ggplot_built"
  )
})

test_that("get_ref_point interpolates the tested percentage for a target", {
  lf <- lift(Class ~ prob1, data = lift_data)

  # approx() warns about the tied gain values inherent to a lift curve
  ref <- suppressWarnings(caret:::get_ref_point(lf$data, v = c(50, 100)))
  expect_named(ref, c("CumEventPct", "CumTestedPct"))
  expect_identical(ref$CumEventPct, c(50, 100))
  # the interpolated positions lie within the observed range
  expect_all_true(!is.na(ref$CumTestedPct))
})
