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
