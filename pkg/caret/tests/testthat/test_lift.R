# Create sample data for lift tests
withr::local_seed(456)
n_lift_samples <- 200L
lift_test_data <- data.frame(
  obs = factor(rep(c("Class1", "Class2"), each = n_lift_samples / 2)),
  model1_prob = sort(runif(n_lift_samples, 0, 1), decreasing = TRUE),
  model2_prob = runif(n_lift_samples, 0, 1)
)
# Ensure 'obs' has exactly two levels for lift calculations
lift_test_data$obs <- factor(lift_test_data$obs, levels = c("Class1", "Class2"))

test_that("lift.formula works with a single model", {
  lift_obj <- lift(obs ~ model1_prob, data = lift_test_data)

  expect_s3_class(lift_obj, "lift")
  expect_type(lift_obj, "list")
  expect_named(lift_obj, c("data", "class", "probNames", "pct", "call"))

  expect_s3_class(lift_obj$data, "data.frame")
  expected_cols <- c("liftModelVar", "cuts", "events", "n", "Sn", "Sp",
                     "EventPct", "CumEventPct", "lift", "CumTestedPct")
  expect_true(all(expected_cols %in% names(lift_obj$data)))

  expect_equal(lift_obj$class, levels(lift_test_data$obs)[1])
  expect_equal(lift_obj$probNames, "model1_prob")
  expect_equal(lift_obj$pct, mean(lift_test_data$obs == levels(lift_test_data$obs)[1]) * 100)
  expect_true(nrow(lift_obj$data) > 0)
  expect_equal(unique(lift_obj$data$liftModelVar), "model1_prob")
})

test_that("lift.formula works with multiple models", {
  lift_obj <- lift(obs ~ model1_prob + model2_prob, data = lift_test_data)

  expect_s3_class(lift_obj, "lift")
  expect_equal(length(lift_obj$probNames), 2)
  expect_setequal(lift_obj$probNames, c("model1_prob", "model2_prob"))
  expect_true(nrow(lift_obj$data) > 0)
  expect_setequal(unique(lift_obj$data$liftModelVar), c("model1_prob", "model2_prob"))
  # Each model should have its own set of lift calculations
  expect_equal(
    nrow(lift_obj$data),
    nrow(lift(obs ~ model1_prob, data = lift_test_data)$data) +
    nrow(lift(obs ~ model2_prob, data = lift_test_data)$data)
  )
})

test_that("lift.formula works with specified 'class'", {
  target_class <- levels(lift_test_data$obs)[2]
  lift_obj <- lift(obs ~ model1_prob, data = lift_test_data, class = target_class)

  expect_equal(lift_obj$class, target_class)
  expect_equal(lift_obj$pct, mean(lift_test_data$obs == target_class) * 100)
})

test_that("lift.formula works with 'cuts' as a number", {
  num_cuts <- 10
  lift_obj <- lift(obs ~ model1_prob, data = lift_test_data, cuts = num_cuts)
  expect_equal(nrow(lift_obj$data), num_cuts)
})

test_that("lift.formula works with 'cuts' as a vector", {
  custom_cuts <- c(0.1, 0.5, 0.9)
  lift_obj <- lift(obs ~ model1_prob, data = lift_test_data, cuts = custom_cuts)
  # liftCalc adds 0 and 1 and sorts unique decreasingly. So 0, 0.1, 0.5, 0.9, 1 -> 5 cut points
  expect_equal(nrow(lift_obj$data), length(unique(c(0, 1, custom_cuts))))
})

test_that("lift.formula works with 'cuts = NULL' (default)", {
  lift_obj_null_cuts <- lift(obs ~ model1_prob, data = lift_test_data, cuts = NULL)
  expected_rows <- length(unique(c(0, 1, lift_test_data$model1_prob)))
  expect_equal(nrow(lift_obj_null_cuts$data), expected_rows)
})


test_that("lift.formula works with 'labels'", {
  new_labels <- c(model1_prob = "Model Alpha", model2_prob = "Model Beta")
  lift_obj <- lift(obs ~ model1_prob + model2_prob, data = lift_test_data, labels = new_labels)

  expect_setequal(unique(lift_obj$data$liftModelVar), as.character(new_labels))
  # Check factor levels are ordered as in labels
  expect_equal(levels(lift_obj$data$liftModelVar), as.character(new_labels))
})

test_that("lift.formula throws error with incorrect labels", {
  expect_error(
    lift(obs ~ model1_prob, data = lift_test_data, labels = c(wrong_name = "foo")),
    "labels should be a named vector or list with names: model1_prob"
  )
  expect_error(
    lift(obs ~ model1_prob + model2_prob, data = lift_test_data, labels = c(model1_prob = "foo")),
    "labels should have an element for each term on the rhs of the formula"
  )
})

test_that("lift.formula throws error if LHS is not a factor", {
  bad_data <- lift_test_data
  bad_data$obs <- as.numeric(bad_data$obs)
  expect_error(
    lift(obs ~ model1_prob, data = bad_data),
    "the left-hand side of the formula must be a factor of classes"
  )
})

test_that("ggplot.lift returns a ggplot object", {
  lift_obj_single <- lift(obs ~ model1_prob, data = lift_test_data)
  p <- ggplot(lift_obj_single)
  expect_s3_class(p, "ggplot")

  lift_obj_multi <- lift(obs ~ model1_prob + model2_prob, data = lift_test_data)
  p_multi <- ggplot(lift_obj_multi)
  expect_s3_class(p_multi, "ggplot")
})

test_that("ggplot.lift works with a single model", {
  lift_obj <- lift(obs ~ model1_prob, data = lift_test_data)
  p <- ggplot(lift_obj)

  # Check layers: should have geom_line and geom_point for the model, and a reference line
  expect_length(p$layers, 4L)
  expect_s3_class(p$layers[[1L]]$geom, "GeomSegment")
  expect_s3_class(p$layers[[2L]]$geom, "GeomSegment")
  expect_s3_class(p$layers[[3L]]$geom, "GeomSegment")
  expect_s3_class(p$layers[[4L]]$geom, "GeomLine")
})

test_that("ggplot.lift works with multiple models", {
  lift_obj <- lift(obs ~ model1_prob + model2_prob, data = lift_test_data)
  p <- ggplot(lift_obj)

  # Check layers: 2 models (line + point each) + 1 reference line
  expect_length(p$layers, 4L)
  expect_s3_class(p$layers[[1L]]$geom, "GeomSegment")
  expect_s3_class(p$layers[[2L]]$geom, "GeomSegment")
  expect_s3_class(p$layers[[3L]]$geom, "GeomSegment")
  expect_s3_class(p$layers[[4L]]$geom, "GeomLine")
})

test_that("ggplot.lift plot = 'gain' works", {
  lift_obj <- lift(obs ~ model1_prob, data = lift_test_data)
  p <- ggplot(lift_obj, plot = "gain")

  expect_s3_class(p, "ggplot")
  # Reference line for gain chart is different
  plot_data <- ggplot_build(p)$data
  ref_line_data <- plot_data[[length(plot_data)]] # Last layer is reference
  expect_equal(max(ref_line_data$y), 100, tolerance = 1e-6)
  expect_equal(max(ref_line_data$x), 100, tolerance = 1e-6)
})

test_that("ggplot.lift plot = 'lift' works", {
  lift_obj <- lift(obs ~ model1_prob, data = lift_test_data)
  p <- ggplot(lift_obj, plot = "gain")
  expect_s3_class(p, "ggplot")
  plot_data <- ggplot_build(p)$data
  ref_line_data <- plot_data[[length(plot_data)]] # Last layer is reference
  expect_equal(sum(ref_line_data$y == 100), 102L)
})

test_that("ggplot.lift plot = 'gain' works", {
  lift_obj <- lift(obs ~ model1_prob, data = lift_test_data)
  p <- ggplot(lift_obj, plot = "gain")
  expect_s3_class(p, "ggplot")
  # No reference line for this plot type by default
  line_layers <- Filter(function(l) inherits(l$geom, "GeomLine"), p$layers)
  expect_length(line_layers, 1) # Only the model line
})

test_that("ggplot.lift respects labels from lift object", {
  custom_labels <- c(model1_prob = "Custom Model Name")
  lift_obj_labeled <- lift(obs ~ model1_prob, data = lift_test_data, labels = custom_labels)
  p <- ggplot(lift_obj_labeled)
  plot_data_df <- p$data
  expect_equal(levels(plot_data_df$Model), custom_labels[["model1_prob"]])

  custom_labels_multi <- c(model1_prob = "Alpha", model2_prob = "Beta")
  lift_obj_labeled_multi <- lift(obs ~ model1_prob + model2_prob, data = lift_test_data, labels = custom_labels_multi)
  p_multi <- ggplot(lift_obj_labeled_multi)
  plot_data_df_multi <- p_multi$data
  expect_equal(levels(plot_data_df_multi$Model), unname(custom_labels_multi))
})

test_that("ggplot.lift with 'values' argument", {
  lift_obj <- lift(obs ~ model1_prob, data = lift_test_data)
  # 'values' controls where vertical reference lines are drawn
  p <- ggplot(lift_obj, values = c(25, 50, 75))
  expect_s3_class(p, "ggplot")

  expect_identical(p$layers[[5L]]$data$CumEventPct, c(25, 50, 75))
})
