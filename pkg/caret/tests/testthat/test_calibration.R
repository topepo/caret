# Create sample data similar to the example
withr::local_seed(123)
n_samples <- 100
test_data <- data.frame(
  obs = factor(c(
    # Ensure there are both levels in the observed data
    "Case", "Control",
    sample(c("Case", "Control"), n_samples - 2L, replace = TRUE)
  )),
  prob1 = runif(n_samples, 0, 1),
  prob2 = runif(n_samples, 0, 1)
)

test_that("calibration.formula works with one model", {
  cal_obj <- calibration(obs ~ prob1, data = test_data, cuts = 5)

  # Check the class and structure of the output
  expect_s3_class(cal_obj, "calibration")
  expect_type(cal_obj, "list")
  expect_named(cal_obj, c("data", "cuts", "class", "probNames", "call"))

  # Check the data frame structure
  expect_s3_class(cal_obj$data, "data.frame")
  expect_true("midpoint" %in% names(cal_obj$data))
  expect_true("Percent" %in% names(cal_obj$data))
  expect_true("Count" %in% names(cal_obj$data))
  expect_true("bin" %in% names(cal_obj$data))
  expect_true("calibModelVar" %in% names(cal_obj$data))
  expect_equal(unique(cal_obj$data$calibModelVar), "prob1")

  # Check cuts and class
  expect_equal(cal_obj$cuts, 5)
  expect_equal(cal_obj$class, levels(test_data$obs)[1]) # Default class

  # Check probNames
  expect_equal(cal_obj$probNames, "prob1")
})

test_that("calibration.formula works with multiple models", {
  cal_obj <- calibration(obs ~ prob1 + prob2, data = test_data, cuts = 10)

  # Check the class and structure of the output
  expect_s3_class(cal_obj, "calibration")
  expect_type(cal_obj, "list")
  expect_named(cal_obj, c("data", "cuts", "class", "probNames", "call"))

  # Check the data frame structure
  expect_s3_class(cal_obj$data, "data.frame")
  expect_true("midpoint" %in% names(cal_obj$data))
  expect_true("Percent" %in% names(cal_obj$data))
  expect_true("Count" %in% names(cal_obj$data))
  expect_true("bin" %in% names(cal_obj$data))
  expect_true("calibModelVar" %in% names(cal_obj$data))
  expect_setequal(unique(cal_obj$data$calibModelVar), c("prob1", "prob2"))

  # Check cuts and class
  expect_equal(cal_obj$cuts, 10)
  expect_equal(cal_obj$class, levels(test_data$obs)[1]) # Default class

  # Check probNames
  expect_equal(cal_obj$probNames, c("prob1", "prob2"))
})

test_that("calibration.formula works with specified class", {
  cal_obj <- calibration(obs ~ prob1, data = test_data, class = "Control", cuts = 5)

  expect_s3_class(cal_obj, "calibration")
  expect_equal(cal_obj$class, "Control")
})

test_that("calibration.formula works with vector cuts", {
  custom_cuts <- c(0, 0.2, 0.5, 0.8, 1)
  cal_obj <- calibration(obs ~ prob1, data = test_data, cuts = custom_cuts)

  expect_s3_class(cal_obj, "calibration")
  # The function adds 0 and 1 if not present and makes them unique
  expect_equal(cal_obj$cuts, unique(c(0, custom_cuts, 1)))
  # Check the number of bins created
  expect_equal(length(levels(cal_obj$data$bin)), length(unique(c(0, custom_cuts, 1))) - 1)
})

test_that("calibration.formula works with subset", {
  subset_data <- test_data[1:50, ]
  cal_obj_subset <- calibration(obs ~ prob1, data = test_data, subset = 1:50, cuts = 5)
  cal_obj_full <- calibration(obs ~ prob1, data = subset_data, cuts = 5)

  # The data in the output should be the same
  expect_equal(cal_obj_subset$data, cal_obj_full$data)
})

test_that("calibration.formula throws error if LHS is not a factor", {
  test_data_numeric_obs <- test_data
  test_data_numeric_obs$obs <- as.numeric(test_data_numeric_obs$obs)

  expect_error(calibration(obs ~ prob1, data = test_data_numeric_obs),
               "the left-hand side of the formula must be a factor of classes")
})

test_that("calibCalc handles cut producing factor levels without data", {
  withr::local_seed(789) # Using a new seed for this specific test
  n_samples_sparse <- 30 # A reasonable number of samples

  # Probabilities are concentrated to ensure some bins are empty.
  # For cuts = 5, the breaks are c(0, 0.2, 0.4, 0.6, 0.8, 1.0).
  # Probabilities between 0.41 and 0.59 will fall into the (0.4, 0.6] bin (midpoint 50).
  prob_values_sparse <- runif(n_samples_sparse, 0.41, 0.59)

  # Ensure 'obs' is a factor with both levels present
  obs_values_sparse <- factor(c(
    "Case", "Control", # Ensure both levels are included
    sample(c("Case", "Control"), n_samples_sparse - 2L, replace = TRUE)
  ))

  test_data_sparse <- data.frame(
    obs = obs_values_sparse,
    prob_val = prob_values_sparse
  )

  # We are interested in "Case" as the positive class
  cal_obj_sparse <- calibration(obs ~ prob_val, data = test_data_sparse, cuts = 5, class = "Case")
  cal_data <- cal_obj_sparse$data

  expect_s3_class(cal_obj_sparse, "calibration")
  expect_s3_class(cal_data, "data.frame")
  expect_equal(nrow(cal_data), 5)
  expect_equal(
    as.character(cal_data$bin),
    c("[0,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,1]")
  )
  expect_equal(cal_data$midpoint, c(10, 30, 50, 70, 90))
  # All data is concentrated in midpoint==50, but the others are retained as NA or 0
  expect_equal(sum(cal_data$Percent == 0), 4L)
  expect_equal(sum(is.na(cal_data$Lower)), 4L)
  expect_equal(sum(is.na(cal_data$Upper)), 4L)
  expect_equal(sum(cal_data$Count == 0), 4L)
  expect_gt(cal_data$Count[cal_data$midpoint == 50], 0)

  expect_equal(cal_obj_sparse$cuts, 5)
  expect_equal(cal_obj_sparse$class, "Case")
  expect_equal(cal_obj_sparse$probNames, "prob_val")
})
