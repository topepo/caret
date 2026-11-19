# Load necessary data
data(BloodBrain)
data(mdrr)

test_that("bag() works for regression with ctreeBag", {
  skip_if_not_installed("party")

  # Regression example from documentation
  set.seed(123) # for reproducibility
  treebag_control <- bagControl(fit = ctreeBag$fit,
                                predict = ctreeBag$pred,
                                allowParallel = FALSE,
                                aggregate = ctreeBag$aggregate)

  treebag_fit <- bag(bbbDescr, logBBB, B = 10, bagControl = treebag_control)

  # Check the class of the output object
  expect_s3_class(treebag_fit, "bag")

  # Check the number of bagged models
  expect_length(treebag_fit$fits, 10)

  # Check if the fits contain model objects and variable info
  expect_s4_class(treebag_fit$fits[[1]]$fit, "BinaryTree")
  expect_length(treebag_fit$fits[[1]]$vars, 134L)
  expect_identical(head(treebag_fit$fits[[1]]$vars), c(72L, 3L, 64L, 19L, 121L, 109L))
})

test_that("bag() works for classification with ldaBag", {
  # Classification example from documentation
  set.seed(123) # for reproducibility

  # Preprocess data as in the example
  mdrrDescr_processed <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
  mdrrDescr_processed <- mdrrDescr_processed[, -findCorrelation(cor(mdrrDescr_processed), .95)]

  lda_bag_control <- bagControl(fit = ldaBag$fit,
                                predict = ldaBag$pred,
                                allowParallel = FALSE,
                                aggregate = ldaBag$aggregate)

  # Using a smaller number of vars for faster testing
  bagLDA_fit <- bag(mdrrDescr_processed, mdrrClass,
                    B = 5, # Reduced B for faster testing
                    bagControl = lda_bag_control,
                    vars = 50) # Using a subset of variables

  # Check the class of the output object
  expect_s3_class(bagLDA_fit, "bag")

  # Check the number of bagged models
  expect_length(bagLDA_fit$fits, 5)

  # Check if the fits contain model objects and variable info
  expect_s3_class(bagLDA_fit$fits[[1]]$fit, "lda")
  expect_length(bagLDA_fit$fits[[1]]$vars, 50L)
  expect_identical(head(bagLDA_fit$fits[[1]]$vars), c(47L, 78L, 8L, 35L, 55L, 40L))
})

test_that("print.bag() works correctly", {
  skip_if_not_installed("party")

  set.seed(123)
  treebag_control <- bagControl(fit = ctreeBag$fit,
                                predict = ctreeBag$pred,
                                allowParallel = FALSE,
                                aggregate = ctreeBag$aggregate)
  treebag_fit <- bag(bbbDescr, logBBB, B = 3, bagControl = treebag_control) # Reduced B for faster printing

  # Capture the print output
  print_output <- capture.output(print(treebag_fit))

  # Check for expected elements in the output
  expect_true(any(grepl("B: 3", print_output)))
  expect_true(any(grepl("Training data:", print_output)))
  expect_true(any(grepl("variables and", print_output)))
  expect_true(any(grepl("samples", print_output)))
  expect_true(any(grepl("All variables were used in each model", print_output)))
})

test_that("summary.bag() works correctly", {
  set.seed(123)
  # Use a classification example with oob = TRUE for summary stats
  mdrrDescr_processed <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
  mdrrDescr_processed <- mdrrDescr_processed[, -findCorrelation(cor(mdrrDescr_processed), .95)]

  lda_bag_control <- bagControl(fit = ldaBag$fit,
                                predict = ldaBag$pred,
                                allowParallel = FALSE,
                                aggregate = ldaBag$aggregate,
                                oob = TRUE) # Ensure oob stats are computed

  bagLDA_fit <- bag(mdrrDescr_processed, mdrrClass,
                    B = 5, # Reduced B for faster testing
                    bagControl = lda_bag_control,
                    vars = 50)

  # Generate the summary
  bag_summary <- summary(bagLDA_fit)

  # Check the class of the summary object
  expect_s3_class(bag_summary, "summary.bag")
  expect_named(bag_summary, c("oobStat", "call", "B"))
  expect_identical(bag_summary$B, 5L)
  expect_equal(
    bag_summary$oobStat,
    cbind(
      Accuracy = c(
        `  0.0%` = 0.754,
        `  2.5%` = 0.755,
        ` 25.0%` = 0.761,
        ` 50.0%` = 0.777,
        ` 75.0%` = 0.801,
        ` 97.5%` = 0.817,
        `100.0%` = 0.819
      ),
      Kappa = c(0.499, 0.5, 0.508, 0.549, 0.596, 0.635, 0.639)
    ),
    tolerance = 1e-3
  )
})

test_that("print.summary.bag() works correctly", {
  set.seed(123)
  mdrrDescr_processed <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
  mdrrDescr_processed <- mdrrDescr_processed[, -findCorrelation(cor(mdrrDescr_processed), .95)]

  lda_bag_control <- bagControl(fit = ldaBag$fit,
                                predict = ldaBag$pred,
                                allowParallel = FALSE,
                                aggregate = ldaBag$aggregate,
                                oob = TRUE)

  bagLDA_fit <- bag(mdrrDescr_processed, mdrrClass,
                    B = 5,
                    bagControl = lda_bag_control,
                    vars = 50)

  bag_summary <- summary(bagLDA_fit)

  # Capture the print output
  print_output <- capture.output(print(bag_summary))

  # Check for expected elements in the output
  expect_true(any(grepl("Out of bag statistics", print_output)))
  expect_true(any(grepl("B =", print_output)))
})

test_that("predict.bag() works correctly for regression", {
  skip_if_not_installed("party")

  set.seed(123)
  treebag_control <- bagControl(fit = ctreeBag$fit,
                                predict = ctreeBag$pred,
                                allowParallel = FALSE,
                                aggregate = ctreeBag$aggregate)
  treebag_fit <- bag(bbbDescr, logBBB, B = 5, bagControl = treebag_control)

  # Use a subset of the training data as new data for prediction
  newdata <- bbbDescr[1:10, ]

  # Generate predictions
  predictions <- predict(treebag_fit, newdata = newdata)

  # Check the class and dimensions of the predictions
  expect_true(is.numeric(predictions)) # Regression predictions should be numeric
  expect_length(predictions, nrow(newdata))
})

test_that("predict.bag() works correctly for classification", {
  set.seed(123)
  mdrrDescr_processed <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
  mdrrDescr_processed <- mdrrDescr_processed[, -findCorrelation(cor(mdrrDescr_processed), .95)]

  lda_bag_control <- bagControl(fit = ldaBag$fit,
                                predict = ldaBag$pred,
                                allowParallel = FALSE,
                                aggregate = ldaBag$aggregate)

  bagLDA_fit <- bag(mdrrDescr_processed, mdrrClass,
                    B = 5,
                    bagControl = lda_bag_control,
                    vars = 50)

  # Use a subset of the training data as new data for prediction
  newdata <- mdrrDescr_processed[1:10, ]

  # Generate predictions (default type is "class")
  predictions_class <- predict(bagLDA_fit, newdata = newdata)

  # Check the class and dimensions of the class predictions
  expect_true(is.factor(predictions_class))
  expect_length(predictions_class, nrow(newdata))
  expect_equal(levels(predictions_class), levels(mdrrClass))

  # Generate predictions (type is "prob")
  predictions_prob <- predict(bagLDA_fit, newdata = newdata, type = "prob")

  # Check the class and dimensions of the probability predictions
  expect_true(is.data.frame(predictions_prob) || is.matrix(predictions_prob))
  expect_equal(nrow(predictions_prob), nrow(newdata))
  expect_equal(colnames(predictions_prob), levels(mdrrClass))
  expect_true(all(predictions_prob >= 0 & predictions_prob <= 1))
  expect_true(all(abs(rowSums(predictions_prob) - 1) < 1e-6)) # Check that probabilities sum to 1
})
