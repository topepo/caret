test_that("thresholder throws errors for invalid inputs", {
  withr::local_seed(49813)
  dat <- twoClassSim(30L, intercept = -10)
  ctrl <- trainControl(
    method = "cv",
    number = 2, # Use a small number of folds/resamples for speed
    classProbs = TRUE,
    savePredictions = "all",
    summaryFunction = twoClassSummary,
    allowParallel = FALSE # Ensure tests run serially
  )

  train_obj_valid <- suppressWarnings({
    train(Class ~ ., data = dat, method = "glm", metric = "ROC", trControl = ctrl)
  })
  thresholds <- 0.5

  expect_error(thresholder(list(), threshold = thresholds),
               "`x` should be an object of class 'train'")

  train_obj_no_probs <- train_obj_valid
  train_obj_no_probs$control$classProbs <- FALSE
  expect_error(thresholder(train_obj_no_probs, threshold = thresholds),
               "`classProbs` must be TRUE in `trainControl`")

  train_obj_no_save <- train_obj_valid
  train_obj_no_save$control$savePredictions <- FALSE
  expect_error(thresholder(train_obj_no_save, threshold = thresholds),
               "`savePredictions` should be TRUE, 'all', or 'final'")
  
  train_obj_save_none <- train_obj_valid
  train_obj_save_none$control$savePredictions <- "none"
  expect_error(thresholder(train_obj_save_none, threshold = thresholds),
               "`savePredictions` should be TRUE, 'all', or 'final'")

  train_obj_mc <- train_obj_valid
  # Simulate multi-class by altering $pred$obs levels
  # The check is `if (length(levels(x$pred$obs)) > 2)`
  # train() itself would likely error or behave differently for multi-class with twoClassSummary
  # For this unit test, directly modify pred to test thresholder's check
  mc_pred_data <- train_obj_mc$pred
  mc_pred_data$obs <- factor(sample(letters[1:3], nrow(mc_pred_data), replace = TRUE))
  train_obj_mc$pred <- mc_pred_data
  expect_error(thresholder(train_obj_mc, threshold = thresholds),
               "For two class problems only")

  expect_error(thresholder(train_obj_valid, threshold = NULL),
               "Please supply probability threshold values.")
  
  expect_error(thresholder(train_obj_valid, threshold = c(0.5, 1.1)),
               "`threshold` should be on \\[0,1\\]")
  expect_error(thresholder(train_obj_valid, threshold = -0.1),
               "`threshold` should be on \\[0,1\\]")

  expect_error(thresholder(train_obj_valid, threshold = thresholds, statistics = "InvalidStat"),
               "`statistics` should be either 'all', or one or more of")
  expect_error(thresholder(train_obj_valid, threshold = thresholds, statistics = c("all", "Sensitivity")),
               "`statistics` should be either 'all', or one or more of")
})
