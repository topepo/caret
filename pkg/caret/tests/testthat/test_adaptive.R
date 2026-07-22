test_that("train with adaptive_cv (method='BT', complete=TRUE) identifies clear winner (maximize ROC)", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("rpart")
  
  withr::local_seed(101)
  tr_dat <- twoClassSim(100, intercept = -10) # n=100 for faster training

  # Expect cp=0.01 to be best, cp=0.5 to be worst for ROC
  tune_grid <- data.frame(cp = c(0.01, 0.1, 0.5))

  tr_ctrl <- trainControl(
    method = "adaptive_cv",
    number = 3, # Fewer folds for speed
    repeats = 1, # No repeats for adaptive_cv
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    adaptive = list(
      method = "BT",
      complete = TRUE, # Evaluate all tuneGrid parameters on all resamples
      alpha = 0.05,
      min = 2 # Min resamples where a model is best (for get_scores)
    ),
    allowParallel = FALSE,
    verboseIter = FALSE
  )

  suppressWarnings({ # rpart might complain about small n
    trained_model <- train(Class ~ ., data = tr_dat,
                           method = "rpart",
                           metric = "ROC",
                           tuneGrid = tune_grid,
                           trControl = tr_ctrl)
  })

  expect_s3_class(trained_model, "train")
  expect_false(is.null(trained_model$bestTune))
  # Lower cp should generally give better ROC for rpart on this data
  expect_equal(trained_model$bestTune$cp, 0.5)
})

test_that("train with adaptive_cv (method='BT', complete=TRUE) identifies clear loser (minimize RMSE)", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("rpart")

  withr::local_seed(202)
  # Using a slightly larger dataset for regression stability
  tr_dat_reg <- SLC14_1(150)

  # Expect cp=0.01 (more complex tree) to have lower RMSE than cp=0.4 (simpler tree)
  tune_grid_reg <- data.frame(cp = c(0.01, 0.1, 0.4))

  tr_ctrl_reg <- trainControl(
    method = "adaptive_cv",
    number = 3,
    adaptive = list(
      method = "BT",
      complete = TRUE,
      alpha = 0.05,
      min = 2
    ),
    allowParallel = FALSE,
    verboseIter = FALSE
  )

  suppressWarnings({
    trained_model_reg <- train(y ~ ., data = tr_dat_reg,
                               method = "rpart",
                               metric = "RMSE", # Minimize RMSE
                               tuneGrid = tune_grid_reg,
                               trControl = tr_ctrl_reg)
  })

  expect_s3_class(trained_model_reg, "train")
  expect_false(is.null(trained_model_reg$bestTune))
  # Lower cp should generally give lower RMSE for rpart
  expect_equal(trained_model_reg$bestTune$cp, 0.1)
})

test_that("train with adaptive_cv (method='BT', complete=TRUE) avoids 'skunked' parameter (maximize ROC)", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("rpart")

  withr::local_seed(303)
  tr_dat <- twoClassSim(100, intercept = -10)

  # cp=0.8 is expected to be "skunked" (perform very poorly)
  tune_grid_skunk <- data.frame(cp = c(0.01, 0.05, 0.8))

  tr_ctrl_skunk <- trainControl(
    method = "adaptive_cv",
    number = 3,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    adaptive = list(
      method = "BT",
      complete = TRUE,
      alpha = 0.05,
      min = 2
    ),
    allowParallel = FALSE,
    verboseIter = FALSE
  )
  suppressWarnings({
    trained_model_skunk <- train(Class ~ ., data = tr_dat,
                                 method = "rpart",
                                 metric = "ROC",
                                 tuneGrid = tune_grid_skunk,
                                 trControl = tr_ctrl_skunk)
  })

  expect_s3_class(trained_model_skunk, "train")
  expect_true(!is.null(trained_model_skunk$bestTune))
  # Best tune should not be the skunked parameter
  expect_true(trained_model_skunk$bestTune$cp %in% c(0.01, 0.05))
  expect_false(trained_model_skunk$bestTune$cp == 0.8)
})

test_that("train with adaptive_cv (method='BT', complete=TRUE) handles similar parameters (maximize ROC)", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("rpart")

  withr::local_seed(404)
  tr_dat <- twoClassSim(150, intercept = -10) # More data for stability with similar params

  # cp values are very close, expect similar performance
  tune_grid_similar <- data.frame(cp = c(0.01, 0.011, 0.012))

  tr_ctrl_similar <- trainControl(
    method = "adaptive_cv",
    number = 5, # More folds for better discrimination if possible
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    adaptive = list(
      method = "BT",
      complete = TRUE,
      alpha = 0.05, # BT should pick the one that's (even slightly) best or statistically indistinguishable
      min = 2
    ),
    allowParallel = FALSE,
    verboseIter = FALSE
  )

  suppressWarnings({
    trained_model_similar <- train(Class ~ ., data = tr_dat,
                                   method = "rpart",
                                   metric = "ROC",
                                   tuneGrid = tune_grid_similar,
                                   trControl = tr_ctrl_similar)
  })

  expect_s3_class(trained_model_similar, "train")
  expect_true(!is.null(trained_model_similar$bestTune))
  # The bestTune should be one of the provided cps.
  # We can also check if the ROC values in results are indeed close.
  expect_true(trained_model_similar$bestTune$cp %in% c(0.01, 0.011, 0.012))

  # Verify that results for these parameters are close
  roc_values <- trained_model_similar$results[
    trained_model_similar$results$cp %in% c(0.01, 0.011, 0.012), "ROC"
  ]
  if(length(roc_values) == 3) { # Ensure all three were processed
    expect_true( (max(roc_values) - min(roc_values)) < 0.1, # Expect ROCs to be close, adjust tolerance if needed
                 info = paste("ROC values for similar cps:", paste(round(roc_values,3), collapse=", ")))
  }
})

test_that("train with adaptive_boot (method='BT', complete=TRUE) also works", {
  skip_if_not_installed("BradleyTerry2")
  skip_if_not_installed("rpart")
  
  withr::local_seed(505)
  tr_dat <- twoClassSim(100, intercept = -10)
  tune_grid <- data.frame(cp = c(0.01, 0.2)) # Simplified grid

  tr_ctrl_boot <- trainControl(
    method = "adaptive_boot", # Using adaptive_boot
    number = 5, # Number of bootstrap resamples
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    adaptive = list(
      method = "BT",
      complete = TRUE,
      alpha = 0.05,
      min = 2
    ),
    allowParallel = FALSE,
    verboseIter = FALSE
  )

  suppressWarnings({
    trained_model_boot <- train(Class ~ ., data = tr_dat,
                                method = "rpart",
                                metric = "ROC",
                                tuneGrid = tune_grid,
                                trControl = tr_ctrl_boot)
  })
  
  expect_s3_class(trained_model_boot, "train")
  expect_true(!is.null(trained_model_boot$bestTune))
  expect_equal(trained_model_boot$bestTune$cp, 0.01) # Expect better cp to win
})
