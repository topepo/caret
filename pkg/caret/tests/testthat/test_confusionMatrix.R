context('Test confusionMatrix')

test_that("Confusion matrix works", {
  set.seed(442)
  train <- twoClassSim(n = 1000, intercept = -8, linearVars = 3,
                       noiseVars = 10, corrVars = 4, corrValue = 0.6)

  ctrl <- trainControl(method = "cv", classProbs = TRUE)

  fullModel <- train(Class ~ ., data = train,
                     method = "knn",
                     preProc = c("center", "scale"),
                     tuneLength = 4,
                     trControl = ctrl)
  dat <- train$Class
  ref <- predict(fullModel, newdata = train)
  dat2 <- as.character(dat)
  ref2 <- as.character(ref)
  dat2 <- factor(dat2, levels = c("Class2", "Class1"))
  ref2 <- factor(ref2, levels = c("Class1", "Class2"))
  dat3 <- rep("Class1", length(ref2))
  dat3 <- factor(dat3)
  dat4 <- factor(dat3, levels = c("Class1", "Class4"))
  dat5 <- as.character(dat3)
  dat5[200] <- "Class4"
  dat5 <- factor(dat5, levels = c("Class1", "Class4"))
  cm1 <- confusionMatrix(dat, ref)
  cm2 <- suppressWarnings(confusionMatrix(dat2, ref2))
  cm3 <- suppressWarnings(confusionMatrix(dat3, ref2))
  cm4 <- suppressWarnings(confusionMatrix(dat4, ref2))
  expect_true(inherits(cm1, "confusionMatrix"))
  expect_true(inherits(cm2, "confusionMatrix"))
  expect_true(inherits(cm3, "confusionMatrix"))
  expect_true(inherits(cm4, "confusionMatrix"))
  expect_warning(confusionMatrix(dat2, ref2))
  expect_warning(confusionMatrix(dat3, ref2))
  expect_error(confusionMatrix(dat5, ref2))
  expect_error(confusionMatrix(dat5, ref1))
  expect_identical(cm1$overall, cm2$overall)
  expect_identical(cm4$overall, cm3$overall)
  expect_true(identical(cm1, cm2))
  expect_true(identical(cm3, cm4))

  mat1 <- as.matrix(cm1$table)
  cm11 <- confusionMatrix(mat1)
  expect_equal(cm1, cm11)
})

test_that("confusionMatrix.train works with repeatedcv and different norms", {
  set.seed(442)
  train_data <- twoClassSim(n = 100, intercept = -8, linearVars = 3,
                            noiseVars = 10, corrVars = 4, corrValue = 0.6)

  ctrl <- trainControl(method = "repeatedcv", number = 2, repeats = 1,
                       classProbs = TRUE, savePredictions = TRUE) # savePredictions needed for older caret versions

  knnFit <- train(Class ~ ., data = train_data,
                     method = "knn",
                     preProc = c("center", "scale"),
                     tuneLength = 2,
                     trControl = ctrl)

  # Test with default norm ("overall")
  cm_overall <- confusionMatrix(knnFit)
  expect_s3_class(cm_overall, "confusionMatrix.train")
  expect_equal(dim(cm_overall$table), c(2, 2))
  expect_equal(cm_overall$norm, "overall")

  # Test with norm = "average"
  cm_average <- confusionMatrix(knnFit, norm = "average")
  expect_s3_class(cm_average, "confusionMatrix.train")
  expect_equal(dim(cm_average$table), c(2, 2))
  expect_equal(cm_average$norm, "average")

  # Test with norm = "none"
  cm_none <- confusionMatrix(knnFit, norm = "none")
  expect_s3_class(cm_none, "confusionMatrix.train")
  expect_equal(dim(cm_none$table), c(2, 2))
  expect_equal(cm_none$norm, "none")

  # Test print method
  expect_output(print(cm_overall), "Cross-Validated.*percentual average cell counts")
})

test_that("confusionMatrix.train works with lgocv", {
  set.seed(442)
  train_data <- twoClassSim(n = 100, intercept = -8, linearVars = 3,
                            noiseVars = 10, corrVars = 4, corrValue = 0.6)

  ctrl <- trainControl(method = "lgocv", p = 0.7, number = 3,
                       classProbs = TRUE, savePredictions = TRUE)

  knnFit <- train(Class ~ ., data = train_data,
                  method = "knn",
                  preProc = c("center", "scale"),
                  tuneLength = 2,
                  trControl = ctrl)

  cm_lgocv <- confusionMatrix(knnFit)
  expect_s3_class(cm_lgocv, "confusionMatrix.train")
  expect_equal(dim(cm_lgocv$table), c(2, 2))
  expect_equal(cm_lgocv$norm, "overall")

  # Test print method
  expect_output(print(cm_lgocv), "Repeated Train/Test Splits Estimated")
})

test_that("confusionMatrix.rfe works with cv and different norms", {
  skip_if_not_installed("randomForest")

  set.seed(442)
  # Using iris for simplicity with rfe
  TrainData <- iris[,1:4]
  TrainClasses <- iris[,5]

  rfe_ctrl <- rfeControl(method = "cv", number = 2)

  expect_output({
    rfe_fit <- rfe(TrainData, TrainClasses,
                   sizes = c(2, 3), # Test with 2 and 3 variables
                   rfeControl = rfe_ctrl,
                   metric = "Accuracy",
                   functions = ldaFuncs)
  }, "only.*unique complexity parameters in default grid.*Truncating the grid")

  # Test with default norm ("overall")
  cm_overall <- confusionMatrix(rfe_fit)
  expect_s3_class(cm_overall, "confusionMatrix.rfe")
  expect_equal(dim(cm_overall$table), c(3, 3)) # 3 classes in iris
  expect_equal(cm_overall$norm, "overall")

  # Test with norm = "average"
  cm_average <- confusionMatrix(rfe_fit, norm = "average")
  expect_s3_class(cm_average, "confusionMatrix.rfe")
  expect_equal(dim(cm_average$table), c(3, 3))
  expect_equal(cm_average$norm, "average")

  # Test with norm = "none"
  cm_none <- confusionMatrix(rfe_fit, norm = "none")
  expect_s3_class(cm_none, "confusionMatrix.rfe")
  expect_equal(dim(cm_none$table), c(3, 3))
  expect_equal(cm_none$norm, "none")

  # Test print method
  expect_output(print(cm_overall), "Cross-Validated.*percentual average cell counts")
})

test_that("confusionMatrix.sbf works with cv and different norms", {
  set.seed(442)
  # Using iris for simplicity with sbf
  TrainData <- iris[,1:4]
  TrainClasses <- iris[,5]

  sbf_ctrl <- sbfControl(method = "cv", number = 2,
                         saveDetails = TRUE)

  sbf_fit <- sbf(TrainData, TrainClasses,
                 sbfControl = sbf_ctrl)

  # Test with default norm ("overall")
  cm_overall <- confusionMatrix(sbf_fit)
  expect_s3_class(cm_overall, "confusionMatrix.sbf")
  expect_equal(dim(cm_overall$table), c(3, 3)) # 3 classes in iris
  expect_equal(cm_overall$norm, "overall")

  # Test print method
  expect_output(print(cm_overall), "Cross-Validated.*percentual average cell counts")
})
