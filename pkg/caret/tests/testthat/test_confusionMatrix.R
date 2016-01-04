context('Test confusionMatrix')
set.seed(442)


test_that("Confusion matrix works", {
  library(caret)
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
  cm2 <- confusionMatrix(dat2, ref2)
  cm3 <- confusionMatrix(dat3, ref2)
  cm4 <- confusionMatrix(dat4, ref2)
  expect_true(class(cm1) == "confusionMatrix")
  expect_true(class(cm2) == "confusionMatrix")
  expect_true(class(cm3) == "confusionMatrix")
  expect_true(class(cm4) == "confusionMatrix")
  expect_warning(confusionMatrix(dat2, ref2))
  expect_warning(confusionMatrix(dat3, ref2))
  expect_error(confusionMatrix(dat5, ref2))
  expect_error(confusionMatrix(dat5, ref1))
  expect_identical(cm1$overall, cm2$overall)
  expect_identical(cm4$overall, cm3$overall)
  expect_true(identical(cm1, cm2))
  expect_true(identical(cm3, cm4))
})