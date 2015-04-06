context('Test confusionMatrix')
set.seed(442)
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
ref <- predict(fullModel)

dat2 <- as.character(dat)
ref2 <- as.character(ref)
dat2 <- factor(dat2, levels = c("Class2", "Class1"))
ref2 <- factor(ref2, levels = c("Class1", "Class2"))

test_that("Confusion matrix works", {
  cm1 <- confusionMatrix(dat, ref)
  cm2 <- confusionMatrix(dat2, ref2)
  expect_true(class(cm1) == "confusionMatrix")
  expect_true(class(cm2) == "confusionMatrix")
  expect_warning(confusionMatrix(dat2, ref2))
  expect_identical(cm1$overall, cm2$overall)
  expect_false(identical(cm1, cm2))
})