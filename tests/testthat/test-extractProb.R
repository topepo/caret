# Tests for extractProb (R/extractProb.R); its class-prediction twin is tested
# in test-extractPrediction.R along with the shared pipeline.

test_that("extractProb reports progress and handles unknowns", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3, classProbs = TRUE)
  )

  te <- iris[1:20, 1:4]
  te$.outcome <- iris$Species[1:20]
  unk <- iris[21:30, 1:4]
  unk$.outcome <- iris$Species[21:30]

  expect_snapshot(
    ep <- extractProb(
      list(fit),
      testX = te,
      testY = iris$Species[1:20],
      unkX = unk,
      verbose = TRUE
    )
  )
  expect_identical(unique(as.character(ep$object)), "Object1")
  expect_setequal(
    unique(as.character(ep$dataType)),
    c("Training", "Test", "Unknown")
  )
  expect_in(levels(iris$Species), colnames(ep))
})

test_that("extractProb handles unknown-only extraction", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3, classProbs = TRUE)
  )

  ep <- extractProb(list(knn = fit), unkX = iris[1:10, 1:4], unkOnly = TRUE)
  expect_identical(unique(as.character(ep$dataType)), "Unknown")
  expect_identical(nrow(ep), 10L)
})

test_that("extractProb converts matrix inputs and stacks several models", {
  skip_on_cran()

  ctrl <- trainControl(method = "cv", number = 3, classProbs = TRUE)
  set.seed(1)
  m1 <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = ctrl
  )
  set.seed(1)
  m2 <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = 7),
    trControl = ctrl
  )

  te <- as.matrix(iris[1:20, 1:4])
  unk <- as.matrix(iris[21:30, 1:4])
  ep <- extractProb(
    list(a = m1, b = m2),
    testX = te,
    testY = iris$Species[1:20],
    unkX = unk
  )
  expect_setequal(unique(as.character(ep$object)), c("a", "b"))
  expect_setequal(
    unique(as.character(ep$dataType)),
    c("Training", "Test", "Unknown")
  )
})
