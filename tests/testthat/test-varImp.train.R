test_that("high level tests", {
  skip_on_cran()
  data(iris)
  TrainData <- iris[, 1:4]
  TrainClasses <- iris[, 5]

  expect_silent(
    knnFit1 <- train(
      TrainData,
      TrainClasses,
      method = "knn",
      preProcess = c("center", "scale"),
      tuneLength = 10,
      trControl = trainControl(method = "cv")
    )
  )

  expect_silent(vv <- varImp(knnFit1))

  expect_true(ncol(vv$importance) == length(levels(TrainClasses)))
  expect_true(nrow(vv$importance) == ncol(TrainData))
})

test_that("varImp.train can use filter importances from a recipe fit", {
  skip_on_cran()

  rec <- recipes::recipe(Species ~ ., data = iris)
  set.seed(5871)
  fit <- train(
    rec,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3)
  )
  vi <- varImp(fit, useModel = FALSE)
  expect_s3_class(vi, "varImp.train")
  expect_identical(vi$model, "ROC curve")
  expect_setequal(rownames(vi$importance), colnames(iris[, 1:4]))
})

test_that("varImp.train uses absolute importances for pam fits", {
  skip_on_cran()
  skip_if_not_installed("pamr")

  set.seed(408)
  # pamr.train() prints an iteration counter, so capture it
  discarded <- capture.output(
    fit <- train(
      iris[, 1:4],
      iris$Species,
      method = "pam",
      tuneLength = 2,
      trControl = trainControl(method = "cv", number = 2)
    )
  )
  vi <- varImp(fit)
  expect_s3_class(vi, "varImp.train")
  expect_all_true(as.vector(as.matrix(vi$importance)) >= 0)
})

# ------------------------------------------------------------------------------
# print.varImp.train and sortImp, exercised with hand-built objects so the
# snapshots are exact

test_that("print.varImp.train shows the model and importances", {
  vi <- structure(
    list(
      importance = data.frame(
        Overall = c(100, 50, 0),
        row.names = c("a", "b", "c")
      ),
      model = "lm",
      calledFrom = "varImp"
    ),
    class = "varImp.train"
  )
  expect_snapshot(print(vi))
  # a smaller top truncates the listing
  expect_snapshot(print(vi, top = 2))
})

test_that("print.varImp.train collapses two-column importances", {
  vi <- structure(
    list(
      importance = data.frame(
        yes = c(100, 0),
        no = c(100, 0),
        row.names = c("a", "b")
      ),
      model = "ROC curve",
      calledFrom = "varImp"
    ),
    class = "varImp.train"
  )
  expect_snapshot(print(vi))
})

test_that("print.varImp.train sorts multi-class importances by the maximum", {
  vi <- structure(
    list(
      importance = data.frame(
        x = c(10, 100, 20),
        y = c(30, 0, 25),
        z = c(80, 50, 0),
        row.names = c("a", "b", "c")
      ),
      model = "pam",
      calledFrom = "varImp"
    ),
    class = "varImp.train"
  )
  expect_snapshot(print(vi))
})

test_that("sortImp ranks by the maximum for non-varImp callers", {
  obj <- list(
    importance = data.frame(
      Overall = c(5, 20, 10),
      row.names = c("a", "b", "c")
    ),
    calledFrom = "filterVarImp"
  )
  out <- caret:::sortImp(obj, top = 2)
  expect_identical(rownames(out), c("b", "c"))
})
