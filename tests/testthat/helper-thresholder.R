# Shared fixture builder for test-thresholder.R.
#
# thresholder() operates on a fitted two-class train object, so this returns a
# small knn fit. `...` is passed to trainControl so tests can flip classProbs /
# savePredictions to exercise the validation paths. It is a function (not a
# top-level fit) so nothing runs at load time and the tests can gate it with
# skip_on_cran.
threshold_fit <- function(..., formula = Class ~ ., data = NULL) {
  if (is.null(data)) {
    set.seed(1)
    data <- twoClassSim(200)
  }
  set.seed(2)
  train(
    formula,
    data = data,
    method = "knn",
    tuneGrid = data.frame(k = c(5, 9)),
    trControl = trainControl(method = "cv", number = 3, ...)
  )
}
