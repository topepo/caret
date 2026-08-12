# Shared fixture builder for test-lattice-train.R.
#
# The lattice plot methods for train objects need retained per-resample results,
# so this fits a small knn with returnResamp = "all". `...` is forwarded to
# trainControl so tests can switch the resampling method (e.g. LOOCV) to hit the
# error paths. It is a function so nothing runs at load time.
lattice_train_fit <- function(..., tuneLength = 3, data = NULL, method = "cv") {
  if (is.null(data)) {
    set.seed(1)
    data <- twoClassSim(200)
  }
  set.seed(2)
  train(
    Class ~ .,
    data = data,
    method = "knn",
    tuneLength = tuneLength,
    trControl = trainControl(
      method = method,
      number = 5,
      returnResamp = "all",
      classProbs = TRUE,
      ...
    )
  )
}
