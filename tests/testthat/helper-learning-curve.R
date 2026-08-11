# Shared fixture builder for test-learning-curve.R.
#
# learning_curve_dat() fits a model at each training-set proportion, so this
# wraps the (verbose) call with small knn/ROC defaults on a two-class data set.
# `...` is forwarded to learning_curve_dat. It is a function so nothing runs at
# load time and the tests can gate it with skip_on_cran.
learning_curve_fixture <- function(..., verbose = FALSE) {
  set.seed(1)
  dat <- twoClassSim(200)
  set.seed(2)
  learning_curve_dat(
    dat,
    outcome = "Class",
    verbose = verbose,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    metric = "ROC",
    trControl = trainControl(
      method = "cv",
      number = 3,
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    ),
    ...
  )
}
