# Shared fixture builder for test-plot-varImp.R.
#
# The varImp plot methods act on a varImp.train object, so this fits a small
# rpart model and returns its variable importance. `formula`/`data` let tests
# use fewer predictors. It is a function so nothing runs at load time.
varimp_fixture <- function(formula = Class ~ ., data = NULL) {
  if (is.null(data)) {
    set.seed(1)
    data <- twoClassSim(150)
  }
  set.seed(2)
  fit <- train(
    formula,
    data = data,
    method = "rpart",
    tuneLength = 3,
    trControl = trainControl(method = "cv", number = 3)
  )
  varImp(fit)
}
