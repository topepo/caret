# Fit an xgboost model with the model code directly (without train()) and add
# the information that train() would attach to it. By default, the first row of
# the model's default grid is used, with 10 boosting iterations.
xgb_fit <- function(method, x, y, grid = NULL, ...) {
  info <- getModelInfo(method, regex = FALSE)[[1]]
  if (is.null(grid)) {
    grid <- info$grid(x, y, len = 1)[1, , drop = FALSE]
    grid$nrounds <- 10
  }
  if (is.factor(y)) {
    lev <- levels(y)
  } else {
    lev <- NULL
  }
  fit <- info$fit(
    x = x,
    y = y,
    wts = NULL,
    param = grid,
    lev = lev,
    last = TRUE,
    classProbs = TRUE,
    nthread = 1,
    ...
  )
  add_fit_info(
    fit,
    x_names = colnames(x),
    y = y,
    tuneValue = grid,
    obsLevels = lev,
    param = list()
  )
}
