#' @param formula A formula of the form `y ~ x1 + x2 + ...`
#' @param data Data frame from which variables specified in `formula` are
#'   preferentially to be taken.
#' @param weights (case) weights for each example - if missing defaults to 1.
#' @param subset An index vector specifying the cases to be used in the
#'   training sample. (NOTE: If given, this argument must be named.)
#' @param na.action A function to specify the action to be taken if `NA`s are
#'   found. The default action is for the procedure to fail. An alternative is
#'   `na.omit`, which leads to rejection of cases with missing values on any
#'   required variable. (NOTE: If given, this argument must be named.)
