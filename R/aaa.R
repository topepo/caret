#' @useDynLib caret
NULL

.onUnload <- function(libpath) {
  library.dynam.unload("caret", libpath)
}

is_cran_check <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}

###################################################################
## Global Functions
###################################################################
altTrainWorkflow <- function(x) x


#' @export
best <- function(x, metric, maximize) {
  bestIter <- if (maximize) {
    which.max(x[, metric])
  } else {
    which.min(x[, metric])
  }

  bestIter
}

#' @rdname postResample
#' @export
defaultSummary <- function(data, lev = NULL, model = NULL) {
  if (is.character(data$obs)) {
    data$obs <- factor(data$obs, levels = lev)
  }
  postResample(data[, "pred"], data[, "obs"])
}
