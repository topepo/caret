#' @useDynLib caret
NULL

.onUnload <- function(libpath) {
  library.dynam.unload("caret", libpath)
}

is_cran_check <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}

# Used by @examplesIf, and inside examples, so that an example is skipped when
# the suggested packages that it needs are not installed.
has_packages <- function(...) {
  pkgs <- c(...)
  all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))
}

###################################################################
## Global Functions
###################################################################

#' @export
best <- function(x, metric, maximize) {
  if (maximize) {
    bestIter <- which.max(x[, metric])
  } else {
    bestIter <- which.min(x[, metric])
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
