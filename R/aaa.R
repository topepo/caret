## This file is a cheat to minimize the false positives flagged during R CMD check. such as
##
##   "bwplot.diff.resamples: no visible binding for global variable 'Metric'"
##   "bwplot.resamples: no visible binding for global variable 'Model'"
##   "bwplot.resamples: no visible binding for global variable 'Metric'"
##
## when
##
## bwplot.resamples <- function (x, data = NULL, models = x$models, metric = x$metric, ...)
## {
## ...
##   plotData <- subset(plotData, Model %in% models & Metric  %in% metric)
## ...
## }
##
## and other examples.


#' @useDynLib caret
NULL


.onUnload <- function(libpath) { library.dynam.unload("caret", libpath) }


is_cran_check <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}

###################################################################
## Global Variables
###################################################################

#' @importFrom utils globalVariables
if (getRversion() >= "2.15.1") {
  ## Variables flagged by R CMD check as "no visible binding for global
  ## variable" -- false positives from non-standard evaluation (subset(),
  ## foreach(), lattice/ggplot2 aesthetics, etc.). Declaring them here
  ## silences those NOTEs; see the note at the top of this file for an
  ## example.
  # fmt: skip
  global_false_positives <- c(".alpha", ".B", ".k", ".lambda", ".outcome", ".phi", ".size", ".xdim", ".ydim", "bin", "CumEventPct", "CumTestedPct", "cuts", "dat", "Estimate", "groups", "ilevels", "ind", "iter", "Iter", "j", "Lower", "LowerLimit", "lvls", "Mean", "method", "Metric", "midpoint", "min_prob", "Model", "model_id", "name", "Num_Resamples", "object", "obs", "Observed", "parameter", "parm", "Percent", "playa", "player1", "player2", "probValues", "rel.inf", "Resample", "sampling_methods", "Selected", "thresh", "trainData", "Upper", "UpperLimit", "Variables", "varIndex", "win1", "win2", "x", "x1", "x2", "X2", "y1", "y2")
  utils::globalVariables(global_false_positives)
}

###################################################################
## Global Functions
###################################################################
altTrainWorkflow <- function(x) x


#' @export
best <- function(x, metric, maximize)
{

  bestIter <- if(maximize) which.max(x[,metric])
  else which.min(x[,metric])

  bestIter
}

#' @rdname postResample
#' @export
defaultSummary <- function(data, lev = NULL, model = NULL)
{
  if(is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
  postResample(data[,"pred"], data[,"obs"])
}
