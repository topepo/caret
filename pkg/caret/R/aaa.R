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
#' @import methods plyr reshape2 ggplot2 lattice nlme
NULL


.onUnload <- function(libpath) { library.dynam.unload("caret", libpath) }


###################################################################
## Global Variables
###################################################################

if(getRversion() >= "2.15.1"){

  utils::globalVariables(c('Metric', 'Model', 'Num_Resamples'))


  ## densityplot(~ values|Metric, data = plotData, groups = ind,
  ##             xlab = "", ...)

  utils::globalVariables(c('ind'))

  ##   avPerf <- ddply(subset(results, Metric == metric[1] & X2 == "Estimate"),
  ##                   .(Model),
  ##                   function(x) c(Median = median(x$value, na.rm = TRUE)))

  utils::globalVariables(c('X2'))

  ## x[[i]]$resample <- subset(x[[i]]$resample, Variables == x[[i]]$bestSubset)

  utils::globalVariables(c('Variables'))

  ## calibCalc: no visible binding for global variable 'obs'
  ## calibCalc: no visible binding for global variable 'bin'
  ##
  ## calibCalc <- function(x, class = levels(obs)[1], cuts = 11)
  ##   {
  ##     binData <-  data.frame(prob = x$calibProbVar,
  ##                            bin = cut(x$calibProbVar, (0:cuts)/cuts, include.lowest = TRUE),
  ##                            class = x$calibClassVar)

  utils::globalVariables(c('obs', 'bin'))

  ##
  ## checkConditionalX: no visible binding for global variable '.outcome'
  ## checkConditionalX <- function(x, y)
  ##   {
  ##     x$.outcome <- y
  ##     unique(unlist(dlply(x, .(.outcome), zeroVar)))
  ##   }

  utils::globalVariables(c('.outcome'))

  ## classLevels.splsda: no visible global function definition for 'ilevels'
  ##
  ## classLevels.splsda <- function(x, ...)
  ##   {
  ##     ## objects from package caret and spls have the
  ##     ## same class name, but this works for either
  ##     ilevels(x$y)
  ##   }

  utils::globalVariables(c('ilevels'))

  ## looRfeWorkflow: no visible binding for global variable 'iter'
  ## looSbfWorkflow: no visible binding for global variable 'iter'
  ## looTrainWorkflow: no visible binding for global variable 'parm'
  ## looTrainWorkflow: no visible binding for global variable 'iter'
  ## nominalRfeWorkflow: no visible binding for global variable 'iter'
  ## nominalRfeWorkflow: no visible binding for global variable 'method'
  ## nominalRfeWorkflow: no visible binding for global variable 'Resample'
  ## nominalSbfWorkflow: no visible binding for global variable 'dat'
  ## nominalSbfWorkflow: no visible binding for global variable 'iter'
  ## nominalTrainWorkflow: no visible binding for global variable 'parm'
  ## nominalTrainWorkflow: no visible binding for global variable 'iter'
  ## nominalTrainWorkflow: no visible binding for global variable 'Resample'
  ## oobTrainWorkflow: no visible binding for global variable 'parm'
  ##
  ##  result <- foreach(iter = seq(along = resampleIndex),
  ##                    .combine = "c", .verbose = FALSE,
  ##                    .packages = "caret", .errorhandling = "stop") %:%
  ##    foreach(parm = 1:nrow(info$loop), .combine = "c",
  ##            .verbose = FALSE, .packages = "caret",
  ##            .errorhandling = "stop") %dopar%
  ##    {
  ##

  utils::globalVariables(c('iter', 'parm', 'method', 'Resample', 'dat'))

  ## tuneScheme: no visible binding for global variable '.alpha'
  ## tuneScheme: no visible binding for global variable '.phi'
  ## tuneScheme: no visible binding for global variable '.lambda'
  ##
  ##  seqParam[[i]] <- data.frame(.lambda = subset(grid,
  ##                              subset = .phi == loop$.phi[i] &
  ##                              .lambda < loop$.lambda[i])$.lambda)

  utils::globalVariables(c('.alpha', '.phi', '.lambda'))

  ##  createGrid : somDims: no visible binding for global variable '.xdim'
  ##  createGrid : somDims: no visible binding for global variable '.ydim'
  ##  createGrid : lvqGrid: no visible binding for global variable '.k'
  ##  createGrid : lvqGrid: no visible binding for global variable '.size'
  ##
  ##       out <- expand.grid(.xdim = 1:x, .ydim = 2:(x+1),
  ##                         .xweight = seq(.5, .9, length = len))
  ##

  utils::globalVariables(c('.xdim', '.ydim', '.k', '.size'))

  ##  createModel: possible error in rda(trainX, trainY, gamma =
  ##    tuneValue$.gamma, lambda = tuneValue$.lambda, ...): unused
  ##    argument(s) (gamma = tuneValue$.gamma, lambda = tuneValue$.lambda)
  ##  createModel: no visible global function definition for
  ##    'randomForestNWS'
  ##  createModel: no visible global function definition for 'rfLSF'
  ##  createModel: possible error in rvm(as.matrix(trainX), trainY, kernel =
  ##    polydot, kpar = list(degree = tuneValue$.degree, scale =
  ##    tuneValue$.scale, offset = 1), ...): unused argument(s) (kernel =
  ##    polydot, kpar = list(degree = tuneValue$.degree, scale =
  ##    tuneValue$.scale, offset = 1))
  ##  createModel: possible error in rvm(as.matrix(trainX), trainY, kernel =
  ##    rbfdot, kpar = list(sigma = tuneValue$.sigma), ...): unused
  ##    argument(s) (kernel = rbfdot, kpar = list(sigma = tuneValue$.sigma))
  ##  createModel: possible error in rvm(as.matrix(trainX), trainY, kernel =
  ##    vanilladot(), ...): unused argument(s) (kernel = vanilladot())
  ##
  ## ????
  ##
  ## > formals(klaR::rda.default)
  ## $x
  ## <snip>
  ## $gamma
  ## [1] NA
  ##
  ## $lambda
  ## [1] NA

  ## predictionFunction: no visible binding for global variable '.alpha'
  ##
  ##  delta <- subset(param, .alpha == uniqueA[i])$.delta
  ##

  utils::globalVariables(c('.alpha'))

  ## predictors.gbm: no visible binding for global variable 'rel.inf'
  ## predictors.sda: no visible binding for global variable 'varIndex'
  ## predictors.smda: no visible binding for global variable 'varIndex'
  ##
  ##    varUsed <- as.character(subset(relImp, rel.inf != 0)$var)

  utils::globalVariables(c('rel.inf', 'varIndex'))

  ## plotClassProbs: no visible binding for global variable 'Observed'
  ##
  ## out <- densityplot(form, data = stackProbs, groups = Observed, ...)

  utils::globalVariables(c('Observed'))

  ## plot.train: no visible binding for global variable 'parameter'
  ##
  ## paramLabs <- subset(modelInfo, parameter %in% params)$label

  utils::globalVariables(c('parameter'))

  ## plot.rfe: no visible binding for global variable 'Selected'
  ##
  ## out <- xyplot(plotForm, data = results, groups = Selected, panel =  panel.profile, ...)

  utils::globalVariables(c('Selected'))

  ## icr.formula: no visible binding for global variable 'thresh'
  ##
  ## res <- icr.default(x, y, weights = w, thresh = thresh, ...)

  utils::globalVariables(c('thresh', 'probValues', 'min_prob', 'groups', 'trainData', 'j', 'x', '.B'))

  utils::globalVariables(c('model_id', 'player1', 'player2', 'playa', 'win1', 'win2', 'name'))

  utils::globalVariables(c('object', 'Iter', 'lvls', 'Mean', 'Estimate'))


  ## parse_sampling: no visible binding for global variable 'sampling_methods'
  utils::globalVariables(c('sampling_methods'))

  ## ggplot.calibration: no visible binding for global variable 'midpoint'
  ## ggplot.calibration: no visible binding for global variable 'Percent'
  ## ggplot.calibration: no visible binding for global variable 'Lower'
  ## ggplot.calibration: no visible binding for global variable 'Upper'
  utils::globalVariables(c('midpoint', 'Percent', 'Lower', 'Upper'))
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
