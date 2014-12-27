## This file is a cheat to minimize the false positives flagged during R CMD check. such as
## 
##   "bwplot.diff.resamples: no visible binding for global variable ‘Metric’"
##   "bwplot.resamples: no visible binding for global variable ‘Model’"
##   "bwplot.resamples: no visible binding for global variable ‘Metric’"
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

Metric <- Model <- NULL


## densityplot(~ values|Metric, data = plotData, groups = ind,
##             xlab = "", ...)

ind <- NULL

##   avPerf <- ddply(subset(results, Metric == metric[1] & X2 == "Estimate"),
##                   .(Model),
##                   function(x) c(Median = median(x$value, na.rm = TRUE)))

X2 <- NULL

## x[[i]]$resample <- subset(x[[i]]$resample, Variables == x[[i]]$bestSubset)

Variables <- NULL

## calibCalc: no visible binding for global variable ‘obs’
## calibCalc: no visible binding for global variable ‘bin’
## 
## calibCalc <- function(x, class = levels(obs)[1], cuts = 11)
##   {
##     binData <-  data.frame(prob = x$calibProbVar,
##                            bin = cut(x$calibProbVar, (0:cuts)/cuts, include.lowest = TRUE),
##                            class = x$calibClassVar)

obs <- bin <- NULL

##
## checkConditionalX: no visible binding for global variable ‘.outcome’
## checkConditionalX <- function(x, y)
##   {
##     x$.outcome <- y
##     unique(unlist(dlply(x, .(.outcome), zeroVar)))
##   }

.outcome <- NULL

## classLevels.splsda: no visible global function definition for ‘ilevels’
## 
## classLevels.splsda <- function(x, ...)
##   {
##     ## objects from package caret and spls have the
##     ## same class name, but this works for either
##     ilevels(x$y)
##   }

ilevels <- NULL

## looRfeWorkflow: no visible binding for global variable ‘iter’
## looSbfWorkflow: no visible binding for global variable ‘iter’
## looTrainWorkflow: no visible binding for global variable ‘parm’
## looTrainWorkflow: no visible binding for global variable ‘iter’
## nominalRfeWorkflow: no visible binding for global variable ‘iter’
## nominalRfeWorkflow: no visible binding for global variable ‘method’
## nominalRfeWorkflow: no visible binding for global variable ‘Resample’
## nominalSbfWorkflow: no visible binding for global variable ‘dat’
## nominalSbfWorkflow: no visible binding for global variable ‘iter’
## nominalTrainWorkflow: no visible binding for global variable ‘parm’
## nominalTrainWorkflow: no visible binding for global variable ‘iter’
## nominalTrainWorkflow: no visible binding for global variable ‘Resample’
## oobTrainWorkflow: no visible binding for global variable ‘parm’
##
##  result <- foreach(iter = seq(along = resampleIndex),
##                    .combine = "c", .verbose = FALSE,
##                    .packages = "caret", .errorhandling = "stop") %:%
##    foreach(parm = 1:nrow(info$loop), .combine = "c",
##            .verbose = FALSE, .packages = "caret",
##            .errorhandling = "stop") %dopar%
##    {
##

iter <- parm <- method <- Resample <- dat <- NULL

## tuneScheme: no visible binding for global variable ‘.alpha’
## tuneScheme: no visible binding for global variable ‘.phi’
## tuneScheme: no visible binding for global variable ‘.lambda’
##
##  seqParam[[i]] <- data.frame(.lambda = subset(grid,
##                              subset = .phi == loop$.phi[i] &
##                              .lambda < loop$.lambda[i])$.lambda)

.alpha <- .phi <- .lambda <- NULL

##  createGrid : somDims: no visible binding for global variable ‘.xdim’
##  createGrid : somDims: no visible binding for global variable ‘.ydim’
##  createGrid : lvqGrid: no visible binding for global variable ‘.k’
##  createGrid : lvqGrid: no visible binding for global variable ‘.size’
##
##       out <- expand.grid(.xdim = 1:x, .ydim = 2:(x+1),
##                         .xweight = seq(.5, .9, length = len))
## 

.xdim <- .ydim <- .k <- .size <- NULL

##  createModel: possible error in rda(trainX, trainY, gamma =
##    tuneValue$.gamma, lambda = tuneValue$.lambda, ...): unused
##    argument(s) (gamma = tuneValue$.gamma, lambda = tuneValue$.lambda)
##  createModel: no visible global function definition for
##    ‘randomForestNWS’
##  createModel: no visible global function definition for ‘rfLSF’
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

## predictionFunction: no visible binding for global variable ‘.alpha’
##
##  delta <- subset(param, .alpha == uniqueA[i])$.delta
##

.alpha <- NULL

## predictors.gbm: no visible binding for global variable ‘rel.inf’
## predictors.sda: no visible binding for global variable ‘varIndex’
## predictors.smda: no visible binding for global variable ‘varIndex’
##
##    varUsed <- as.character(subset(relImp, rel.inf != 0)$var)

rel.inf <- varIndex <- NULL

## plotClassProbs: no visible binding for global variable ‘Observed’
##
## out <- densityplot(form, data = stackProbs, groups = Observed, ...)

Observed <- NULL

## plot.train: no visible binding for global variable ‘parameter’
##
## paramLabs <- subset(modelInfo, parameter %in% params)$label

parameter <- NULL

## plot.rfe: no visible binding for global variable ‘Selected’
##
## out <- xyplot(plotForm, data = results, groups = Selected, panel =  panel.profile, ...)

Selected <- NULL

## icr.formula: no visible binding for global variable ‘thresh’
##
## res <- icr.default(x, y, weights = w, thresh = thresh, ...)

thresh <- NULL

probValues <- min_prob <- NULL

altTrainWorkflow <- function(x) x

groups <- NULL

trainData <- NULL

j <- NULL

x <- NULL

.B <- NULL

model_id <- player1 <- player2 <- playa <- win1 <- win2 <- name <- NULL

object <- Iter <- lvls <- Mean <- Estimate <- NULL


###################################################################
##

best <- function(x, metric, maximize)
{
  
  bestIter <- if(maximize) which.max(x[,metric])
  else which.min(x[,metric])   
  
  bestIter
}

defaultSummary <- function(data, lev = NULL, model = NULL)
{
  if(is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
  postResample(data[,"pred"], data[,"obs"])
}

twoClassSummary <- function (data, lev = NULL, model = NULL) 
{
  requireNamespaceQuietStop('pROC')
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  rocObject <- try(pROC::roc.default(data$obs, data[, lev[1]]), silent = TRUE)
  rocAUC <- if(class(rocObject)[1] == "try-error") NA else rocObject$auc
  out <- c(rocAUC,
           sensitivity(data[, "pred"], data[, "obs"], lev[1]),
           specificity(data[, "pred"], data[, "obs"], lev[2]))
  names(out) <- c("ROC", "Sens", "Spec")
  out
}


