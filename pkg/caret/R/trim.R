trim <- function(object, ...) UseMethod("trim")

#' @export
trim.train <- function(object, ...) {
  removals <- c("results", "pred", "bestTune", "call", "dots",
                "metric", "trainingData", "resample", "resampledCM",
                "perfNames", "maxmimize", "times")
  for(i in removals)
    if(i %in% names(object)) object[i] <- NULL
  c_removals <- c('method', 'number', 'repeats', 'p', 'initialWindow', 
                  'horizon', 'fixedWindow', 'verboseIter', 'returnData', 
                  'returnResamp', 'savePredictions', 'summaryFunction', 
                  'selectionFunction', 'index', 'indexOut', 'indexFinal',
                  'timingSamps', 'trim', 'yLimits')
  for(i in c_removals)
    if(i %in% names(object$control)) object$control[i] <- NULL  
  if(!is.null(object$modelInfo$trim))
    object$finalModel <- object$modelInfo$trim(object$finalModel)
  object
}