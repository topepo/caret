"createModel2" <- function(x, y, wts, method, tuneValue, obsLevels, pp = NULL, last = FALSE, classProbs, ...)
  {
    if(!is.null(pp$options))
    {
      pp$method <- pp$options
      pp$options <- NULL
      if("ica" %in% pp$method) pp$n.comp <- pp$ICAcomp
      pp$ICAcomp <- NULL
      pp$x <- x
      ppObj <- do.call("preProcess", pp)
      ppObj$call <- "scrubed"
      x <- predict(ppObj, x)
      rm(pp)
    } else ppObj <- NULL
    modelFit <- method$fit(x = if(!is.data.frame(x)) as.data.frame(x) else x, 
                           y = y, wts = wts, 
                           param  = tuneValue, lev = obsLevels, 
                           last = FALSE, 
                           classProbs = classProbs, ...) 
    ## for models using S4 classes, you can't easily append data, so 
    ## exclude these and we'll use other methods to get this information
    if(!isS4(modelFit))
    {
      modelFit$xNames <- colnames(x)
      modelFit$problemType <- if(is.factor(y)) "Classification" else "Regression"
      modelFit$tuneValue <- tuneValue
      modelFit$obsLevels <- obsLevels
    }
    ## TODO move these to individ models
#     if(!is.null(modelFit) && 
#          any(names(modelFit) == "call" & 
#                !(method %in% c("rpart", "rpart2", "earth", "fda")))) 
#       modelFit$call <- scrubCall(modelFit$call)
#     require(methods)
#     if(isS4(modelFit) && any(slotNames(modelFit) == "call")) modelFit@call <- scrubCall(modelFit@call)
    
    list(fit = modelFit, preProc = ppObj)
  }

"createModel" <-function(x, y, wts, method, tuneValue, obsLevels, pp = NULL, last = FALSE, classProbs, ...)
{
  if(!is.null(pp$options))
  {
    pp$method <- pp$options
    pp$options <- NULL
    if("ica" %in% pp$method) pp$n.comp <- pp$ICAcomp
    pp$ICAcomp <- NULL
    pp$x <- x
    ppObj <- do.call("preProcess", pp)
    ppObj$call <- "scrubed"
    x <- predict(ppObj, x)
    rm(pp)
  } else ppObj <- NULL
  modelFit <- method$fit(x = if(!is.data.frame(x)) as.data.frame(x) else x, 
                         y = y, wts = wts, 
                         param  = tuneValue, lev = obsLevels, 
                         last = FALSE, 
                         classProbs = classProbs, ...) 
  ## for models using S4 classes, you can't easily append data, so 
  ## exclude these and we'll use other methods to get this information
  if(!isS4(modelFit))
  {
    modelFit$xNames <- colnames(x)
    modelFit$problemType <- if(is.factor(y)) "Classification" else "Regression"
    modelFit$tuneValue <- tuneValue
    modelFit$obsLevels <- obsLevels
  }
  
  list(fit = modelFit, preProc = ppObj)
}