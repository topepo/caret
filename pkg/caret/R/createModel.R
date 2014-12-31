
"createModel" <-function(x, y, wts, method, tuneValue, obsLevels, pp = NULL, last = FALSE, classProbs, ...) {
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
  modelFit <- method$fit(x = x, 
                         y = y, wts = wts, 
                         param  = tuneValue, lev = obsLevels, 
                         last = last, 
                         classProbs = classProbs, ...) 
  ## for models using S4 classes, you can't easily append data, so 
  ## exclude these and we'll use other methods to get this information
  if(is.null(method$label)) method$label <- ""
  if(!isS4(modelFit) &
       !(method$label %in% c("Ensemble Partial Least Squares Regression",
                             "Ensemble Partial Least Squares Regression with Feature Selection")))
  {
    modelFit$xNames <- colnames(x)
    modelFit$problemType <- if(is.factor(y)) "Classification" else "Regression"
    modelFit$tuneValue <- tuneValue
    modelFit$obsLevels <- obsLevels
  }
  
  list(fit = modelFit, preProc = ppObj)
}