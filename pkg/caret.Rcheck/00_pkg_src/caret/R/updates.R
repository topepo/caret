update.train <- function(object, param = NULL, ...)
{
  if(is.null(param)) {
    if(all(names(object) != "modelInfo")) {
      funcs <- try(getModelInfo(object$method)[[1]], silent = TRUE)
      if(class(funcs)[1] == "list" && length(funcs) > 0) {
        funcs$updated <- TRUE
        object$modelInfo <- funcs
        warning(paste("The model was updated to work with the current version of caret.",
                      "Please re-create the model object since future versions will",
                      "require objects to be created from caret versions >= 6.", 
                      "Alternatively, do not update caret beyond version 5.17-7."))
      } else stop(paste("This appears to be from an old version of caret",
                        "and the model type is unknown to the new version"))
    }
  } else {
    ## check for original data
    if(is.null(object$trainingData)) stop("original training data is needed; use returnData = TRUE in trainControl()")
        
    if(is.list(param)) param <- as.data.frame(param)
    dotNames <- hasDots(param, object$modelInfo)
    if(dotNames) colnames(param) <- gsub("^\\.", "", colnames(param))
    
    if(!is.data.frame(param)) stop("param should be a data frame or a named list")
    if(nrow(param) > 1) stop("only one set of parameters should be specified")
    
    paramNames <- as.character(object$modelInfo$parameter$parameter)
    if(length(paramNames) != ncol(param))
      stop(paste("Parameters should be", paste(paramNames, sep = "", collapse = ", ")))
    if(any(sort(names(param)) != sort(paste(paramNames, sep = ""))))
      stop(paste("Parameters should be", paste(paramNames, sep = "", collapse = ", ")))
    
    ## get pre-processing options
    if(!is.null(object$preProcess))
    {
      ppOpt <- list(options = object$preProcess$method)
      if(length(object$control$preProcOptions) > 0) ppOpt <- c(ppOpt,object$control$preProcOptions)
    } else ppOpt <- NULL
    
    ## refit model with new parameters
    args <- list(x = object$trainingData[, colnames(object$trainingData) != ".outcome"],
                 y = object$trainingData$.outcome,
                 method = object$modelInfo, 
                 tuneValue = param, 
                 obsLevels = levels(object$trainingData$.outcome),
                 pp = ppOpt,
                 last = TRUE,
                 classProbs = object$control$classProbs)
    if(any(names(object$trainingData) == ".weights")) {
      args$wts <- object$trainingData$.weights 
    } else args <- c(args, list(wts = NULL))
      
    if(length(object$dots) > 0) args <- c(args, object$dots)
    finalFinalModel <- do.call("createModel", args)
    object$finalModel <- finalFinalModel$fit
    object$preProcess <- finalFinalModel$preProc
    object$bestTune <- param
    
    
    ## modify objects so print method reflects update
    object$update <- param
  }
  ## return object
  object
}
