predict.list <- function(object, ...) {
  out <- lapply(object, predict, ... = ...)
  if(!is.null(names(object))) names(out) <- names(object)
  out
}

predict.train <- function(object, newdata = NULL, type = "raw", na.action = na.omit, ...) {
  if(all(names(object) != "modelInfo")) {
    object <- update(object, param = NULL)
  }
  if(!is.null(object$modelInfo$library))
    for(i in object$modelInfo$library) 
      do.call("require", list(package = i))
  if(!(type %in% c("raw", "prob"))) stop("type must be either \"raw\" or \"prob\"")
  if(type == "prob") {
    if (is.null(object$modelInfo$prob))
      stop("only classification models that produce probabilities are allowed")
  }
  
  if(!is.null(newdata)) {
    if (inherits(object, "train.formula")) {
      newdata <- as.data.frame(newdata)
      rn <- row.names(newdata)
      Terms <- delete.response(object$terms)
      m <- model.frame(Terms, newdata, na.action = na.action, xlev = object$xlevels)
      if (!is.null(cl <- attr(Terms, "dataClasses"))) 
        .checkMFClasses(cl, m)
      keep <- match(row.names(m), rn)
      newdata <- model.matrix(Terms, m, contrasts = object$contrasts)
      xint <- match("(Intercept)", colnames(newdata), nomatch = 0)
      if (xint > 0) 
        newdata <- newdata[, -xint, drop = FALSE]   
    }
  }
  else if(object$control$method != "oob") {
    if(!is.null(object$trainingData)) {            
      if(object$method == "pam") {
        newdata <- object$finalModel$xData 
      } else {
        newdata <- object$trainingData
        newdata$.outcome <- NULL
        if("train.formula" %in% class(object) && 
           any(unlist(lapply(newdata, is.factor)))) {
          newdata <- model.matrix(~., data = newdata)[,-1]
          newdata <- as.data.frame(newdata)
        }
      }
    } else stop("please specify data via newdata")
  }
  
  if(type == "prob") {
    out <- probFunction(method = object$modelInfo, 
                        modelFit = object$finalModel,
                        newdata = newdata, 
                        preProc = object$preProcess)
    obsLevels <- levels(object)
    out <- out[, obsLevels, drop = FALSE]
  } else {
    out <- predictionFunction(method = object$modelInfo, 
                              modelFit = object$finalModel,
                              newdata = newdata, 
                              preProc = object$preProcess)
    if (object$modelType == "Regression") {
      out <- trimPredictions(pred = out,
                             mod_type =object$modelType, 
                             bounds = object$control$predictionBounds, 
                             limits = object$yLimit)
    } else {
      out <- factor(as.character(out), levels = levels(object))
    }
  }
  out  
}
