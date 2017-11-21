#' @rdname predict.train
#' @export
extractPrediction <- function(models, 
                              testX = NULL, 
                              testY = NULL, 
                              unkX = NULL, 
                              unkOnly = !is.null(unkX) & is.null(testX), 
                              verbose = FALSE)
{
  
  objectNames <- names(models)
  if(is.null(objectNames)) objectNames <- paste("Object", 1:length(models), sep = "")
  
  if(!unkOnly) {
    trainX <- models[[1]]$trainingData[,!(colnames(models[[1]]$trainingData) %in% ".outcome"), drop = FALSE]
    trainY <- models[[1]]$trainingData$.outcome  
  }
  obsLevels <- levels(models[[1]])
  
  if(verbose)
  {
    cat("Number of training samples:", length(trainY), "\n")
    cat("Number of test samples:    ", length(testY), "\n\n")
  }
  
  pred <- obs <- modelName <- dataType <- objName <- NULL
  if(!is.null(testX))
  {
    #if(!is.data.frame(testX)) testX <- as.data.frame(testX)
    hasNa <- apply(testX, 1, function(data) any(is.na(data)))
    if(verbose) cat("There were ", sum(hasNa), "rows with missing values\n\n")
  }
  
  for(i in seq(along = models))
  {
    if(!unkOnly)
    {
      tempTrainPred <- predictionFunction(models[[i]]$modelInfo,
                                          models[[i]]$finalModel, 
                                          trainX, 
                                          models[[i]]$preProcess)
      
      if(verbose) cat(models[[i]]$method, ":", length(tempTrainPred), "training predictions were added\n")         
      
      if(models[[i]]$modelType == "Classification")
      {
        pred <- c(pred, as.character(tempTrainPred))
        obs <- c(obs, as.character(trainY))
      } else {
        tempTrainPred <- trimPredictions(mod_type = models[[i]]$modelType,
                                         bounds =  models[[i]]$control$predictionBounds,
                                         limits =  models[[i]]$yLimit,
                                         pred = tempTrainPred)
        pred <- c(pred, tempTrainPred)
        obs <- c(obs, trainY)      
      }
      
      modelName <- c(modelName, rep(models[[i]]$method, length(tempTrainPred)))
      objName <- c(objName, rep(objectNames[[i]], length(tempTrainPred)))
      dataType <- c(dataType, rep("Training", length(tempTrainPred)))
      
      if(!is.null(testX) & !is.null(testY))
      {
        if(any(colnames(testX) == ".outcome")) 
          testX <- testX[, colnames(testX) != ".outcome", drop = FALSE]
        
        tempTestPred <- predictionFunction(models[[i]]$modelInfo,
                                           models[[i]]$finalModel, 
                                           testX, 
                                           models[[i]]$preProcess)              
        
        if(verbose) cat(models[[i]]$method, ":", length(tempTestPred), "test predictions were added\n")         
        
        if(models[[i]]$modelType == "Classification")
        {
          pred <- c(pred, as.character(tempTestPred))
          obs <- c(obs, as.character(testY))    
        } else {
          tempTestPred <- trimPredictions(mod_type = models[[i]]$modelType,
                                          bounds =  models[[i]]$control$predictionBounds,
                                          limits =  models[[i]]$yLimit,
                                          pred = tempTestPred)
          pred <- c(pred, tempTestPred)   
          obs <- c(obs, testY) 
        }
        
        modelName <- c(modelName, rep(models[[i]]$method, length(tempTestPred)))
        objName <- c(objName, rep(objectNames[[i]], length(tempTestPred)))         
        dataType <- c(dataType, rep("Test", length(tempTestPred)))   
        
      }
      if(verbose) cat("\n")
    }
    if(!is.null(unkX))
    {
      if(any(colnames(unkX) == ".outcome")) 
        unkX <- unkX[, colnames(unkX) != ".outcome", drop = FALSE]
      tempUnkPred <- predictionFunction(models[[i]]$modelInfo,
                                        models[[i]]$finalModel, 
                                        unkX, 
                                        models[[i]]$preProcess)     
      
      if(verbose) cat(models[[i]]$method, ":", length(tempUnkPred), "unknown predictions were added\n")         
      
      if(models[[i]]$modelType == "Classification")
      {
        pred <- c(pred, as.character(tempUnkPred))
        obs <- c(obs, rep("", length(tempUnkPred)))    
      } else {
        tempUnkPred <- trimPredictions(mod_type = models[[i]]$modelType,
                                       bounds =  models[[i]]$control$predictionBounds,
                                       limits =  models[[i]]$yLimit,
                                       pred = tempUnkPred)
        pred <- c(pred, tempUnkPred)   
        obs <- c(obs, rep(NA, length(tempUnkPred)))    
      }
      
      modelName <- c(modelName, rep(models[[i]]$method, length(tempUnkPred)))
      objName <- c(objName, rep(objectNames[[i]], length(tempUnkPred)))             
      dataType <- c(dataType, rep("Unknown", length(tempUnkPred)))   
      
    }
    if(verbose) cat("\n")      
  }
  if(models[[1]]$modelType == "Classification")
  {   
    pred <- factor(pred, levels = obsLevels)
    obs <- factor(obs, levels = obsLevels)
  }
  
  data.frame(obs = obs,
             pred = pred,
             model = modelName,
             dataType = dataType,
             object = objName)
}


trimPredictions <- function(pred, mod_type, bounds, limits) {
  if(mod_type == "Regression" && is.logical(bounds) && any(bounds)) {
    if(bounds[1]) pred <- ifelse(pred < limits[1], limits[1], pred)
    if(bounds[2]) pred <- ifelse(pred > limits[2], limits[2], pred)         
  }
  if(mod_type == "Regression" && is.numeric(bounds) && any(!is.na(bounds))) {
    if(!is.na(bounds[1])) pred <- ifelse(pred < bounds[1], bounds[1], pred)
    if(!is.na(bounds[2])) pred <- ifelse(pred > bounds[2], bounds[2], pred)
  }
  pred
}

## This is used in workflows
trim_values <- function(preds, ctrl, is_num) {
  if(is_num) {
    if(is.logical(ctrl$predictionBounds) && any(ctrl$predictionBounds)) {
      if(is.list(preds)) {
        preds <- lapply(preds, trimPredictions,
                        mod_type = "Regression",
                        bounds = ctrl$predictionBounds,
                        limits = ctrl$yLimits)
      } else {
        preds <- trimPredictions(mod_type = "Regression",
                                 bounds =  ctrl$predictionBounds,
                                 limits =  ctrl$yLimit,
                                 pred = preds)
      }
    } else {
      if(is.numeric(ctrl$predictionBounds) && any(!is.na(ctrl$predictionBounds))) {
        if(is.list(preds)) {
          preds <- lapply(preds, trimPredictions,
                          mod_type = "Regression",
                          bounds = ctrl$predictionBounds,
                          limits = ctrl$yLimits)
        } else {
          preds <- trimPredictions(mod_type = "Regression",
                                   bounds =  ctrl$predictionBounds,
                                   limits =  ctrl$yLimit,
                                   pred = preds)
        }
      }
    } 
  }
  preds
}

