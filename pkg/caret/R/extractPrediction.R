

extractPrediction <- function(models, 
                              testX = NULL, 
                              testY = NULL, 
                              unkX = NULL, 
                              unkOnly = !is.null(unkX) & is.null(testX), 
                              verbose = FALSE)
{
  
  objectNames <- names(models)
  if(is.null(objectNames)) objectNames <- paste("Object", 1:length(models), sep = "")
  
  trainX <- models[[1]]$trainingData[,!(names(models[[1]]$trainingData) %in% ".outcome"), drop = FALSE]
  trainY <- models[[1]]$trainingData$.outcome  
  
  obsLevels <- levels(models[[1]])
  
  if(verbose)
  {
    cat("Number of training samples:", length(trainY), "\n")
    cat("Number of test samples:    ", length(testY), "\n\n")
  }
  
  pred <- obs <- modelName <- dataType <- objName <- NULL
  if(!is.null(testX))
  {
    if(!is.data.frame(testX)) testX <- as.data.frame(testX)
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
        tempTrainPred <- trimPredictions(models[[i]], tempTrainPred)
        pred <- c(pred, tempTrainPred)
        obs <- c(obs, trainY)      
      }
      
      modelName <- c(modelName, rep(models[[i]]$method, length(tempTrainPred)))
      objName <- c(objName, rep(objectNames[[i]], length(tempTrainPred)))
      dataType <- c(dataType, rep("Training", length(tempTrainPred)))
      
      if(!is.null(testX) & !is.null(testY))
      {
        if(!is.data.frame(testX)) testX <- as.data.frame(testX)
        tempX <- testX
        tempY <- testY
        tempX$.outcome <- NULL
        
        tempTestPred <- predictionFunction(models[[i]]$modelInfo,
                                           models[[i]]$finalModel, 
                                           tempX, 
                                           models[[i]]$preProcess)              
        
        if(verbose) cat(models[[i]]$method, ":", length(tempTestPred), "test predictions were added\n")         
        
        if(models[[i]]$modelType == "Classification")
        {
          pred <- c(pred, as.character(tempTestPred))
          obs <- c(obs, as.character(tempY))    
        } else {
          tempTestPred <- trimPredictions(models[[i]], tempTestPred)
          pred <- c(pred, tempTestPred)   
          obs <- c(obs, tempY) 
        }
        
        modelName <- c(modelName, rep(models[[i]]$method, length(tempTestPred)))
        objName <- c(objName, rep(objectNames[[i]], length(tempTestPred)))         
        dataType <- c(dataType, rep("Test", length(tempTestPred)))   
        
      }
      if(verbose) cat("\n")
    }
    if(!is.null(unkX))
    {
      if(!is.data.frame(unkX)) unkX <- as.data.frame(unkX)
      tempX <- unkX
      tempX$.outcome <- NULL
      
      tempUnkPred <- predictionFunction(models[[i]]$modelInfo,
                                        models[[i]]$finalModel, 
                                        tempX, 
                                        models[[i]]$preProcess)     
      
      if(verbose) cat(models[[i]]$method, ":", length(tempUnkPred), "unknown predictions were added\n")         
      
      if(models[[i]]$modelType == "Classification")
      {
        pred <- c(pred, as.character(tempUnkPred))
        obs <- c(obs, rep("", length(tempUnkPred)))    
      } else {
        tempUnkPred <- trimPredictions(models[[i]], tempUnkPred)
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


trimPredictions <- function(object, pred)
{
  if(object$modelType == "Regression" &&
       is.logical(object$control$predictionBounds) &&
       any(object$control$predictionBounds))
  {
    if(object$control$predictionBounds[1]) pred <- ifelse(pred < object$yLimit[1], object$yLimit[1], pred)
    if(object$control$predictionBounds[2]) pred <- ifelse(pred > object$yLimit[2], object$yLimit[2], pred)         
  }
  if(object$modelType == "Regression" &&
       is.numeric(object$control$predictionBounds) &&
       any(!is.na(object$control$predictionBounds)))
  {
    if(!is.na(object$control$predictionBounds[1])) pred <- ifelse(pred < object$control$predictionBounds[1], object$control$predictionBounds[1], pred)
    if(!is.na(object$control$predictionBounds[2])) pred <- ifelse(pred > object$control$predictionBounds[2], object$control$predictionBounds[2], pred)
  }
  pred
  
}

