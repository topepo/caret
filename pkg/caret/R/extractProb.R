 ## TODO use foreach to parallelize
#' @rdname predict.train
#' @importFrom utils flush.console
#' @export
extractProb <- function(models,
                        testX = NULL,
                        testY = NULL,
                        unkX = NULL,
                        unkOnly = !is.null(unkX) & is.null(testX),
                        verbose = FALSE)
{

  objectNames <- names(models)
  if(is.null(objectNames)) objectNames <- paste("Object", 1:length(models), sep = "")


  if(any(unlist(lapply(models, function(x) is.null(x$modelInfo$prob)))))
    stop("only classification models that produce probabilities are allowed")

  obsLevels <- levels(models[[1]])

  if(!unkOnly) {
    trainX <- models[[1]]$trainingData[,!(colnames(models[[1]]$trainingData) %in% ".outcome"), drop = FALSE]
    trainY <- models[[1]]$trainingData$.outcome
  }
  if(verbose)
  {
    cat("Number of training samples:", length(trainY), "\n")
    cat("Number of test samples:    ", length(testY), "\n\n")
  }


  predProb <- predClass <- obs <- modelName <- dataType <- objName <- NULL
  if(!is.null(testX))
  {
    if(!is.data.frame(testX)) testX <- as.data.frame(testX, stringsAsFactors = TRUE)
    hasNa <- apply(testX, 1, function(data) any(is.na(data)))
    if(verbose) cat("There were ", sum(hasNa), "rows with missing values\n\n"); flush.console()
  }

  for(i in seq(along = models))
  {
    if(verbose) cat("starting ", models[[i]]$method, "\n"); flush.console()
    if(!unkOnly) {
      tempTrainProb <- probFunction(models[[i]]$modelInfo,
                                    models[[i]]$finalModel,
                                    trainX,
                                    models[[i]]$preProcess)
      tempTrainPred <- apply(tempTrainProb, 1, which.max)
      tempTrainPred <- colnames(tempTrainProb)[tempTrainPred]
      tempTrainPred <- factor(tempTrainPred, levels = obsLevels)

      if(verbose) cat(models[[i]]$method, ":", length(tempTrainPred), "training predictions were added\n"); flush.console()

      predProb <- if(is.null(predProb)) tempTrainProb else rbind(predProb, tempTrainProb)
      predClass <- c(predClass, as.character(tempTrainPred))
      obs <- c(obs, as.character(trainY))
      modelName <- c(modelName, rep(models[[i]]$method, length(tempTrainPred)))
      objName <- c(objName, rep(objectNames[[i]], length(tempTrainPred)))
      dataType <- c(dataType, rep("Training", length(tempTrainPred)))

      # Test Data
      if(!is.null(testX) & !is.null(testY)) {
        if(!is.data.frame(testX)) testX <- as.data.frame(testX, stringsAsFactors = TRUE)
        tempX <- testX
        tempY <- testY
        tempX$.outcome <- NULL
        tempTestProb <- probFunction(models[[i]]$modelInfo,
                                     models[[i]]$finalModel,
                                     tempX,
                                     models[[i]]$preProcess)
        tempTestPred <- apply(tempTestProb, 1, which.max)
        tempTestPred <- colnames(tempTestProb)[tempTestPred]
        tempTestPred <- factor(tempTestPred, levels = obsLevels)

        if(verbose) cat(models[[i]]$method, ":", length(tempTestPred), "test predictions were added\n")

        predProb <- if(is.null(predProb)) tempTestProb else rbind(predProb, tempTestProb)
        predClass <- c(predClass, as.character(tempTestPred))
        obs <- c(obs, as.character(testY))
        modelName <- c(modelName, rep(models[[i]]$method, length(tempTestPred)))
        objName <- c(objName, rep(objectNames[[i]], length(tempTestPred)))
        dataType <- c(dataType, rep("Test", length(tempTestPred)))
      }

    }

    # Unknown Data
    if(!is.null(unkX))
    {
      if(!is.data.frame(unkX)) unkX <- as.data.frame(unkX, stringsAsFactors = TRUE)
      tempX <- unkX
      tempX$.outcome <- NULL

      tempUnkProb <- probFunction(models[[i]]$modelInfo,
                                  models[[i]]$finalModel,
                                  tempX,
                                  models[[i]]$preProcess)
      tempUnkPred <- apply(tempUnkProb, 1, which.max)
      tempUnkPred <- colnames(tempUnkProb)[tempUnkPred]
      tempUnkPred <- factor(tempUnkPred, levels = obsLevels)

      if(verbose) cat(models[[i]]$method, ":", length(tempUnkPred), "unknown predictions were added\n")

      predProb <- if(is.null(predProb)) tempUnkProb else rbind(predProb, tempUnkProb)
      predClass <- c(predClass, as.character(tempUnkPred))
      obs <- c(obs, rep(NA, length(tempUnkPred)))
      modelName <- c(modelName, rep(models[[i]]$method, length(tempUnkPred)))
      objName <- c(objName, rep(objectNames[[i]], length(tempUnkPred)))
      dataType <- c(dataType, rep("Unknown", length(tempUnkPred)))

    }
    if(verbose) cat("\n")
  }

  predClass <- factor(predClass, levels = obsLevels)
  obs <- factor(obs, levels = obsLevels)

  out <- data.frame(predProb)
  out$obs <- obs
  out$pred <- predClass
  out$model <- modelName
  out$dataType <- dataType
  out$object <- objName
  out
}

