"train" <-
  function(x, ...){
    UseMethod("train")
  }

train.default <- function(x, y, 
                          method = "rf",
                          preProcess = NULL,
                          ...,
                          weights = NULL,
                          metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
                          maximize = ifelse(metric == "RMSE", FALSE, TRUE),
                          trControl = trainControl(),
                          tuneGrid = NULL,
                          tuneLength = 3)
{
  startTime <- proc.time()
  
  if(is.list(method)) {
    minNames <- c("library", "type", "parameters", "grid",
                  "fit", "predict", "prob")
    nameCheck <- minNames %in% names(method) 
    if(!all(nameCheck)) stop(paste("some required components are missing:",
                                   paste(minNames[!nameCheck], collapse = ", ")))
    models <- method
    method <- "custom"
  } else {
    models <- getModelInfo(method, regex = FALSE)[[1]]
    if (length(models) == 0) 
      stop(paste("Model", method, "is not in caret's built-in library"))
  }
  checkInstall(models$library)
  for(i in seq(along = models$library)) do.call("require", list(package = models$library[i]))
  
  paramNames <- as.character(models$parameters$parameter)
  
  funcCall <- match.call(expand.dots = TRUE)
  modelType <- if(is.factor(y)) "Classification"  else "Regression"
  if(!(modelType %in% models$type)) stop(paste("wrong model type for", tolower(modelType)))

  if(grepl("^svm", method) & grepl("String$", method)) {
    if(is.vector(x) && is.character(x)) {
      stop("'x' should be a character matrix with a single column for string kernel methods")
    }
    if(is.matrix(x) && is.numeric(x)) {
      stop("'x' should be a character matrix with a single column for string kernel methods")
    }
    if(is.data.frame(x)) {
      stop("'x' should be a character matrix with a single column for string kernel methods")
    }
  }
  
  if(any(class(x) == "data.table")) x <- as.data.frame(x)
  stopifnot(length(y) > 1)
  stopifnot(nrow(x) > 1)
  stopifnot(nrow(x) == length(y))
  
  ## TODO add check method and execute here
  
  ## Some models that use RWeka start multiple threads and this conflicts with multicore:
  if(any(search() == "package:doMC") && getDoParRegistered() && "RWeka" %in% models$library)
    warning("Models using Weka will not work with parallel processing with multicore/doMC")
  flush.console()
  
  if(!is.null(preProcess) && !(all(preProcess %in% ppMethods))) 
    stop(paste('pre-processing methods are limited to:', paste(ppMethods, collapse = ", ")))
  if(modelType == "Classification")
  {     
    ## We should get and save the class labels to ensure that predictions are coerced      
    ## to factors that have the same levels as the original data. This is especially 
    ## important with multiclass systems where one or more classes have low sample sizes
    ## relative to the others
    classLevels <- levels(y)
    
    if(trControl$classProbs && any(classLevels != make.names(classLevels)))
    {
      warning(paste("At least one of the class levels are not valid R variables names;",
                    "This may cause errors if class probabilities are generated because",
                    "the variables names will be converted to:",
                    paste(make.names(classLevels), collapse = ", ")))
    }
    
    if(metric %in% c("RMSE", "Rsquared")) 
      stop(paste("Metric", metric, "not applicable for classification models"))
    if(trControl$classProbs)
    {
      if(!is.function(models$prob))
      {
        warning("Class probabilities were requested for a model that does not implement them")
        trControl$classProbs <- FALSE
      }
    }         
  } else {
    if(metric %in% c("Accuracy", "Kappa")) 
      stop(paste("Metric", metric, "not applicable for regression models"))         
    classLevels <- NA
    if(trControl$classProbs)
    {
      warning("cannnot compute class probabilities for regression")
      trControl$classProbs <- FALSE
    }   
  }
  
  if(trControl$method == "oob" & !(method %in% c("rf", "treebag", "cforest", "bagEarth", "bagEarthGCV", "bagFDA","bagFDAGCV", "parRF")))
    stop("for oob error rates, model bust be one of: rf, cforest, bagEarth, bagFDA or treebag")
  
  ## If they don't exist, make the data partitions for the resampling iterations.
  if(is.null(trControl$index)) {
    trControl$index <- switch(tolower(trControl$method),
                              oob = NULL,
                              none = list(seq(along = y)),
                              alt_cv =, cv = createFolds(y, trControl$number, returnTrain = TRUE),
                              repeatedcv =, adaptive_cv = createMultiFolds(y, trControl$number, trControl$repeats),
                              loocv = createFolds(y, length(y), returnTrain = TRUE),
                              boot =, boot632 =,  adaptive_boot = createResample(y, trControl$number),
                              test = createDataPartition(y, 1, trControl$p),
                              adaptive_lgocv =, lgocv = createDataPartition(y, trControl$number, trControl$p),
                              timeslice = createTimeSlices(seq(along = y),
                                                           initialWindow = trControl$initialWindow,
                                                           horizon = trControl$horizon,
                                                           fixedWindow = trControl$fixedWindow)$train,
                              subsemble = subsemble_index(y, V = trControl$number, J = trControl$repeats))
  }
  
  if(trControl$method == "subsemble") {
    if(!trControl$savePredictions) trControl$savePredictions <- TRUE
    trControl$indexOut <- trControl$index$holdout
    trControl$index <- trControl$index$model    
  }
  
  ## Create hold--out indicies
  if(is.null(trControl$indexOut) & trControl$method != "oob"){
    if(tolower(trControl$method) != "timeslice") {      
      trControl$indexOut <- lapply(trControl$index,
                                   function(training, allSamples) allSamples[-unique(training)],
                                   allSamples = seq(along = y))
      names(trControl$indexOut) <- prettySeq(trControl$indexOut)
    } else {
      trControl$indexOut <- createTimeSlices(seq(along = y),
                                             initialWindow = trControl$initialWindow,
                                             horizon = trControl$horizon,
                                             fixedWindow = trControl$fixedWindow)$test
    }
  }
  
  if(trControl$method != "oob" & is.null(trControl$index)) names(trControl$index) <- prettySeq(trControl$index)
  if(trControl$method != "oob" & is.null(names(trControl$index)))    names(trControl$index)    <- prettySeq(trControl$index)
  if(trControl$method != "oob" & is.null(names(trControl$indexOut))) names(trControl$indexOut) <- prettySeq(trControl$indexOut)
  
#   if(!is.data.frame(x)) x <- as.data.frame(x)
  
  ## Gather all the pre-processing info. We will need it to pass into the grid creation
  ## code so that there is a concorance between the data used for modeling and grid creation
  if(!is.null(preProcess))
  {
    ppOpt <- list(options = preProcess)
    if(length(trControl$preProcOptions) > 0) ppOpt <- c(ppOpt,trControl$preProcOptions)
  } else ppOpt <- NULL
  
  ## If no default training grid is specified, get one. We have to pass in the formula
  ## and data for some models (rpart, pam, etc - see manual for more details)
  if(is.null(tuneGrid)) {
    if(!is.null(ppOpt) && length(models$parameters$parameter) > 1 && as.character(models$parameters$parameter) != "parameter") {
      pp <- list(method = ppOpt$options)
      if("ica" %in% pp$method) pp$n.comp <- ppOpt$ICAcomp
      if("pca" %in% pp$method) pp$thresh <- ppOpt$thresh
      if("knnImpute" %in% pp$method) pp$k <- ppOpt$k   
      pp$x <- x
      ppObj <- do.call("preProcess", pp)
      tuneGrid <- models$grid(predict(ppObj, x), y, tuneLength)
      rm(ppObj, pp)
    } else tuneGrid <- models$grid(x, y, tuneLength)
  }
  dotNames <- hasDots(tuneGrid, models)
  if(dotNames) colnames(tuneGrid) <- gsub("^\\.", "", colnames(tuneGrid))
  ## Check tuning parameter names
  tuneNames <- as.character(models$parameters$parameter)
  goodNames <- all.equal(sort(tuneNames), sort(names(tuneGrid)))
  
  if(!is.logical(goodNames) || !goodNames) {
    stop(paste("The tuning parameter grid should have columns",
               paste(tuneNames, collapse = ", ", sep = "")))
  }
  
  if(trControl$method == "none" && nrow(tuneGrid) != 1) 
    stop("Only one model should be specified in tuneGrid with no resampling")

  ## In case prediction bounds are used, compute the limits. For now,
  ## store these in the control object since that gets passed everywhere
  trControl$yLimits <- if(is.numeric(y)) extendrange(y) else NULL


  if(trControl$method != "none") {
    ##------------------------------------------------------------------------------------------------------------------------------------------------------#
    
    ## For each tuning parameter combination, we will loop over them, fit models and generate predictions.
    ## We only save the predictions at this point, not the models (and in the case of method = "oob" we 
    ## only save the prediction summaries at this stage.
    
    ## trainInfo will hold the information about how we should loop to train the model and what types
    ## of parameters are used.
    
    ## There are two types of methods to build the models: "basic" means that each tuning parameter
    ## combination requires it's own model fit and "seq" where a single model fit can be used to
    ## get predictions for multiple tuning parameters.
    
    ## The tuneScheme() function is in miscr.R and it helps define the following:
    ##   - A data frame called "loop" with columns for parameters and a row for each model to be fit.
    ##     For "basic" models, this is the same as the tuning grid. For "seq" models, it is only
    ##     the subset of parameters that need to be fit
    ##   - A list called "submodels". If "basic", it is NULL. For "seq" models, it is a list. Each list
    ##     item is a data frame of the parameters that need to be varied for the corresponding row of
    ##     the loop oject.
    ##
    ## For example, for a gbm model, our tuning grid might be:
    ##    .interaction.depth .n.trees .shrinkage
    ##                     1       50        0.1
    ##                     1      100        0.1
    ##                     2       50        0.1
    ##                     2      100        0.1
    ##                     2      150        0.1
    ##
    ## For this example:
    ## 
    ##   loop:
    ##   .interaction.depth .shrinkage .n.trees
    ##                    1        0.1      100
    ##                    2        0.1      150
    ##
    ##   submodels:
    ##   [[1]]
    ##     .n.trees
    ##           50
    ## 
    ##   [[2]]
    ##     .n.trees
    ##           50
    ##          100
    ## 
    ## A simplified version of predictionFunction() would have the following gbm section:
    ##
    ##     # First get the predicitons with the value of n.trees as given in the current
    ##     # row of loop
    ##     out <- predict(modelFit,
    ##                    newdata,
    ##                    type = "response",
    ##                    n.trees = modelFit$tuneValue$.n.trees)
    ##
    ##     # param is the current value of submodels. In normal predction mode (i.e
    ##     # when using predict.train), param = NULL. When called within train()
    ##     # with this model, it will have the other values for n.trees.
    ##     # In this case, the output of the function is a list of predictions
    ##     # These values are deconvoluted in workerTasks() in misc.R
    ##     if(!is.null(param))
    ##       {
    ##         tmp <- vector(mode = "list", length = nrow(param) + 1)
    ##         tmp[[1]] <- out
    ##         
    ##         for(j in seq(along = param$.n.trees))
    ##           {   
    ##             tmp[[j]]  <- predict(modelFit,
    ##                                  newdata,
    ##                                  type = "response",
    ##                                  n.trees = param$.n.trees[j])
    ##           }
    ##         out <- tmp
    ##
    
    # paramCols <- paste(".", as.character(models$parameters$parameter), sep = "")
    
    if(is.function(models$loop) && nrow(tuneGrid) > 1){
      trainInfo <- models$loop(tuneGrid)
      if(!all(c("loop", "submodels") %in% names(trainInfo))) 
        stop("The 'loop' function should produce a list with elements 'loop' and 'submodels'")
    } else trainInfo <- list(loop = tuneGrid)
    
    
    ## Set or check the seeds when needed
    if(is.null(trControl$seeds))
    {
      seeds <- vector(mode = "list", length = length(trControl$index))
      seeds <- lapply(seeds, function(x) sample.int(n = 1000000, size = nrow(trainInfo$loop)))
      seeds[[length(trControl$index) + 1]] <- sample.int(n = 1000000, size = 1)
      trControl$seeds <- seeds     
    } else {
      if(!(length(trControl$seeds) == 1 && is.na(trControl$seeds)))
      {
        ## check versus number of tasks
        numSeeds <- unlist(lapply(trControl$seeds, length))
        badSeed <- (length(trControl$seeds) < length(trControl$index) + 1) ||
          (any(numSeeds[-length(numSeeds)] < nrow(trainInfo$loop)))
        if(badSeed) stop(paste("Bad seeds: the seed object should be a list of length",
                               length(trControl$index) + 1, "with", 
                               length(trControl$index), "integer vectors of size",
                               nrow(trainInfo$loop), "and the last list element having a",
                               "single integer"))      
      }
    }
    
    
    ## run some data thru the sumamry function and see what we get  
    if(trControl$method == "oob")
    {
      perfNames <- if(modelType == "Regression") c("RMSE", "Rsquared") else  c("Accuracy", "Kappa")    
    } else {
      testSummary <- evalSummaryFunction(y, wts = weights, ctrl = trControl, 
                                         lev = classLevels, metric = metric, 
                                         method = method)
      perfNames <- names(testSummary)
    }
    
    if(!(metric %in% perfNames)){
      oldMetric <- metric
      metric <- perfNames[1]
      warning(paste("The metric \"",
                    oldMetric,
                    "\" was not in ",
                    "the result set. ",
                    metric,
                    " will be used instead.",
                    sep = ""))
    }
    
    if(trControl$method == "oob"){
      tmp <- oobTrainWorkflow(x = x, y = y, wts = weights, 
                              info = trainInfo, method = models,
                              ppOpts = preProcess, ctrl = trControl, lev = classLevels, ...)
      performance <- tmp
    } else {
      if(trControl$method == "LOOCV"){
        tmp <- looTrainWorkflow(x = x, y = y, wts = weights, 
                                info = trainInfo, method = models,
                                ppOpts = preProcess, ctrl = trControl, lev = classLevels, ...)
        performance <- tmp$performance
      } else {
        if(!grepl("adapt", trControl$method)){
          tmp <- nominalTrainWorkflow(x = x, y = y, wts = weights, 
                                      info = trainInfo, method = models,
                                      ppOpts = preProcess, ctrl = trControl, lev = classLevels, ...)
          performance <- tmp$performance
          resampleResults <- tmp$resample
        } else {
          tmp <- adaptiveWorkflow(x = x, y = y, wts = weights, 
                                  info = trainInfo, method = models,
                                  ppOpts = preProcess, 
                                  ctrl = trControl, 
                                  lev = classLevels, 
                                  metric = metric, 
                                  maximize = maximize, 
                                  ...)
          performance <- tmp$performance
          resampleResults <- tmp$resample     
        }
      }
    }
    
    ## TODO we used to give resampled results for LOO
    if(!(trControl$method %in% c("LOOCV", "oob")))
    {
      if(modelType == "Classification" && length(grep("^\\cell", colnames(resampleResults))) > 0)
      {
        resampledCM <- resampleResults[, !(names(resampleResults) %in% perfNames)]
        resampleResults <- resampleResults[, -grep("^\\cell", colnames(resampleResults))]
        #colnames(resampledCM) <- gsub("^\\.", "", colnames(resampledCM))
      } else resampledCM <- NULL
    } else resampledCM <- NULL
    
    if(trControl$verboseIter)
    {
      cat("Aggregating results\n")
      flush.console()
    }
    
    perfCols <- names(performance)
    perfCols <- perfCols[!(perfCols %in% paramNames)]
    
    ## Sort the tuning parameters from least complex to most complex
    if(!is.null(models$sort)) performance <- models$sort(performance)
    
    if(any(is.na(performance[, metric])))
      warning("missing values found in aggregated results")
    
    
    if(trControl$verboseIter && nrow(performance) > 1)
    {
      cat("Selecting tuning parameters\n")
      flush.console()
    }
    
    ## select the optimal set
    selectClass <- class(trControl$selectionFunction)[1]
    
    ## Select the "optimal" tuning parameter.
    if(grepl("adapt", trControl$method)) {
      perf_check <- subset(performance, .B == max(performance$.B))
    } else perf_check <- performance
    
    ## Make adaptive only look at parameters with B = max(B)
    if(selectClass == "function")
    {
      bestIter <- trControl$selectionFunction(x = perf_check,
                                              metric = metric,
                                              maximize = maximize)
    }
    else {
      if(trControl$selectionFunction == "oneSE")
      {
        bestIter <- oneSE(perf_check,
                          metric,
                          length(trControl$index),
                          maximize)
      } else {
        bestIter <- do.call(trControl$selectionFunction,
                            list(x = perf_check,
                                 metric = metric,
                                 maximize = maximize))
      }
    }

    if(is.na(bestIter) || length(bestIter) != 1) stop("final tuning parameters could not be determined")
    
    if(grepl("adapt", trControl$method)) {
      best_perf <- perf_check[bestIter,as.character(models$parameters$parameter),drop = FALSE]
      performance$order <- 1:nrow(performance)
      bestIter <- merge(performance, best_perf)$order
      performance$order <- NULL
    }
    
    
    ## Based on the optimality criterion, select the tuning parameter(s)
    bestTune <- performance[bestIter, paramNames, drop = FALSE]
  } else {
    bestTune <- tuneGrid
    performance <- evalSummaryFunction(y, wts = weights, ctrl = trControl, 
                                       lev = classLevels, metric = metric, 
                                       method = method)
    perfNames <- names(performance)
    performance <- as.data.frame(t(performance))
    performance <- cbind(performance, tuneGrid)
    performance <- performance[-1,,drop = FALSE]
    tmp <- resampledCM <- NULL
  }
  ## Save some or all of the resampling summary metrics
  if(!(trControl$method %in% c("LOOCV", "oob", "none")))
  {
    byResample <- switch(trControl$returnResamp,
                         none = NULL,
                         all = {
                           out <- resampleResults
                           colnames(out) <- gsub("^\\.", "", colnames(out))
                           out
                         },
                         final = {
                           out <- merge(bestTune, resampleResults)
                           out <- out[,!(names(out) %in% names(tuneGrid)), drop = FALSE]
                           out
                         })                        
  } else {
    byResample <- NULL        
  } 
  
  # names(bestTune) <- paste(".", names(bestTune), sep = "")   
  
  ## Reorder rows of performance
  orderList <- list()
  for(i in seq(along = paramNames)) orderList[[i]] <- performance[,paramNames[i]]
  names(orderList) <- paramNames
  performance <- performance[do.call("order", orderList),]      
  
  if(trControl$verboseIter)
  {
    bestText <- paste(paste(names(bestTune), "=",
                            format(bestTune, digits = 3)),
                      collapse = ", ")
    if(nrow(performance) == 1) bestText <- "final model"
    cat("Fitting", bestText, "on full training set\n")
    flush.console()
  }
  
  ## Make the final model based on the tuning results
  
  if(!(length(trControl$seeds) == 1 && is.na(trControl$seeds))) set.seed(trControl$seeds[[length(trControl$seeds)]][1])
  finalTime <- system.time(
    finalModel <- createModel(x = x, y = y, wts = weights, 
                              method = models, 
                              tuneValue = bestTune, 
                              obsLevels = classLevels,
                              pp = ppOpt,
                              last = TRUE,
                              classProbs = trControl$classProbs,
                              ...))
  
  ## get pp info
  pp <- finalModel$preProc
  finalModel <- finalModel$fit
  
  ## Remove this and check for other places it is reference
  ## replaced by tuneValue
  if(method == "pls") finalModel$bestIter <- bestTune
  
  ## To use predict.train and automatically use the optimal lambda,
  ## we need to save it
  if(method == "glmnet") finalModel$lambdaOpt <- bestTune$lambda
  
  if(trControl$returnData) { 
    outData <- if(!is.data.frame(x)) try(as.data.frame(x), silent = TRUE) else x
    if(class(outData)[1] == "try-error") {
      warning("The training data could not be converted to a data frame for saving")
      outData <- NULL
    } else  outData$.outcome <- y
  } else outData <- NULL
  
  ## In the case of pam, the data will need to be saved differently
  if(trControl$returnData & method == "pam")
  {
    finalModel$xData <- x
    finalModel$yData <- y
  }     
  
  endTime <- proc.time()
  times <- list(everything = endTime - startTime,
                final = finalTime)
  
  out <- structure(list(method = method,
                        modelInfo = models,
                        modelType = modelType,
                        results = performance,
                        pred = tmp$predictions,
                        bestTune = bestTune,
                        call = funcCall, 
                        dots = list(...),
                        metric = metric,
                        control = trControl,
                        finalModel = finalModel,
                        preProcess = pp,
                        trainingData = outData,
                        resample = byResample,
                        resampledCM = resampledCM,
                        perfNames = perfNames,
                        maximize = maximize,
                        yLimits = trControl$yLimits,
                        times = times), 
                   class = "train")
   trControl$yLimits <- NULL

  if(trControl$timingSamps > 0) {
    pData <- lapply(x, function(x, n) sample(x, n, replace = TRUE), n = trControl$timingSamps)
    pData <- as.data.frame(pData)
    out$times$prediction <- system.time(predict(out, pData))
  } else  out$times$prediction <- rep(NA, 3)
  out
  
}

train.formula <- function (form, data, ..., weights, subset, na.action = na.fail, contrasts = NULL) 
{
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval.parent(m$data))) m$data <- as.data.frame(data)
  m$... <- m$contrasts <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval.parent(m)
  if(nrow(m) < 1) stop("Every row has at least one missing value were found")
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts, na.action = na.action)
  cons <- attr(x, "contrast")
  xint <- match("(Intercept)", colnames(x), nomatch = 0)
  if (xint > 0)  x <- x[, -xint, drop = FALSE]
  y <- model.response(m)
  w <- as.vector(model.weights(m))
  res <- train(x, y, weights = w, ...)
  res$terms <- Terms
  res$coefnames <- colnames(x)
  res$call <- match.call()
  res$na.action <- attr(m, "na.action")
  res$contrasts <- cons
  res$xlevels <- .getXlevels(Terms, m)
  if(!is.null(res$trainingData))
  {
    res$trainingData <- data
    isY <- names(res$trainingData) %in% as.character(form[[2]])
    if(any(isY)) colnames(res$trainingData)[isY] <- ".outcome"
  }
  class(res) <- c("train", "train.formula")
  res
}

summary.train <- function(object, ...) summary(object$finalModel, ...)
residuals.train <- function(object, ...)
{
  if(object$modelType != "Regression") stop("train() only produces redisuals on numeric outcomes")
  resid <- residuals(object$finalModel, ...)
  if(is.null(resid))
  {    
    if(!is.null(object$trainingData))
    {
      resid <- object$trainingData$.outcome - predict(object, object$trainingData[, names(object$trainingData) != ".outcome",drop = FALSE])
    } else stop("The training data must be saved to produce residuals")
  }
  resid
}

fitted.train <- function(object, ...)
{
  prd <- fitted(object$finalModel)
  if(is.null(prd))
  {    
    if(!is.null(object$trainingData))
    {
      prd <- predict(object, object$trainingData[, names(object$trainingData) != ".outcome",drop = FALSE])
    } else stop("The training data must be saved to produce fitted values")
  }
  prd
  
}
