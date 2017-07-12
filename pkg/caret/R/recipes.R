
## Overall method for recipes
#' @export
train.recipe <- function(recipe,
                         data,
                         method = "rf",
                         ...,
                         metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
                         maximize = ifelse(metric %in% c("RMSE", "logLoss"), FALSE, TRUE),
                         trControl = trainControl(),
                         tuneGrid = NULL,
                         tuneLength = ifelse(trControl$method == "none", 1, 3)) {
  preproc_dots(...)
  
  startTime <- proc.time()
  
  if(trControl$verboseIter)  {
    cat("Preparing recipe\n")
    flush.console()
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # prep and bake recipe on entire training set
  
  trained_rec <- prepare(recipe, training = data, fresh = TRUE, 
                         retain = TRUE,
                         verbose = FALSE, stringsAsFactors = TRUE)
  x <- juice(trained_rec, all_predictors())
  y <- juice(trained_rec, all_outcomes())
  if(ncol(y) > 1) 
    stop("`train` doesn't support multivariate outcomes")
  y <- getElement(y, names(y))
  is_weight <- summary(trained_rec)$role == "case weight"
  if(any(is_weight)) {
    if(sum(is_weight) > 1)
      stop("Ony one column can be used as a case weight.")
    weights <- juice(trained_rec, has_role("case weight"))
    weights <- getElement(weights, names(weights))
  } else weights <- NULL
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if(is.list(method)) {
    minNames <- c("library", "type", "parameters", "grid",
                  "fit", "predict", "prob")
    nameCheck <- minNames %in% names(method)
    if(!all(nameCheck)) stop(paste("some required components are missing:",
                                   paste(minNames[!nameCheck], collapse = ", ")), 
                             call. = FALSE)
    models <- method
    method <- "custom"
  } else {
    models <- getModelInfo(method, regex = FALSE)[[1]]
    if (length(models) == 0)
      stop(paste("Model", method, "is not in caret's built-in library"), call. = FALSE)
  }
  checkInstall(models$library)
  for(i in seq(along = models$library)) 
    do.call("require", list(package = models$library[i]))
  if(any(names(models) == "check") && is.function(models$check)) {
    software_check <- models$check(models$library)
  }
  
  paramNames <- as.character(models$parameters$parameter)
  
  funcCall <- match.call(expand.dots = TRUE)
  modelType <- get_model_type(y)
  if(!(modelType %in% models$type)) 
    stop(paste("wrong model type for", tolower(modelType)), call. = FALSE)
  
  ## RECIPE the rec might produce character `x` so convert if these
  ## models are used? 
  if(grepl("^svm", method) & grepl("String$", method)) {
    if(is.vector(x) && is.character(x)) {
      stop("'x' should be a character matrix with a single column for string kernel methods", 
           call. = FALSE)
    }
    if(is.matrix(x) && is.numeric(x)) {
      stop("'x' should be a character matrix with a single column for string kernel methods", 
           call. = FALSE)
    }
    if(is.data.frame(x)) {
      stop("'x' should be a character matrix with a single column for string kernel methods", 
           call. = FALSE)
    }
  }
  
  if(modelType == "Regression" & length(unique(y)) == 2)
    warning(paste("You are trying to do regression and your outcome only has",
                  "two possible values Are you trying to do classification?",
                  "If so, use a 2 level factor as your outcome column."))
  
  if(modelType != "Classification" & !is.null(trControl$sampling))
    stop("sampling methods are only implemented for classification problems", 
         call. = FALSE)
  if(!is.null(trControl$sampling)) {
    trControl$sampling <- parse_sampling(trControl$sampling)
  }
  
  check_dims(x = x, y = y)
  n <- if(class(y)[1] == "Surv") nrow(y) else length(y)
  
  ## Some models that use RWeka start multiple threads and this conflicts with multicore:
  if(any(search() == "package:doMC") && getDoParRegistered() && "RWeka" %in% models$library)
    warning("Models using Weka will not work with parallel processing with multicore/doMC")
  flush.console()
  
  if(modelType == "Classification") {
    ## We should get and save the class labels to ensure that predictions are coerced
    ## to factors that have the same levels as the original data. This is especially
    ## important with multiclass systems where one or more classes have low sample sizes
    ## relative to the others
    classLevels <- levels(y)
    attributes(classLevels) <- list(ordered = is.ordered(y))
    xtab <- table(y)
    if(any(xtab == 0)) {
      xtab_msg <- paste("'", names(xtab)[xtab == 0], "'", collapse = ", ", sep = "")
      stop(paste("One or more factor levels in the outcome has no data:", xtab_msg), 
           call. = FALSE)
    }
    
    if(trControl$classProbs && any(classLevels != make.names(classLevels))) {
      stop(paste("At least one of the class levels is not a valid R variable name;",
                 "This will cause errors when class probabilities are generated because",
                 "the variables names will be converted to ",
                 paste(make.names(classLevels), collapse = ", "),
                 ". Please use factor levels that can be used as valid R variable names",
                 " (see ?make.names for help)."), call. = FALSE)
    }
    
    if(metric %in% c("RMSE", "Rsquared"))
      stop(paste("Metric", metric, "not applicable for classification models"), 
           call. = FALSE)
    if(!trControl$classProbs && metric == "ROC")
      stop(paste("Class probabilities are needed to score models using the",
                 "area under the ROC curve. Set `classProbs = TRUE`",
                 "in the trainControl() function."), call. = FALSE)
    
    if(trControl$classProbs) {
      if(!is.function(models$prob)) {
        warning("Class probabilities were requested for a model that does not implement them")
        trControl$classProbs <- FALSE
      }
    }
  } else {
    if(metric %in% c("Accuracy", "Kappa"))
      stop(paste("Metric", metric, "not applicable for regression models"), 
           call. = FALSE)
    classLevels <- NA
    if(trControl$classProbs) {
      warning("cannnot compute class probabilities for regression")
      trControl$classProbs <- FALSE
    }
  }
  
  
  if(trControl$method == "oob" & is.null(models$oob))
    stop("Out of bag estimates are not implemented for this model", 
         call. = FALSE)
  
  ## If they don't exist, make the data partitions for the resampling iterations.
  if(is.null(trControl$index)) {
    if(trControl$method == "custom")
      stop("'custom' resampling is appropriate when the `trControl` argument `index` is used", 
           call. = FALSE)
    trControl$index <- switch(tolower(trControl$method),
                              oob = NULL,
                              none = list(seq(along = y)),
                              apparent = list(all = seq(along = y)),
                              alt_cv =, cv = createFolds(y, trControl$number, returnTrain = TRUE),
                              repeatedcv =, adaptive_cv = createMultiFolds(y, trControl$number, trControl$repeats),
                              loocv = createFolds(y, n, returnTrain = TRUE),
                              boot =, boot632 =, optimism_boot =, boot_all =,
                              adaptive_boot = createResample(y, trControl$number),
                              test = createDataPartition(y, 1, trControl$p),
                              adaptive_lgocv =, lgocv = createDataPartition(y, trControl$number, trControl$p),
                              timeslice = createTimeSlices(seq(along = y),
                                                           initialWindow = trControl$initialWindow,
                                                           horizon = trControl$horizon,
                                                           fixedWindow = trControl$fixedWindow,
                                                           skip = trControl$skip)$train,
                              subsemble = subsemble_index(y, V = trControl$number, J = trControl$repeats))
  } else {
    index_types <- unlist(lapply(trControl$index, is.integer))
    if(!isTRUE(all(index_types)))
      stop("`index` should be lists of integers.", call. = FALSE)
    if(!is.null(trControl$indexOut)) {
      index_types <- unlist(lapply(trControl$indexOut, is.integer))
      if(!isTRUE(all(index_types)))
        stop("`indexOut` should be lists of integers.", call. = FALSE)
    }
  }
  
  if(trControl$method == "apparent") trControl$indexOut <- list(all = seq(along = y))
  
  if(trControl$method == "subsemble") {
    if(!trControl$savePredictions) trControl$savePredictions <- TRUE
    trControl$indexOut <- trControl$index$holdout
    trControl$index <- trControl$index$model
  }
  
  if(is.logical(trControl$savePredictions)) {
    trControl$savePredictions <- if(trControl$savePredictions) "all" else "none"
  } else {
    if(!(trControl$savePredictions %in% c("all", "final", "none")))
      stop('`savePredictions` should be either logical or "all", "final" or "none"', call. = FALSE)
  }
  
  ## Create holdout indices
  if(is.null(trControl$indexOut) && trControl$method != "oob"){
    if(tolower(trControl$method) != "timeslice") {
      y_index <- if(class(y)[1] == "Surv") 1:nrow(y) else seq(along = y)
      trControl$indexOut <- lapply(trControl$index, function(training) setdiff(y_index, training))
      if(trControl$method %in% c("optimism_boot", "boot_all")) {
        trControl$indexExtra <- lapply(trControl$index, function(training) {
          list(origIndex = y_index, bootIndex = training)
        })
      }
      names(trControl$indexOut) <- prettySeq(trControl$indexOut)
    } else {
      trControl$indexOut <- createTimeSlices(seq(along = y),
                                             initialWindow = trControl$initialWindow,
                                             horizon = trControl$horizon,
                                             fixedWindow = trControl$fixedWindow,
                                             skip = trControl$skip)$test
    }
  }
  
  if(trControl$method != "oob" & is.null(trControl$index)) 
    names(trControl$index) <- prettySeq(trControl$index)
  if(trControl$method != "oob" & is.null(names(trControl$index)))    
    names(trControl$index) <- prettySeq(trControl$index)
  if(trControl$method != "oob" & is.null(names(trControl$indexOut))) 
    names(trControl$indexOut) <- prettySeq(trControl$indexOut)
  
  if(is.null(tuneGrid)) {
    tuneGrid <- models$grid(x = x, y = y, len = tuneLength, search = trControl$search)
    if (trControl$search != "grid" && tuneLength < nrow(tuneGrid))
      tuneGrid <- tuneGrid[1:tuneLength,,drop = FALSE]
  }
  
  ## Check to make sure that there are tuning parameters in some cases
  if(grepl("adaptive", trControl$method) & nrow(tuneGrid) == 1) {
    stop(paste("For adaptive resampling, there needs to be more than one",
               "tuning parameter for evaluation"), call. = FALSE)
  }
  
  dotNames <- hasDots(tuneGrid, models)
  if(dotNames) colnames(tuneGrid) <- gsub("^\\.", "", colnames(tuneGrid))
  ## Check tuning parameter names
  tuneNames <- as.character(models$parameters$parameter)
  goodNames <- all.equal(sort(tuneNames), sort(names(tuneGrid)))
  
  if(!is.logical(goodNames) || !goodNames) {
    stop(paste("The tuning parameter grid should have columns",
               paste(tuneNames, collapse = ", ", sep = "")), call. = FALSE)
  }
  
  if(trControl$method == "none" && nrow(tuneGrid) != 1)
    stop("Only one model should be specified in tuneGrid with no resampling", call. = FALSE)
  
  ## In case prediction bounds are used, compute the limits. For now,
  ## store these in the control object since that gets passed everywhere
  trControl$yLimits <- if(is.numeric(y)) get_range(y) else NULL  
  
  if(trControl$method != "none") {
    
    if(is.function(models$loop) && nrow(tuneGrid) > 1){
      trainInfo <- models$loop(tuneGrid)
      if(!all(c("loop", "submodels") %in% names(trainInfo)))
        stop("The 'loop' function should produce a list with elements 'loop' and 'submodels'", call. = FALSE)
      lengths <- unlist(lapply(trainInfo$submodels, nrow))
      if(all(lengths == 0)) trainInfo$submodels <- NULL
    } else trainInfo <- list(loop = tuneGrid)
    
    num_rs <- if(trControl$method != "oob") length(trControl$index) else 1L
    if(trControl$method %in% c("boot632", "optimism_boot", "boot_all")) num_rs <- num_rs + 1L
    ## Set or check the seeds when needed
    if(is.null(trControl$seeds) || all(is.na(trControl$seeds)))  {
      seeds <- sample.int(n = 1000000L, size = num_rs * nrow(trainInfo$loop) + 1L)
      seeds <- lapply(seq(from = 1L, to = length(seeds), by = nrow(trainInfo$loop)),
                      function(x) { seeds[x:(x+nrow(trainInfo$loop)-1L)] })
      seeds[[num_rs + 1L]] <- seeds[[num_rs + 1L]][1L]
      trControl$seeds <- seeds
    } else {
      if(!(length(trControl$seeds) == 1 && is.na(trControl$seeds))) {
        ## check versus number of tasks
        numSeeds <- unlist(lapply(trControl$seeds, length))
        badSeed <- (length(trControl$seeds) < num_rs + 1L) ||
          (any(numSeeds[-length(numSeeds)] < nrow(trainInfo$loop))) ||
          (numSeeds[length(numSeeds)] < 1L)
        if(badSeed) stop(paste("Bad seeds: the seed object should be a list of length",
                               num_rs + 1, "with",
                               num_rs, "integer vectors of size",
                               nrow(trainInfo$loop), "and the last list element having at least a",
                               "single integer"), call. = FALSE)
        if(any(is.na(unlist(trControl$seeds)))) stop("At least one seed is missing (NA)", call. = FALSE)
      }
    }
    if(trControl$method == "oob") {
      ## delay this test until later
      perfNames <- metric
    } else {
      ## run some data thru the summary function and see what we get
      testSummary <- evalSummaryFunction(y, 
                                         wts = weights, ctrl = trControl,
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
      tmp <- oob_train_rec(rec = recipe, dat = data,
                           info = trainInfo, method = models,
                           ctrl = trControl, lev = classLevels, ...)
      performance <- tmp
      perfNames <- colnames(performance)
      perfNames <- perfNames[!(perfNames %in% as.character(models$parameters$parameter))]
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
    } else {
      if(trControl$method == "LOOCV"){
        tmp <- loo_train_rec(rec = recipe, dat = data,
                             info = trainInfo, method = models,
                             ctrl = trControl, lev = classLevels, ...)
        performance <- tmp$performance
      } else {
        if(!grepl("adapt", trControl$method)){
          tmp <- train_rec(rec = recipe, dat = data,
                           info = trainInfo, method = models,
                           ctrl = trControl, lev = classLevels, ...)
          performance <- tmp$performance
          resampleResults <- tmp$resample
        } else {
          tmp <- train_adapt_rec(rec = recipe, dat = data,
                                 info = trainInfo, 
                                 method = models,
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
    
    ## Remove extra indices
    trControl$indexExtra <- NULL
    
    if(!(trControl$method %in% c("LOOCV", "oob"))) {
      if(modelType == "Classification" && length(grep("^\\cell", colnames(resampleResults))) > 0) {
        resampledCM <- resampleResults[, !(names(resampleResults) %in% perfNames)]
        resampleResults <- resampleResults[, -grep("^\\cell", colnames(resampleResults))]
        #colnames(resampledCM) <- gsub("^\\.", "", colnames(resampledCM))
      } else resampledCM <- NULL
    } else resampledCM <- NULL
    
    
    if(trControl$verboseIter)  {
      cat("Aggregating results\n")
      flush.console()
    }
    
    perfCols <- names(performance)
    perfCols <- perfCols[!(perfCols %in% paramNames)]
    
    if(all(is.na(performance[, metric]))) {
      cat(paste("Something is wrong; all the", metric, "metric values are missing:\n"))
      print(summary(performance[, perfCols[!grepl("SD$", perfCols)], drop = FALSE]))
      stop("Stopping", call. = FALSE)
    }
    
    ## Sort the tuning parameters from least complex to most complex
    if(!is.null(models$sort)) performance <- models$sort(performance)
    
    if(any(is.na(performance[, metric])))
      warning("missing values found in aggregated results")
    
    
    if(trControl$verboseIter && nrow(performance) > 1) {
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
    if(selectClass == "function") {
      bestIter <- trControl$selectionFunction(x = perf_check,
                                              metric = metric,
                                              maximize = maximize)
    }
    else {
      if(trControl$selectionFunction == "oneSE") {
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
    
    if(is.na(bestIter) || length(bestIter) != 1) stop("final tuning parameters could not be determined", call. = FALSE)
    
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
    
  } # end(trControl$method != "none")
  
  ## Save some or all of the resampling summary metrics
  if(!(trControl$method %in% c("LOOCV", "oob", "none"))) {
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
  
  performance <- performance[do.call("order", orderList),]
  
  if(trControl$verboseIter) {
    bestText <- paste(paste(names(bestTune), "=",
                            format(bestTune, digits = 3)),
                      collapse = ", ")
    if(nrow(performance) == 1) bestText <- "final model"
    cat("Fitting", bestText, "on full training set\n")
    flush.console()
  }
  
  ## Make the final model based on the tuning results
  
  indexFinal <- if(is.null(trControl$indexFinal)) 
    seq(along = y) else trControl$indexFinal

  if(!(length(trControl$seeds) == 1 && is.na(trControl$seeds))) 
    set.seed(trControl$seeds[[length(trControl$seeds)]][1])
  finalTime <- system.time(
    finalModel <- rec_model(recipe, 
                            subset_x(data, indexFinal),
                            method = models,
                            tuneValue = bestTune,
                            obsLevels = classLevels,
                            last = TRUE,
                            classProbs = trControl$classProbs,
                            sampling = trControl$sampling,
                            ...)
  )
  
  if(trControl$trim && !is.null(models$trim)) {
    if(trControl$verboseIter) old_size <- object.size(finalModel$fit)
    finalModel$fit <- models$trim(finalModel$fit)
    if(trControl$verboseIter) {
      new_size <- object.size(finalModel$fit)
      reduction <- format(old_size - new_size, units = "Mb")
      if(reduction == "0 Mb") reduction <- "< 0 Mb"
      p_reduction <- (unclass(old_size) - unclass(new_size))/unclass(old_size)*100
      p_reduction <- if(p_reduction < 1) "< 1%" else paste0(round(p_reduction, 0), "%")
      cat("Final model footprint reduced by", reduction, "or", p_reduction, "\n")
    }
  }
  
  finalModel <- finalModel$fit
  
  ## Remove this and check for other places it is reference
  ## replaced by tuneValue
  if(method == "pls") finalModel$bestIter <- bestTune
  
  ## To use predict.train and automatically use the optimal lambda,
  ## we need to save it
  if(method == "glmnet") finalModel$lambdaOpt <- bestTune$lambda
  
  if(trControl$returnData) {
    outData <- data
  } else outData <- NULL
  
  if(trControl$savePredictions == "final")
    tmp$predictions <- merge(bestTune, tmp$predictions)
  
  endTime <- proc.time()
  times <- list(everything = endTime - startTime,
                final = finalTime)
  
  out <- structure(list(method = method,
                        modelInfo = models,
                        modelType = modelType,
                        recipe = trained_rec,
                        results = performance,
                        pred = tmp$predictions,
                        bestTune = bestTune,
                        call = funcCall,
                        dots = list(...),
                        metric = metric,
                        control = trControl,
                        finalModel = finalModel,
                        trainingData = outData,
                        resample = byResample,
                        resampledCM = resampledCM,
                        perfNames = perfNames,
                        maximize = maximize,
                        yLimits = trControl$yLimits,
                        times = times,
                        levels = classLevels),
                   class = c("train.recipe", "train"))
  trControl$yLimits <- NULL
  
  if(trControl$timingSamps > 0) {
    pData <- x[sample(1:nrow(x), trControl$timingSamps, replace = TRUE),,drop = FALSE]
    out$times$prediction <- system.time(predict(out, pData))
  } else  out$times$prediction <- rep(NA, 3)
  out
}


#' @export
predict.train.recipe <- function(object,
                                 newdata = stop("Please provide `newdata`"),
                                 type = "raw",
                                 ...) {
  if (type == "raw") {
    predicted <- rec_pred(method = object$modelInfo,
                          object = list(fit = object$finalModel,
                                        recipe = object$recipe),
                          newdata = newdata)
    names(predicted) <- NULL
    if (!is.null(object$levels) && !is.na(object$levels)) {
      predicted <- if (attr(object$levels, "ordered"))
        ordered(as.character(predicted), levels = object$levels)
      else
        factor(as.character(predicted), levels = object$levels)
    }
  } else {
    predicted <- rec_prob(method = object$modelInfo,
                          object = list(fit = object$finalModel,
                                        recipe = object$recipe),
                          newdata = newdata)
    predicted <- predicted[, object$levels]
  }
  predicted
}


## drop dimensions from a `tibble`
get_vector <- function(object) {
  if(!inherits(object, "tbl_df") & !is.data.frame(object))
    return(object)
  if(ncol(object) > 1)
    stop("Only one column should be available")
  getElement(object, names(object)[1])
}

## return a vector of names
role_cols <- function(object, role) {
  vars <- object$term_info
  vars$variable[vars$role %in% role]
}

## Check to make sure that old syntax is not used
preproc_dots <- function(...) {
  dots <- list(...)
  is_pp <- grepl("^preProc", names(dots))
  if(any(is_pp))
    warning("When using a recipe with `train`, ",
            paste0("`", names(dots)[is_pp], "`", collapse = ", "),
            " will be ignored.", 
            call. = FALSE)
  invisible(NULL)
}


model_failed <- function(x) {
  if(inherits(x, "try-error")) 
    return(TRUE)
  if(any(names(x) == "fit"))
    if(inherits(x$fit, "try-error")) 
      return(TRUE) 
  if(any(names(x) == "recipe"))
    if(inherits(x$recipe, "try-error")) 
      return(TRUE) 
  FALSE
}

pred_failed <- function(x)
  inherits(x, "try-error")



## Convert the recipe to holdout data. rename this to something like
## get_perf_data
#' @importFrom recipes bake all_predictors all_outcomes has_role
holdout_rec <- function(object, dat, index) {
  ## 
  ho_data <- bake(object$recipe, 
                  newdata = subset_x(dat, index),
                  all_outcomes())
  names(ho_data) <- "obs"
  ## ~~~~~~ move these two to other functions:
  wt_cols <- role_cols(object$recipe, "case weight")
  if(length(wt_cols) > 0) {
    wts <- bake(object$recipe, 
                newdata = subset_x(dat, index),
                has_role("case weight"))
    ho_data$weights <- get_vector(wts)
    rm(wts)
  }
  perf_cols <- role_cols(object$recipe, "performance var")
  if(length(perf_cols) > 0) {
    perf_data <- bake(object$recipe, 
                      newdata = subset_x(dat, index),
                      has_role("performance var"))
    ho_data <- cbind(ho_data, perf_data)
  }  
  ## ~~~~~~
  
  ho_data$rowIndex <- (1:nrow(dat))[index]
  ho_data <- as.data.frame(ho_data)
}

#' @importFrom recipes bake prepare juice has_role
rec_model <- function(rec, dat, method, tuneValue, obsLevels, 
                      last = FALSE, sampling = NULL, classProbs, ...) {
  
  if(!is.null(sampling) && sampling$first) {
    ## get original column names for downsamping then reassemble
    ## the training set prior to making the recipe
    var_info <- summary(rec)
    y_cols <- role_cols(rec, "outcome")
    y <- dat[, y_cols]
    if(length(y_cols) > 1) 
      stop("`train` doesn't support multivariate outcomes")
    if(is.data.frame(y)) y <- getElement(y, names(y))
    other_cols <- var_info[var_info$role %in% c("predictor", "case weight", "performance var"),]
    
    other_cols <- other_cols$variable
    other_dat <- dat[, other_cols]  ## test this with data frames and tibbles
    
    tmp <- sampling$func(other_dat, y)
    orig_dat <- dat
    dat <- tmp$x
    dat[, y_cols] <- tmp$y
    rm(tmp, y, other_cols, other_dat, orig_dat)
  }
  
  trained_rec <- prepare(rec, training = dat, fresh = TRUE, 
                         verbose = FALSE, stringsAsFactors = TRUE,
                         retain = TRUE)
  x <- juice(trained_rec, all_predictors())
  y <- juice(trained_rec, all_outcomes())
  y <- get_vector(y)
  
  is_weight <- summary(trained_rec)$role == "case weight"
  if(any(is_weight)) {
    if(sum(is_weight) > 1)
      stop("Ony one column can be used as a case weight.")
    weights <- bake(trained_rec, newdata = dat, has_role("case weight"))
    weights <- get_vector(weights)
  } else weights <- NULL
  
  if(!is.null(sampling) && !sampling$first) {
    tmp <- sampling$func(x, y)
    x <- tmp$x
    y <- tmp$y
    rm(tmp)
  }
  
  modelFit <- try(method$fit(x = x,
                         y = y, wts = weights,
                         param  = tuneValue, lev = obsLevels,
                         last = last,
                         classProbs = classProbs, ...),
                  silent = TRUE)
  
  ## for models using S4 classes, you can't easily append data, so
  ## exclude these and we'll use other methods to get this information
  if(is.null(method$label)) method$label <- ""
  if(!isS4(modelFit) & !model_failed(modelFit)) {
    modelFit$xNames <- colnames(x)
    modelFit$problemType <- if(is.factor(y)) "Classification" else "Regression"
    modelFit$tuneValue <- tuneValue
    modelFit$obsLevels <- obsLevels
    modelFit$param <- list(...)
  }

  list(fit = modelFit, recipe = trained_rec)
}

#' @importFrom recipes bake all_predictors
rec_pred <- function (method, object, newdata, param = NULL)  {
  x <- bake(object$recipe, newdata = newdata, all_predictors())
  out <- method$predict(modelFit = object$fit, newdata = x, 
                        submodels = param)
  if(is.matrix(out) | is.data.frame(out))
    out <- out[,1]
  out
}

#' @importFrom recipes bake all_predictors
rec_prob <- function (method, object, newdata = NULL, param = NULL)  {
  x <- bake(object$recipe, newdata = newdata, all_predictors())
  obsLevels <- levels(object$fit)
  classProb <- method$prob(modelFit = object$fit, newdata = x, 
                           submodels = param)
  if (!is.data.frame(classProb) & is.null(param)) {
    classProb <- as.data.frame(classProb)
    if (!is.null(obsLevels)) 
      classprob <- classProb[, obsLevels]
  }
  classProb
}

## analogous workflows to the originals
loo_train_rec <- function(rec, dat, info, method, 
                          ctrl, lev, testing = FALSE, ...) {
  loadNamespace("caret")
  loadNamespace("recipes")
  
  printed <- format(info$loop)
  colnames(printed) <- gsub("^\\.", "", colnames(printed))
  
  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  
  is_regression <- is.null(lev)
  
  pkgs <- c("methods", "caret", "recipes")
  if(!is.null(method$library)) 
    pkgs <- c(pkgs, method$library)
  
  result <- foreach(iter = seq(along = ctrl$index), 
                    .combine = "rbind", 
                    .verbose = FALSE, 
                    .packages = pkgs, 
                    .errorhandling = "stop") %:%
    foreach(parm = 1:nrow(info$loop), 
            .combine = "rbind", 
            .verbose = FALSE, 
            .packages = pkgs, 
            .errorhandling = "stop") %op% {
              
              if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) 
                set.seed(ctrl$seeds[[iter]][parm])
              
              if(testing) cat("after loops\n")
              loadNamespace("caret")
              if(ctrl$verboseIter) 
                progress(printed[parm,,drop = FALSE],
                         names(ctrl$index), iter, TRUE)
              
              if(is.null(info$submodels[[parm]]) 
                 || nrow(info$submodels[[parm]]) > 0) {
                submod <- info$submodels[[parm]]
              } else submod <- NULL
              
              mod_rec <- 
                try(
                  rec_model(rec, dat[ ctrl$index[[iter]], ],
                            method = method,
                            tuneValue = info$loop[parm,,drop = FALSE],
                            obsLevels = lev,
                            classProbs = ctrl$classProbs,
                            sampling = ctrl$sampling,
                            ...),
                  silent = TRUE)
              
              holdoutIndex <- ctrl$indexOut[[iter]]
              
              if(!model_failed(mod_rec)) {
                predicted <- try(
                  rec_pred(method = method,
                           object = mod_rec,
                           newdata = subset_x(dat, holdoutIndex),
                           param = submod),
                  silent = TRUE)
                
                if(pred_failed(predicted)) {
                  fail_warning(settings = printed[parm,,drop = FALSE], 
                               msg  = predicted, 
                               where = "predictions", 
                               iter = names(ctrl$index)[iter], 
                               verb = ctrl$verboseIter)
                  
                  predicted <- fill_failed_pred(index = holdoutIndex, lev = lev, submod)
                }
              } else {
                fail_warning(settings = printed[parm,,drop = FALSE], 
                             msg  = mod_rec, 
                             iter = names(ctrl$index)[iter], 
                             verb = ctrl$verboseIter)
                predicted <- fill_failed_pred(index = holdoutIndex, lev = lev, submod)
              }
              
              if(testing) print(head(predicted))
              if(ctrl$classProbs) {
                if(!model_failed(mod_rec)) {
                  probValues <- rec_prob(method = method,
                                         object = mod_rec,
                                         newdata = subset_x(dat, holdoutIndex),
                                         param = submod)
                } else {
                  probValues <- fill_failed_prob(holdoutIndex, lev, submod)
                }
                if(testing) print(head(probValues))
              }

              predicted <- trim_values(predicted, ctrl, is_regression) 
              
              ##################################
              
              ## We'll attach data points/columns to the object used
              ## to assess holdout performance

              ho_data <- holdout_rec(mod_rec, dat, holdoutIndex)
              
              if(!is.null(info$submodels)) {
                ## collate the predictions across all the sub-models
                predicted <- lapply(predicted,
                                    function(x, lv, dat) {
                                      x <- outcome_conversion(x, lv = lev)
                                      dat$pred <- x
                                      dat
                                    },
                                    lv = lev,
                                    dat  = ho_data)
                if(testing) print(head(predicted))
                ## same for the class probabilities
                if(ctrl$classProbs) {
                  for(k in seq(along = predicted)) predicted[[k]] <- 
                      cbind(predicted[[k]], probValues[[k]])
                }
                predicted <- do.call("rbind", predicted)
                allParam <- expandParameters(info$loop[parm,,drop = FALSE], submod)
                rownames(predicted) <- NULL
                predicted <- cbind(predicted, allParam)
                ## if saveDetails then save and export 'predicted'
              } else {
                pred_val <- outcome_conversion(predicted, lv = lev)
                predicted <-  ho_data
                predicted$pred <- pred_val
                if(ctrl$classProbs) predicted <- cbind(predicted, probValues)
                predicted <- cbind(predicted, info$loop[parm,,drop = FALSE])
              }
              if(ctrl$verboseIter) 
                progress(printed[parm,,drop = FALSE],
                         names(ctrl$index), iter, FALSE)
              
              predicted
            }
  
  names(result) <- gsub("^\\.", "", names(result))
  out <- ddply(result,
               as.character(method$parameter$parameter),
               ctrl$summaryFunction,
               lev = lev,
               model = method)
  list(performance = out, predictions = result)
}


oob_train_rec <- function(rec, dat, info, method, 
                          ctrl, lev, testing = FALSE, ...) {
  loadNamespace("caret")
  loadNamespace("recipes")
  
  printed <- format(info$loop)
  colnames(printed) <- gsub("^\\.", "", colnames(printed))
  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  
  
  pkgs <- c("methods", "caret", "recipes")
  if(!is.null(method$library)) pkgs <- c(pkgs, method$library)
  result <- foreach(
    parm = 1:nrow(info$loop), 
    .packages = pkgs, 
    .combine = "rbind") %op%  {
      
      loadNamespace("caret")
      if(ctrl$verboseIter) 
        progress(printed[parm,,drop = FALSE], "", 1, TRUE)
      
      if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) 
        set.seed(ctrl$seeds[[1L]][parm])
      
      mod <- rec_model(rec, dat,
                       method = method,
                       tuneValue = info$loop[parm,,drop = FALSE],
                       obsLevels = lev,
                       classProbs = ctrl$classProbs,
                       sampling = ctrl$sampling,
                       ...)
      
      out <- method$oob(mod$fit)
      
      if(ctrl$verboseIter) 
        progress(printed[parm,,drop = FALSE], "", 1, FALSE)
      
      cbind(as.data.frame(t(out)), info$loop[parm,,drop = FALSE])
    }
  names(result) <- gsub("^\\.", "", names(result))
  result
}

train_rec <- function(rec, dat, info, method, ctrl, lev, testing = FALSE, ...) {
  loadNamespace("caret")
  loadNamespace("recipes")
  
  printed <- format(info$loop, digits = 4)
  colnames(printed) <- gsub("^\\.", "", colnames(printed))
  
  ## For 632 estimator, add an element to the index of zeros to trick it into
  ## fitting and predicting the full data set.
  
  resampleIndex <- ctrl$index
  if(ctrl$method %in% c("boot632", "optimism_boot", "boot_all")) {
    resampleIndex <- c(list("AllData" = rep(0, nrow(dat))), resampleIndex)
    ctrl$indexOut <- c(list("AllData" = rep(0, nrow(dat))),  ctrl$indexOut)
    if(!is.null(ctrl$indexExtra)) 
      ctrl$indexExtra <- c(list("AllData" = NULL), ctrl$indexExtra)
  }
  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  keep_pred <- isTRUE(ctrl$savePredictions) || ctrl$savePredictions %in% c("all", "final")
  pkgs <- c("methods", "caret", "recipes")
  if(!is.null(method$library)) pkgs <- c(pkgs, method$library)
  
  is_regression <- is.null(lev)
  
  export <- c("optimism_boot")

  result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .packages = pkgs, .export = export) %:%
    foreach(parm = 1L:nrow(info$loop), .combine = "c", .packages = pkgs, .export = export)  %op% {
      
      if(!(length(ctrl$seeds) == 1L && is.na(ctrl$seeds))) 
        set.seed(ctrl$seeds[[iter]][parm])
      
      loadNamespace("caret")
      loadNamespace("recipes")
      if(ctrl$verboseIter) 
        progress(printed[parm,,drop = FALSE],
                 names(resampleIndex), iter)

      if(names(resampleIndex)[iter] != "AllData") {
        modelIndex <- resampleIndex[[iter]]
        holdoutIndex <- ctrl$indexOut[[iter]]
      } else {
        modelIndex <- 1:nrow(dat)
        holdoutIndex <- modelIndex
      }

      if(testing) cat("pre-model\n")
      
      if(!is.null(info$submodels[[parm]]) && nrow(info$submodels[[parm]]) > 0) {
        submod <- info$submodels[[parm]]
      } else submod <- NULL
      
      mod_rec <- try(
        rec_model(rec, 
                  subset_x(dat, modelIndex),
                  method = method,
                  tuneValue = info$loop[parm,,drop = FALSE],
                  obsLevels = lev,
                  classProbs = ctrl$classProbs,
                  sampling = ctrl$sampling,
                  ...),
        silent = TRUE)
      if(testing) print(mod_rec) 
      
      if(!model_failed(mod_rec)) {
        predicted <- try(
          rec_pred(method = method,
                   object = mod_rec,
                   newdata = subset_x(dat, holdoutIndex),
                   param = submod),
          silent = TRUE)
        
        if(pred_failed(predicted)) {
          fail_warning(settings = printed[parm,,drop = FALSE], 
                       msg  = predicted, 
                       where = "predictions", 
                       iter = names(resampleIndex)[iter], 
                       verb = ctrl$verboseIter)
          
          predicted <- fill_failed_pred(index = holdoutIndex, lev = lev, submod)
        }
      } else {
        fail_warning(settings = printed[parm,,drop = FALSE], 
                     msg  = mod_rec, 
                     iter = names(resampleIndex)[iter], 
                     verb = ctrl$verboseIter)
        ## setup a dummy results with NA values for all predictions
        predicted <- fill_failed_pred(index = holdoutIndex, lev = lev, submod)
      }

      if(testing) print(head(predicted))
      if(ctrl$classProbs) {
        if(!model_failed(mod_rec)) {
          probValues <- rec_prob(method = method,
                                 object = mod_rec,
                                 newdata = subset_x(dat, holdoutIndex),
                                 param = submod)
        } else {
          probValues <- fill_failed_prob(holdoutIndex, lev, submod)
        }
        if(testing) print(head(probValues))
      }
      
      ##################################
      
      predicted <- trim_values(predicted, ctrl, is_regression) 
   
      ## We'll attach data points/columns to the object used
      ## to assess holdout performance

## TODO what to do when the recipe fails?
      ho_data <- holdout_rec(mod_rec, dat, holdoutIndex)
   
      if(!is.null(submod)) {
        ## merge the fixed and seq parameter values together
        allParam <- expandParameters(info$loop[parm,,drop = FALSE], submod)
        allParam <- allParam[complete.cases(allParam),, drop = FALSE]
        
        ## collate the predictions across all the sub-models
        predicted <- lapply(predicted,
                            function(x, lv, dat) {
                              x <- outcome_conversion(x, lv = lev)
                              dat$pred <- x
                              dat
                            },
                            lv = lev,
                            dat  = ho_data)
        if(testing) print(head(predicted))
        
        ## same for the class probabilities
        if(ctrl$classProbs) predicted <- mapply(cbind, predicted, probValues, SIMPLIFY = FALSE)
        
        if(keep_pred) {
          tmpPred <- predicted
          for(modIndex in seq(along = tmpPred)) {
            tmpPred[[modIndex]] <- merge(tmpPred[[modIndex]], 
                                         allParam[modIndex,,drop = FALSE],
                                         all = TRUE)
          }
          tmpPred <- rbind.fill(tmpPred)
          tmpPred$Resample <- names(resampleIndex)[iter]
        } else tmpPred <- NULL
        
        ## get the performance for this resample for each sub-model
        thisResample <- lapply(predicted,
                               ctrl$summaryFunction,
                               lev = lev,
                               model = method)
        if(testing) print(head(thisResample))
        
        ## for classification, add the cell counts
        if(length(lev) > 1 && length(lev) <= 50) {
          cells <- lapply(predicted,
                          function(x) flatTable(x$pred, x$obs))
          for(ind in seq(along = cells)) 
            thisResample[[ind]] <- c(thisResample[[ind]], cells[[ind]])
        }
        thisResample <- do.call("rbind", thisResample)          
        thisResample <- cbind(allParam, thisResample)
        
      } else {       
        pred_val <- outcome_conversion(predicted, lv = lev)
        tmp <-  ho_data
        tmp$pred <- pred_val
        if(ctrl$classProbs) tmp <- cbind(tmp, probValues)
        tmp <- merge(tmp, info$loop[parm,,drop = FALSE], all = TRUE)
        
        if(keep_pred) {
          tmpPred <- tmp
          tmpPred$rowIndex <- holdoutIndex
          tmpPred <- merge(tmpPred, info$loop[parm,,drop = FALSE],
                           all = TRUE)
          tmpPred$Resample <- names(resampleIndex)[iter]
        } else tmpPred <- NULL
        
        ##################################
        
        thisResample <- ctrl$summaryFunction(tmp,
                                             lev = lev,
                                             model = method)
         
        ## if classification, get the confusion matrix
        if(length(lev) > 1 && length(lev) <= 50) 
          thisResample <- c(thisResample, flatTable(tmp$pred, tmp$obs))
        thisResample <- as.data.frame(t(thisResample))
        thisResample <- cbind(thisResample, info$loop[parm,,drop = FALSE])
      }
      thisResample$Resample <- names(resampleIndex)[iter]
      
      thisResampleExtra <- optimism_boot(ctrl, dat, iter, lev, method, mod_rec, predicted, 
                                         submod, info$loop[parm,, drop = FALSE])
      
      if(ctrl$verboseIter) 
        progress(printed[parm,,drop = FALSE],
                 names(resampleIndex), iter, FALSE)
      
      if(testing) print(thisResample)
      list(resamples = thisResample, pred = tmpPred, resamplesExtra = thisResampleExtra)
    }

  resamples <- rbind.fill(result[names(result) == "resamples"])
  pred <- rbind.fill(result[names(result) == "pred"])
  resamplesExtra <- rbind.fill(result[names(result) == "resamplesExtra"])
  if(ctrl$method %in% c("boot632", "optimism_boot", "boot_all")) {
    perfNames <- names(resamples)
    perfNames <- perfNames[!(perfNames %in% c("Resample", as.character(method$parameters$parameter)))]
    perfNames <- perfNames[!grepl("^\\.cell[0-9]", perfNames)]
    apparent <- subset(resamples, Resample == "AllData")
    apparent <- apparent[,!grepl("^\\.cell|Resample", colnames(apparent)), drop = FALSE]
    names(apparent)[which(names(apparent) %in% perfNames)] <- 
      paste(names(apparent)[which(names(apparent) %in% perfNames)], "Apparent", sep = "")
    names(apparent) <- gsub("^\\.", "", names(apparent))
    if(any(!complete.cases(apparent[,!grepl("^cell|Resample", colnames(apparent)), drop = FALSE])))
      warning("There were missing values in the apparent performance measures.")
    
    resamples <- subset(resamples, Resample != "AllData")
    if(!is.null(pred)) {
      predHat <- subset(pred, Resample == "AllData")
      pred <- subset(pred, Resample != "AllData")
    }
  }
  names(resamples) <- gsub("^\\.", "", names(resamples))
  
  if(any(!complete.cases(resamples[,!grepl("^cell|Resample", colnames(resamples)), drop = FALSE])))
    warning("There were missing values in resampled performance measures.")
  
  out <- ddply(resamples[,!grepl("^cell|Resample", colnames(resamples)), drop = FALSE],
               ## TODO check this for seq models
               gsub("^\\.", "", colnames(info$loop)),
               MeanSD, 
               exclude = gsub("^\\.", "", colnames(info$loop)))
  
  if(ctrl$method %in% c("boot632", "boot_all")) {
    out <- merge(out, apparent)
    const <- 1 - exp(-1)
    sapply(perfNames, function(perfName) {
      perfOut <- if(ctrl$method == "boot_all") paste0(perfName, "_632") else perfName
      out[, perfOut] <<- (const * out[, perfName]) +  ((1-const) * out[, paste(perfName, "Apparent", sep = "")])
      NULL
    })
  }
  
  if(ctrl$method %in% c("optimism_boot", "boot_all")) {
    out <- merge(out, apparent)
    out <- merge(out, ddply(resamplesExtra[, !grepl("Resample", colnames(resamplesExtra)), drop = FALSE],
                            colnames(info$loop),
                            function(df, exclude) {
                              colMeans(df[, setdiff(colnames(df), exclude), drop = FALSE])
                            },
                            exclude = colnames(info$loop)))
    sapply(perfNames, function(perfName) {
      optimism <- out[ , paste0(perfName, "Orig")] - out[ , paste0(perfName, "Boot")]
      final_estimate <- out[ , paste0(perfName, "Apparent")] + optimism
      ## Remove unnecessary values
      out[ , paste0(perfName, "Orig")] <<- NULL
      out[ , paste0(perfName, "Boot")] <<- NULL
      perfOut <- if(ctrl$method == "boot_all") paste0(perfName, "_OptBoot") else perfName
      ## Update estimates
      out[ , paste0(perfName, "Optimism")] <<- optimism
      out[ , perfOut] <<- final_estimate
      NULL
    })
  }
  
  list(performance = out, resamples = resamples, predictions = if(keep_pred) pred else NULL)
}




