#' Fit Predictive Models over Different Tuning Parameters
#' 
#' This function sets up a grid of tuning parameters for a number of
#' classification and regression routines, fits each model and calculates a
#' resampling based performance measure.
#' 
#' \code{train} can be used to tune models by picking the complexity parameters
#' that are associated with the optimal resampling statistics. For particular
#' model, a grid of parameters (if any) is created and the model is trained on
#' slightly different data for each candidate combination of tuning parameters.
#' Across each data set, the performance of held-out samples is calculated and
#' the mean and standard deviation is summarized for each combination. The
#' combination with the optimal resampling statistic is chosen as the final
#' model and the entire training set is used to fit a final model.
#' 
#' The predictors in \code{x} can be most any object as long as the underlying
#' model fit function can deal with the object class. The function was designed
#' to work with simple matrices and data frame inputs, so some functionality
#' may not work (e.g. pre-processing). When using string kernels, the vector of
#' character strings should be converted to a matrix with a single column.
#' 
#' More details on this function can be found at
#' \url{http://topepo.github.io/caret/training.html}.
#' 
#' A variety of models are currently available and are enumerated by tag (i.e.
#' their model characteristics) at
#' \url{http://topepo.github.io/caret/bytag.html}.
#' 
#' @aliases train train.default train.formula
#' @param x an object where samples are in rows and features are in columns.
#' This could be a simple matrix, data frame or other type (e.g. sparse
#' matrix). See Details below.
#' @param y a numeric or factor vector containing the outcome for each sample.
#' @param form A formula of the form \code{y ~ x1 + x2 + ...}
#' @param data Data frame from which variables specified in \code{formula} are
#' preferentially to be taken.
#' @param weights a numeric vector of case weights. This argument will only
#' affect models that allow case weights.
#' @param subset An index vector specifying the cases to be used in the
#' training sample. (NOTE: If given, this argument must be named.)
#' @param na.action A function to specify the action to be taken if NAs are
#' found. The default action is for the procedure to fail. An alternative is
#' \code{na.omit}, which leads to rejection of cases with missing values on any
#' required variable. (NOTE: If given, this argument must be named.)
#' @param contrasts a list of contrasts to be used for some or all the factors
#' appearing as variables in the model formula.
#' @param method a string specifying which classification or regression model
#' to use. Possible values are found using \code{names(getModelInfo())}. See
#' \url{http://topepo.github.io/caret/bytag.html}. A list of functions can also
#' be passed for a custom model function. See
#' \url{http://topepo.github.io/caret/custom_models.html} for details.
#' @param \dots arguments passed to the classification or regression routine
#' (such as \code{\link[randomForest]{randomForest}}). Errors will occur if
#' values for tuning parameters are passed here.
#' @param preProcess a string vector that defines a pre-processing of the
#' predictor data. Current possibilities are "BoxCox", "YeoJohnson",
#' "expoTrans", "center", "scale", "range", "knnImpute", "bagImpute",
#' "medianImpute", "pca", "ica" and "spatialSign". The default is no
#' pre-processing. See \code{\link{preProcess}} and \code{\link{trainControl}}
#' on the procedures and how to adjust them. Pre-processing code is only
#' designed to work when \code{x} is a simple matrix or data frame.
#' @param metric a string that specifies what summary metric will be used to
#' select the optimal model. By default, possible values are "RMSE" and
#' "Rsquared" for regression and "Accuracy" and "Kappa" for classification. If
#' custom performance metrics are used (via the \code{summaryFunction} argument
#' in \code{\link{trainControl}}, the value of \code{metric} should match one
#' of the arguments. If it does not, a warning is issued and the first metric
#' given by the \code{summaryFunction} is used. (NOTE: If given, this argument
#' must be named.)
#' @param maximize a logical: should the metric be maximized or minimized?
#' @param trControl a list of values that define how this function acts. See
#' \code{\link{trainControl}} and
#' \url{http://topepo.github.io/caret/training.html#custom}. (NOTE: If given,
#' this argument must be named.)
#' @param tuneGrid a data frame with possible tuning values. The columns are
#' named the same as the tuning parameters. Use \code{\link{getModelInfo}} to
#' get a list of tuning parameters for each model or see
#' \url{http://topepo.github.io/caret/modelList.html}. (NOTE: If given, this
#' argument must be named.)
#' @param tuneLength an integer denoting the amount of granularity in the
#' tuning parameter grid. By default, this argument is the number of levels for
#' each tuning parameters that should be generated by \code{\link{train}}. If
#' \code{\link{trainControl}} has the option \code{search = "random"}, this is
#' the maximum number of tuning parameter combinations that will be generated
#' by the random search. (NOTE: If given, this argument must be named.)
#' @return A list is returned of class \code{train} containing: \item{method
#' }{the chosen model.} \item{modelType }{an identifier of the model type.}
#' \item{results }{a data frame the training error rate and values of the
#' tuning parameters.} \item{bestTune }{a data frame with the final
#' parameters.}
#' 
#' \item{call }{the (matched) function call with dots expanded} \item{dots}{a
#' list containing any ... values passed to the original call} \item{metric}{a
#' string that specifies what summary metric will be used to select the optimal
#' model.} \item{control}{the list of control parameters.} \item{preProcess
#' }{either \code{NULL} or an object of class \code{\link{preProcess}}}
#' \item{finalModel}{an fit object using the best parameters}
#' \item{trainingData}{a data frame} \item{resample}{A data frame with columns
#' for each performance metric. Each row corresponds to each resample. If
#' leave-one-out cross-validation or out-of-bag estimation methods are
#' requested, this will be \code{NULL}. The \code{returnResamp} argument of
#' \code{\link{trainControl}} controls how much of the resampled results are
#' saved.} \item{perfNames}{a character vector of performance metrics that are
#' produced by the summary function} \item{maximize}{a logical recycled from
#' the function arguments.} \item{yLimits}{the range of the training set
#' outcomes.} \item{times}{a list of execution times: \code{everything} is for
#' the entire call to \code{train}, \code{final} for the final model fit and,
#' optionally, \code{prediction} for the time to predict new samples (see
#' \code{\link{trainControl}})}
#' @author Max Kuhn (the guts of \code{train.formula} were based on Ripley's
#' \code{nnet.formula})
#' @seealso \code{\link{models}}, \code{\link{trainControl}},
#' \code{\link{update.train}}, \code{\link{modelLookup}},
#' \code{\link{createFolds}}
#' @references \url{http://topepo.github.io/caret/training.html}
#' 
#' Kuhn (2008), ``Building Predictive Models in R Using the caret''
#' (\url{http://www.jstatsoft.org/article/view/v028i05/v28i05.pdf})
#' @keywords models
#' @examples
#' 
#' \dontrun{
#' 
#' #######################################
#' ## Classification Example
#' 
#' data(iris)
#' TrainData <- iris[,1:4]
#' TrainClasses <- iris[,5]
#' 
#' knnFit1 <- train(TrainData, TrainClasses,
#'                  method = "knn",
#'                  preProcess = c("center", "scale"),
#'                  tuneLength = 10,
#'                  trControl = trainControl(method = "cv"))
#' 
#' knnFit2 <- train(TrainData, TrainClasses,
#'                  method = "knn",
#'                  preProcess = c("center", "scale"),
#'                  tuneLength = 10, 
#'                  trControl = trainControl(method = "boot"))
#' 
#' 
#' library(MASS)
#' nnetFit <- train(TrainData, TrainClasses,
#'                  method = "nnet",
#'                  preProcess = "range", 
#'                  tuneLength = 2,
#'                  trace = FALSE,
#'                  maxit = 100)
#' 
#' #######################################
#' ## Regression Example
#' 
#' library(mlbench)
#' data(BostonHousing)
#' 
#' lmFit <- train(medv ~ . + rm:lstat,
#'                data = BostonHousing, 
#'                method = "lm")
#' 
#' library(rpart)
#' rpartFit <- train(medv ~ .,
#'                   data = BostonHousing,
#'                   method = "rpart",
#'                   tuneLength = 9)
#' 
#' #######################################
#' ## Example with a custom metric
#' 
#' madSummary <- function (data,
#'                         lev = NULL,
#'                         model = NULL) {
#'   out <- mad(data$obs - data$pred, 
#'              na.rm = TRUE)  
#'   names(out) <- "MAD"
#'   out
#' }
#' 
#' robustControl <- trainControl(summaryFunction = madSummary)
#' marsGrid <- expand.grid(degree = 1, nprune = (1:10) * 2)
#' 
#' earthFit <- train(medv ~ .,
#'                   data = BostonHousing, 
#'                   method = "earth",
#'                   tuneGrid = marsGrid,
#'                   metric = "MAD",
#'                   maximize = FALSE,
#'                   trControl = robustControl)
#' 
#' #######################################
#' ## Parallel Processing Example via multicore package
#' 
#' ## library(doMC)
#' ## registerDoMC(2)
#' 
#' ## NOTE: don't run models form RWeka when using
#' ### multicore. The session will crash.
#' 
#' ## The code for train() does not change:
#' set.seed(1)
#' usingMC <-  train(medv ~ .,
#'                   data = BostonHousing, 
#'                   method = "glmboost")
#' 
#' ## or use:
#' ## library(doMPI) or 
#' ## library(doParallel) or 
#' ## library(doSMP) and so on
#' 
#' }
#' 
#' 
#' @export train
"train" <-
  function(x, ...){
    UseMethod("train")
  }

#' @rdname train
#' @importFrom stats predict
#' @importFrom utils object.size flush.console
#' @export
train.default <- function(x, y, 
                          method = "rf",
                          preProcess = NULL,
                          ...,
                          weights = NULL,
                          metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
                          maximize = ifelse(metric %in% c("RMSE", "logLoss"), FALSE, TRUE),
                          trControl = trainControl(),
                          tuneGrid = NULL,
                          tuneLength = 3) {
  startTime <- proc.time()
  
  if(is.character(y)) y <- as.factor(y)
  
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
  if(any(names(models) == "check") && is.function(models$check)) {
    software_check <- models$check(models$library)
  }
  
  
  paramNames <- as.character(models$parameters$parameter)
  
  funcCall <- match.call(expand.dots = TRUE)
  modelType <- get_model_type(y)
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
  
  if(modelType == "Regression" & length(unique(y)) == 2)
    warning(paste("You are trying to do regression and your outcome only has",
                  "two possible values Are you trying to do classification?",
                  "If so, use a 2 level factor as your outcome column."))
  
  if(modelType != "Classification" & !is.null(trControl$sampling))
    stop("sampling methods are only implemented for classification problems")
  if(!is.null(trControl$sampling)) {
    trControl$sampling <- parse_sampling(trControl$sampling)
  }
  
  if(any(class(x) == "data.table")) x <- as.data.frame(x)
  check_dims(x = x, y = y)
  n <- if(class(y)[1] == "Surv") nrow(y) else length(y)
  
  ## TODO add check method and execute here
  
  ## Some models that use RWeka start multiple threads and this conflicts with multicore:
  if(any(search() == "package:doMC") && getDoParRegistered() && "RWeka" %in% models$library)
    warning("Models using Weka will not work with parallel processing with multicore/doMC")
  flush.console()
  
  if(!is.null(preProcess) && !(all(preProcess %in% ppMethods))) 
    stop(paste('pre-processing methods are limited to:', paste(ppMethods, collapse = ", ")))
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
      stop(paste("One or more factor levels in the outcome has no data:", xtab_msg))
    }
    
    if(trControl$classProbs && any(classLevels != make.names(classLevels))) {
      stop(paste("At least one of the class levels is not a valid R variable name;",
                 "This will cause errors when class probabilities are generated because",
                 "the variables names will be converted to ",
                 paste(make.names(classLevels), collapse = ", "),
                 ". Please use factor levels that can be used as valid R variable names",
                 " (see ?make.names for help)."))
    }
    
    if(metric %in% c("RMSE", "Rsquared")) 
      stop(paste("Metric", metric, "not applicable for classification models"))
    if(!trControl$classProbs && metric == "ROC")
      stop(paste("Class probabilities are needed to score models using the",
                 "area under the ROC curve. Set `classProbs = TRUE`",
                 "in the trainControl() function."))
      
    if(trControl$classProbs) {
      if(!is.function(models$prob)) {
        warning("Class probabilities were requested for a model that does not implement them")
        trControl$classProbs <- FALSE
      }
    }         
  } else {
    if(metric %in% c("Accuracy", "Kappa")) 
      stop(paste("Metric", metric, "not applicable for regression models"))         
    classLevels <- NA
    if(trControl$classProbs) {
      warning("cannnot compute class probabilities for regression")
      trControl$classProbs <- FALSE
    }   
  }
  
  
  if(trControl$method == "oob" & is.null(models$oob))
    stop("Out of bag estimates are not implemented for this model")
  
  ## SURV TODO: make resampling functions classes or ifelses based on data type
  
  ## If they don't exist, make the data partitions for the resampling iterations.
  if(is.null(trControl$index)) {
    if(trControl$method == "custom")
      stop("'custom' resampling is appropriate when the `trControl` argument `index` is used")
    trControl$index <- switch(tolower(trControl$method),
                              oob = NULL,
                              none = list(seq(along = y)),
                              apparent = list(all = seq(along = y)),
                              alt_cv =, cv = createFolds(y, trControl$number, returnTrain = TRUE),
                              repeatedcv =, adaptive_cv = createMultiFolds(y, trControl$number, trControl$repeats),
                              loocv = createFolds(y, n, returnTrain = TRUE),
                              boot =, boot632 =,  adaptive_boot = createResample(y, trControl$number),
                              test = createDataPartition(y, 1, trControl$p),
                              adaptive_lgocv =, lgocv = createDataPartition(y, trControl$number, trControl$p),
                              timeslice = createTimeSlices(seq(along = y),
                                                           initialWindow = trControl$initialWindow,
                                                           horizon = trControl$horizon,
                                                           fixedWindow = trControl$fixedWindow)$train,
                              subsemble = subsemble_index(y, V = trControl$number, J = trControl$repeats))
  } else {
    index_types <- unlist(lapply(trControl$index, is.integer))
    if(!isTRUE(all(index_types)))
      stop("`index` should be lists of integers.")
    if(!is.null(trControl$indexOut)) {
      index_types <- unlist(lapply(trControl$indexOut, is.integer))
      if(!isTRUE(all(index_types)))
        stop("`indexOut` should be lists of integers.")
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
       stop('`savePredictions` should be either logical or "all", "final" or "none"')
  }
  
  ## Create hold--out indicies
  if(is.null(trControl$indexOut) & trControl$method != "oob"){
    if(tolower(trControl$method) != "timeslice") {     
      y_index <- if(class(y)[1] == "Surv") 1:nrow(y) else seq(along = y)
      trControl$indexOut <- lapply(trControl$index,
                                   function(training, allSamples) allSamples[-unique(training)],
                                   allSamples = y_index)
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
  
  ## Gather all the pre-processing info. We will need it to pass into the grid creation
  ## code so that there is a concordance between the data used for modeling and grid creation
  if(!is.null(preProcess)) {
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
      tuneGrid <- models$grid(x = predict(ppObj, x), 
                              y = y, 
                              len = tuneLength, 
                              search = trControl$search)
      rm(ppObj, pp)
    } else tuneGrid <- models$grid(x = x, y = y, len = tuneLength, search = trControl$search)
  }
  
  ## Check to make sure that there are tuning parameters in some cases
  if(grepl("adaptive", trControl$method) & nrow(tuneGrid) == 1) {
    stop(paste("For adaptive resampling, there needs to be more than one",
               "tuning parameter for evaluation"))
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
  trControl$yLimits <- if(is.numeric(y)) get_range(y) else NULL
  
  
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
    ##     the loop object.
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
    ##     # First get the predictions with the value of n.trees as given in the current
    ##     # row of loop
    ##     out <- predict(modelFit,
    ##                    newdata,
    ##                    type = "response",
    ##                    n.trees = modelFit$tuneValue$.n.trees)
    ##
    ##     # param is the current value of submodels. In normal prediction mode (i.e
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
      lengths <- unlist(lapply(trainInfo$submodels, nrow))
      if(all(lengths == 0)) trainInfo$submodels <- NULL
    } else trainInfo <- list(loop = tuneGrid)
    
    
    num_rs <- if(trControl$method != "oob") length(trControl$index) else 1L
    if(trControl$method == "boot632") num_rs <- num_rs + 1L
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
                               "single integer"))
        if(any(is.na(unlist(trControl$seeds)))) stop("At least one seed is missing (NA)")
      }
    }
    
    
    ## SURV TODO: modify defaultSummary for Surv objects
    if(trControl$method == "oob") {
      ## delay this test until later
      perfNames <- metric   
    } else {
      ## run some data thru the summary function and see what we get
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
      stop("Stopping")
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
  
  indexFinal <- if(is.null(trControl$indexFinal)) seq(along = y) else trControl$indexFinal
  
  if(!(length(trControl$seeds) == 1 && is.na(trControl$seeds))) set.seed(trControl$seeds[[length(trControl$seeds)]][1])
  finalTime <- system.time(
    finalModel <- createModel(x = x[indexFinal,,drop=FALSE], 
                              y = y[indexFinal], 
                              wts = weights[indexFinal], 
                              method = models, 
                              tuneValue = bestTune, 
                              obsLevels = classLevels,
                              pp = ppOpt,
                              last = TRUE,
                              classProbs = trControl$classProbs,
                              sampling = trControl$sampling,
                              ...))
  
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
    } else   {
      outData$.outcome <- y
      if(!is.null(weights)) outData$.weights <- weights
    }
  } else outData <- NULL
  
  ## In the case of pam, the data will need to be saved differently
  if(trControl$returnData & method == "pam") {
    finalModel$xData <- x
    finalModel$yData <- y
  }     
  
  if(trControl$savePredictions == "final") 
    tmp$predictions <- merge(bestTune, tmp$predictions)
  
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
                        times = times,
                        levels = classLevels), 
                   class = "train")
  trControl$yLimits <- NULL
  
  if(trControl$timingSamps > 0) {
    pData <- x[sample(1:nrow(x), trControl$timingSamps, replace = TRUE),,drop = FALSE]
    out$times$prediction <- system.time(predict(out, pData))
  } else  out$times$prediction <- rep(NA, 3)
  out
  
}

#' @rdname train
#' @importFrom stats .getXlevels complete.cases contrasts model.frame model.matrix model.response model.weights na.fail
#' @export
train.formula <- function (form, data, ..., weights, subset, na.action = na.fail, contrasts = NULL)  {
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval.parent(m$data)))  m$data <- as.data.frame(data)
  m$... <- m$contrasts <- NULL
  
  check_na_conflict(match.call(expand.dots = TRUE))
  
  ## Look for missing `na.action` in call. To make the default (`na.fail`) 
  ## recognizable by `eval.parent(m)`, we need to add it to the call
  ## object `m`
  
  if(!("na.action" %in% names(m))) m$na.action <- quote(na.fail)
  
  # do we need the double colon here?
  m[[1]] <- quote(stats::model.frame)
  m <- eval.parent(m)
  if(nrow(m) < 1) stop("Every row has at least one missing value were found")
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  int_flag <- grepl("(Intercept)", colnames(x))
  if (any(int_flag)) x <- x[, !int_flag, drop = FALSE]
  w <- as.vector(model.weights(m))
  y <- model.response(m)  
 
  res <- train(x, y, weights = w, ...)
  res$terms <- Terms
  res$coefnames <- colnames(x)
  res$call <- match.call()
  res$na.action <- attr(m, "na.action")
  res$contrasts <- cons
  res$xlevels <- .getXlevels(Terms, m)
  if(!is.null(res$trainingData)) {
    ## We re-save the original data from the formula interface
    ## since it has not been converted to dummy variables. 
    cc <- complete.cases(data[, all.vars(Terms), drop = FALSE])
    res$trainingData <- data[cc,all.vars(Terms), drop = FALSE]
    isY <- names(res$trainingData) %in% as.character(form[[2]])
    if(any(isY)) colnames(res$trainingData)[isY] <- ".outcome"
  }
  class(res) <- c("train", "train.formula")
  res
}

#' @export
summary.train <- function(object, ...) summary(object$finalModel, ...)

#' @importFrom stats predict residuals
#' @export
residuals.train <- function(object, ...) {
  if(object$modelType != "Regression") stop("train() only produces residuals on numeric outcomes")
  resid <- residuals(object$finalModel, ...)
  if(is.null(resid)) {    
    if(!is.null(object$trainingData))  {
      resid <- object$trainingData$.outcome - predict(object, object$trainingData[, names(object$trainingData) != ".outcome",drop = FALSE])
    } else stop("The training data must be saved to produce residuals")
  }
  resid
}

#' @importFrom stats predict fitted
#' @export
fitted.train <- function(object, ...) {
  prd <- fitted(object$finalModel)
  if(is.null(prd)) {    
    if(!is.null(object$trainingData)) {
      prd <- predict(object, object$trainingData[, names(object$trainingData) != ".outcome",drop = FALSE])
    } else stop("The training data must be saved to produce fitted values")
  }
  prd
  
}

