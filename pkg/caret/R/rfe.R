#' Backwards Feature Selection
#'
#' A simple backwards selection, a.k.a. recursive feature elimination (RFE),
#' algorithm
#'
#' More details on this function can be found at
#' \url{http://topepo.github.io/caret/recursive-feature-elimination.html}.
#'
#' This function implements backwards selection of predictors based on
#' predictor importance ranking. The predictors are ranked and the less
#' important ones are sequentially eliminated prior to modeling. The goal is to
#' find a subset of predictors that can be used to produce an accurate model.
#' The web page \url{http://topepo.github.io/caret/recursive-feature-elimination.html#rfe}
#' has more details and examples related to this function.
#'
#' \code{rfe} can be used with "explicit parallelism", where different
#' resamples (e.g. cross-validation group) can be split up and run on multiple
#' machines or processors. By default, \code{rfe} will use a single processor
#' on the host machine. As of version 4.99 of this package, the framework used
#' for parallel processing uses the \pkg{foreach} package. To run the resamples
#' in parallel, the code for \code{rfe} does not change; prior to the call to
#' \code{rfe}, a parallel backend is registered with \pkg{foreach} (see the
#' examples below).
#'
#' \code{rfeIter} is the basic algorithm while \code{rfe} wraps these
#' operations inside of resampling. To avoid selection bias, it is better to
#' use the function \code{rfe} than \code{rfeIter}.
#'
#' When updating a model, if the entire set of resamples were not saved using
#' \code{rfeControl(returnResamp = "final")}, the existing resamples are
#' removed with a warning.
#'
#' @aliases rfe rfe.default rfeIter predict.rfe update.rfe
#' @param x A matrix or data frame of predictors for model training. This
#' object must have unique column names. For the recipes method, \code{x}
#' is a recipe object.
#' @param y a vector of training set outcomes (either numeric or factor)
#' @param testX a matrix or data frame of test set predictors. This must have
#' the same column names as \code{x}
#' @param testY a vector of test set outcomes
#' @param sizes a numeric vector of integers corresponding to the number of
#' features that should be retained
#' @param metric a string that specifies what summary metric will be used to
#' select the optimal model. By default, possible values are "RMSE" and
#' "Rsquared" for regression and "Accuracy" and "Kappa" for classification. If
#' custom performance metrics are used (via the \code{functions} argument in
#' \code{\link{rfeControl}}, the value of \code{metric} should match one of the
#' arguments.
#' @param maximize a logical: should the metric be maximized or minimized?
#' @param rfeControl a list of options, including functions for fitting and
#' prediction. The web page
#' \url{http://topepo.github.io/caret/recursive-feature-elimination.html#rfe} has more
#' details and examples related to this function.
#' @param object an object of class \code{rfe}
#' @param size a single integers corresponding to the number of features that
#' should be retained in the updated model
#' @param label an optional character string to be printed when in verbose
#' mode.
#' @param seeds an optional vector of integers for the size. The vector should
#' have length of \code{length(sizes)+1}
#' @param \dots options to pass to the model fitting function (ignored in
#' \code{predict.rfe})
#' @return A list with elements \item{finalVariables}{a list of size
#' \code{length(sizes) + 1} containing the column names of the ``surviving''
#' predictors at each stage of selection. The first element corresponds to all
#' the predictors (i.e. \code{size = ncol(x)})} \item{pred }{a data frame with
#' columns for the test set outcome, the predicted outcome and the subset
#' size.}
#' @note We using a recipe as an input, there may be some subset
#'  sizes that are not well-replicated over resamples. `rfe` method
#'  will only consider subset sizes where at least half of the
#'  resamples have associated results in the search for an optimal
#'  subset size.
#' @author Max Kuhn
#' @seealso \code{\link{rfeControl}}
#' @keywords models
#' @examples
#'
#' \dontrun{
#' data(BloodBrain)
#'
#' x <- scale(bbbDescr[,-nearZeroVar(bbbDescr)])
#' x <- x[, -findCorrelation(cor(x), .8)]
#' x <- as.data.frame(x, stringsAsFactors = TRUE)
#'
#' set.seed(1)
#' lmProfile <- rfe(x, logBBB,
#'                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
#'                  rfeControl = rfeControl(functions = lmFuncs,
#'                                          number = 200))
#' set.seed(1)
#' lmProfile2 <- rfe(x, logBBB,
#'                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
#'                  rfeControl = rfeControl(functions = lmFuncs,
#'                                          rerank = TRUE,
#'                                          number = 200))
#'
#' xyplot(lmProfile$results$RMSE + lmProfile2$results$RMSE  ~
#'        lmProfile$results$Variables,
#'        type = c("g", "p", "l"),
#'        auto.key = TRUE)
#'
#' rfProfile <- rfe(x, logBBB,
#'                  sizes = c(2, 5, 10, 20),
#'                  rfeControl = rfeControl(functions = rfFuncs))
#'
#' bagProfile <- rfe(x, logBBB,
#'                   sizes = c(2, 5, 10, 20),
#'                   rfeControl = rfeControl(functions = treebagFuncs))
#'
#' set.seed(1)
#' svmProfile <- rfe(x, logBBB,
#'                   sizes = c(2, 5, 10, 20),
#'                   rfeControl = rfeControl(functions = caretFuncs,
#'                                           number = 200),
#'                   ## pass options to train()
#'                   method = "svmRadial")
#'
#' ## classification
#'
#' data(mdrr)
#' mdrrDescr <- mdrrDescr[,-nearZeroVar(mdrrDescr)]
#' mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .8)]
#'
#' set.seed(1)
#' inTrain <- createDataPartition(mdrrClass, p = .75, list = FALSE)[,1]
#'
#' train <- mdrrDescr[ inTrain, ]
#' test  <- mdrrDescr[-inTrain, ]
#' trainClass <- mdrrClass[ inTrain]
#' testClass  <- mdrrClass[-inTrain]
#'
#' set.seed(2)
#' ldaProfile <- rfe(train, trainClass,
#'                   sizes = c(1:10, 15, 30),
#'                   rfeControl = rfeControl(functions = ldaFuncs, method = "cv"))
#' plot(ldaProfile, type = c("o", "g"))
#'
#' postResample(predict(ldaProfile, test), testClass)
#'
#' }
#'
#' #######################################
#' ## Parallel Processing Example via multicore
#'
#' \dontrun{
#' library(doMC)
#'
#' ## Note: if the underlying model also uses foreach, the
#' ## number of cores specified above will double (along with
#' ## the memory requirements)
#' registerDoMC(cores = 2)
#'
#' set.seed(1)
#' lmProfile <- rfe(x, logBBB,
#'                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
#'                  rfeControl = rfeControl(functions = lmFuncs,
#'                                          number = 200))
#'
#'
#' }
#'
#'
#' @export rfe
rfe <- function (x, ...) UseMethod("rfe")

#' @rdname rfe
#' @method rfe default
#' @importFrom stats predict runif
#' @export
"rfe.default" <-
  function(x, y,
           sizes = 2^(2:4),
           metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
           maximize = ifelse(metric %in% c("RMSE", "MAE", "logLoss"), FALSE, TRUE),
           rfeControl = rfeControl(), ...)
  {
    startTime <- proc.time()
    funcCall <- match.call(expand.dots = TRUE)
    if(!("caret" %in% loadedNamespaces())) loadNamespace("caret")

    if(nrow(x) != length(y)) stop("there should be the same number of samples in x and y")
    numFeat <- ncol(x)
    classLevels <- levels(y)

    if(is.null(rfeControl$index))
      rfeControl$index <- switch(tolower(rfeControl$method),
                                 cv = createFolds(y, rfeControl$number, returnTrain = TRUE),
                                 repeatedcv = createMultiFolds(y, rfeControl$number, rfeControl$repeats),
                                 loocv = createFolds(y, length(y), returnTrain = TRUE),
                                 boot =, boot632 = createResample(y, rfeControl$number),
                                 test = createDataPartition(y, 1, rfeControl$p),
                                 lgocv = createDataPartition(y, rfeControl$number, rfeControl$p))

    if(is.null(names(rfeControl$index))) names(rfeControl$index) <- prettySeq(rfeControl$index)
    if(is.null(rfeControl$indexOut)){
      rfeControl$indexOut <- lapply(rfeControl$index,
                                    function(training, allSamples) allSamples[-unique(training)],
                                    allSamples = seq(along = y))
      names(rfeControl$indexOut) <- prettySeq(rfeControl$indexOut)
    }

    sizes <- sort(unique(sizes))
    sizes <- sizes[sizes <= ncol(x)]

    ## check summary function and metric
    testOutput <- data.frame(pred = sample(y, min(10, length(y))),
                             obs = sample(y, min(10, length(y))))

    if(is.factor(y))
    {
      for(i in seq(along = classLevels)) testOutput[, classLevels[i]] <- runif(nrow(testOutput))
    }

    test <- rfeControl$functions$summary(testOutput, lev = classLevels)
    perfNames <- names(test)

    if(!(metric %in% perfNames))
    {
      warning(paste("Metric '", metric, "' is not created by the summary function; '",
                    perfNames[1], "' will be used instead", sep = ""))
      metric <- perfNames[1]
    }

    ## Set or check the seeds when needed
    totalSize <- if(any(sizes == ncol(x))) length(sizes) else length(sizes) + 1
    if(is.null(rfeControl$seeds))
    {
      seeds <- vector(mode = "list", length = length(rfeControl$index))
      seeds <- lapply(seeds, function(x) sample.int(n = 1000000, size = totalSize))
      seeds[[length(rfeControl$index) + 1]] <- sample.int(n = 1000000, size = 1)
      rfeControl$seeds <- seeds
    } else {
      if(!(length(rfeControl$seeds) == 1 && is.na(rfeControl$seeds)))
      {
        ## check versus number of tasks
        numSeeds <- unlist(lapply(rfeControl$seeds, length))
        badSeed <- (length(rfeControl$seeds) < length(rfeControl$index) + 1) ||
          (any(numSeeds[-length(numSeeds)] < totalSize))
        if(badSeed) stop(paste("Bad seeds: the seed object should be a list of length",
                               length(rfeControl$index) + 1, "with",
                               length(rfeControl$index), "integer vectors of size",
                               totalSize, "and the last list element having a",
                               "single integer"))
      }
    }

    if(rfeControl$method == "LOOCV")
    {
      tmp <- looRfeWorkflow(x, y, sizes, ppOpts = NULL, ctrl = rfeControl, lev = classLevels, ...)
      selectedVars <- do.call("c", tmp$everything[names(tmp$everything) == "finalVariables"])
      selectedVars <- do.call("rbind", selectedVars)
      externPerf <- tmp$performance
    } else {
      tmp <- nominalRfeWorkflow(x, y, sizes, ppOpts = NULL, ctrl = rfeControl, lev = classLevels, ...)
      selectedVars <- do.call("rbind", tmp$everything[names(tmp$everything) == "selectedVars"])
      resamples <- do.call("rbind", tmp$everything[names(tmp$everything) == "resamples"])
      rownames(resamples) <- NULL
      externPerf <- tmp$performance
    }
    rownames(selectedVars) <- NULL

    bestSubset <- rfeControl$functions$selectSize(x = externPerf,
                                                  metric = metric,
                                                  maximize = maximize)

    bestVar <- rfeControl$functions$selectVar(selectedVars, bestSubset)

    finalTime <- system.time(
      fit <- rfeControl$functions$fit(x[, bestVar, drop = FALSE],
                                      y,
                                      first = FALSE,
                                      last = TRUE,
                                      ...))


    if(is.factor(y) & any(names(tmp$performance) == ".cell1"))
    {
      keepers <- c("Resample", "Variables", grep("\\.cell", names(tmp$performance), value = TRUE))
      resampledCM <- subset(tmp$performance, Variables == bestSubset)
      tmp$performance <- tmp$performance[, -grep("\\.cell", names(tmp$performance))]
    } else resampledCM <- NULL

    if(!(rfeControl$method %in% c("LOOCV"))) {
      resamples <- switch(rfeControl$returnResamp,
                          none = NULL,
                          all = resamples,
                          final = subset(resamples, Variables == bestSubset))
    } else resamples <- NULL

    endTime <- proc.time()
    times <- list(everything = endTime - startTime,
                  final = finalTime)

    #########################################################################
    ## Now, based on probability or static ranking, figure out the best vars
    ## and the best subset size and fit final model

    out <- structure(
      list(
        pred = if(rfeControl$saveDetails) do.call("rbind", tmp$everything[names(tmp$everything) == "predictions"]) else NULL,
        variables = selectedVars,
        results = as.data.frame(externPerf, stringsAsFactors = TRUE),
        bestSubset = bestSubset,
        fit = fit,
        optVariables = bestVar,
        optsize = bestSubset,
        call = funcCall,
        control = rfeControl,
        resample = resamples,
        metric = metric,
        maximize = maximize,
        perfNames = perfNames,
        times = times,
        resampledCM = resampledCM,
        obsLevels = classLevels,
        dots = list(...)),
      class = "rfe")
    if(rfeControl$timingSamps > 0)
    {
      out$times$prediction <- system.time(predict(out, x[1:min(nrow(x), rfeControl$timingSamps),,drop = FALSE]))
    } else  out$times$prediction <- rep(NA, 3)
    out
  }

#' @method rfe formula
#' @inheritParams train
#' @importFrom stats .getXlevels contrasts model.matrix model.response
#' @rdname rfe
#' @export
rfe.formula <- function (form, data, ..., subset, na.action, contrasts = NULL)
{
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval.parent(m$data))) m$data <- as.data.frame(data, stringsAsFactors = TRUE)
  m$... <- m$contrasts <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  xint <- match("(Intercept)", colnames(x), nomatch = 0)
  if (xint > 0)  x <- x[, -xint, drop = FALSE]
  y <- model.response(m)
  res <- rfe(as.data.frame(x, stringsAsFactors = TRUE), y, ...)
  res$terms <- Terms
  res$coefnames <- colnames(x)
  res$call <- match.call()
  res$na.action <- attr(m, "na.action")
  res$contrasts <- cons
  res$xlevels <- .getXlevels(Terms, m)
  class(res) <- c("rfe", "rfe.formula")
  res
}

######################################################################
######################################################################
#' @method print rfe
#' @export
print.rfe <- function(x, top = 5, digits = max(3, getOption("digits") - 3), ...)
{

  cat("\nRecursive feature selection\n\n")

  resampleN <- unlist(lapply(x$control$index, length))
  numResamp <- length(resampleN)

  resampText <- resampName(x)
  cat("Outer resampling method:", resampText, "\n")

  cat("\nResampling performance over subset size:\n\n")
  x$results$Selected <- ""
  x$results$Selected[x$results$Variables == x$bestSubset] <- "*"
  print(format(x$results, digits = digits), row.names = FALSE)
  cat("\n")

  cat("The top ",
      min(top, x$bestSubset),
      " variables (out of ",
      x$bestSubset,
      "):\n   ",
      paste(x$optVariables[1:min(top, x$bestSubset)], collapse = ", "),
      "\n\n",
      sep = "")

  invisible(x)
}

######################################################################
######################################################################

#' @rdname rfe
#' @importFrom stats complete.cases
#' @importFrom utils flush.console
#' @export
rfeIter <- function(x, y,
                    testX, testY, sizes,
                    rfeControl = rfeControl(),
                    label = "",
                    seeds = NA,
                    ...)
{
  if(is.null(colnames(x))) stop("x must have column names")

  if(is.null(testX) | is.null(testY)) stop("a test set must be specified")
  if(is.null(sizes)) stop("please specify the number of features")

  predictionMatrix <- matrix(NA, nrow = length(testY), ncol = length(sizes))
  p <- ncol(x)

  retained <- colnames(x)
  sizeValues <- sort(unique(c(sizes, ncol(x))), decreasing = TRUE)
  sizeText <- format(sizeValues)

  finalVariables <- vector(length(sizeValues), mode = "list")
  for(k in seq(along = sizeValues))
  {
    if(!any(is.na(seeds))) set.seed(seeds[k])
    if(rfeControl$verbose)
    {
      cat("+(rfe) fit",
          ifelse(label != "",
                 label, ""),
          "size:",  sizeText[k], "\n")
    }
    flush.console()
    fitObject <- rfeControl$functions$fit(x[,retained,drop = FALSE], y,
                                          first = p == ncol(x[,retained,drop = FALSE]),
                                          last = FALSE,
                                          ...)
    if(rfeControl$verbose)
    {
      cat("-(rfe) fit",
          ifelse(label != "",
                 label, ""),
          "size:",  sizeText[k], "\n")
    }
    modelPred <- rfeControl$functions$pred(fitObject, testX[,retained,drop = FALSE])
    if(is.data.frame(modelPred) | is.matrix(modelPred))
    {
      if(is.matrix(modelPred)) {
        modelPred <- as.data.frame(modelPred, stringsAsFactors = TRUE)
        ## in the case where the function returns a matrix with a single column
        ## make sure that it is named pred
        if(ncol(modelPred) == 1) names(modelPred) <- "pred"
      }
      modelPred$obs <- testY
      modelPred$Variables <- sizeValues[k]
    } else modelPred <- data.frame(pred = modelPred, obs = testY, Variables = sizeValues[k])

    ## save as a vector and rbind at end
    rfePred <- if(k == 1) modelPred else rbind(rfePred, modelPred)


    if(!exists("modImp")) ##todo: get away from this since it finds object in other spaces
    {
      if(rfeControl$verbose)
      {
        cat("+(rfe) imp",
            ifelse(label != "",
                   label, ""), "\n")
      }
      modImp <- rfeControl$functions$rank(fitObject, x[,retained,drop = FALSE], y)
      if(rfeControl$verbose)
      {
        cat("-(rfe) imp",
            ifelse(label != "",
                   label, ""), "\n")
      }
    } else {
      if(rfeControl$rerank)
      {
        if(rfeControl$verbose)
        {
          cat("+(rfe) imp",
              ifelse(label != "",
                     label, ""),
              "size:",  sizeText[k], "\n")
        }
        modImp <- rfeControl$functions$rank(fitObject, x[,retained,drop = FALSE], y)
        if(rfeControl$verbose)
        {
          cat("-(rfe) imp",
              ifelse(label != "",
                     label, ""),
              "size:",  sizeText[k], "\n")
        }
      }
    }

    if(nrow(modImp) < sizeValues[k]) {
      msg1 <- paste0("rfe is expecting ", sizeValues[k],
                     " importance values but only has ", nrow(modImp), ". ",
                     "This may be caused by having zero-variance predictors, ",
                     "excessively-correlated predictors, factor predictors ",
                     "that were expanded into dummy variables or you may have ",
                     "failed to drop one of your dummy variables.")
      warning(msg1, call. = FALSE)
      modImp <- repair_rank(modImp, colnames(x))
    }
    if(any(!complete.cases(modImp))){
      warning(paste("There were missing importance values.",
                 "There may be linear dependencies in your predictor variables"),
              call. = FALSE)
    }
    if (!any(names(modImp) == "var")) {
      stop("The importance score data should include a column named `var`.")
    }
    finalVariables[[k]] <- subset(modImp, var %in% retained)
    finalVariables[[k]]$Variables <- sizeValues[[k]]
    if(k < length(sizeValues)) retained <- as.character(modImp$var)[1:sizeValues[k+1]]
  }
  list(finalVariables = finalVariables, pred = rfePred)

}

######################################################################
######################################################################





#' Plot RFE Performance Profiles
#'
#' These functions plot the resampling results for the candidate subset sizes
#' evaluated during the recursive feature elimination (RFE) process
#'
#' These plots show the average performance versus the subset sizes.
#'
#' @aliases plot.rfe ggplot.rfe
#' @param x an object of class \code{\link{rfe}}.
#' @param metric What measure of performance to plot. Examples of possible
#' values are "RMSE", "Rsquared", "Accuracy" or "Kappa". Other values can be
#' used depending on what metrics have been calculated.
#' @param \dots \code{plot} only: specifications to be passed to
#' \code{\link[lattice]{xyplot}}. The function automatically sets some
#' arguments (e.g. axis labels) but passing in values here will over-ride the
#' defaults.
#' @param data an object of class \code{\link{rfe}}.
#' @param output either "data", "ggplot" or "layered". The first returns a data
#' frame while the second returns a simple \code{ggplot} object with no layers.
#' The third value returns a plot with a set of layers.
#' @param mapping,environment unused arguments to make consistent with
#' \pkg{ggplot2} generic method
#' @return a lattice or ggplot object
#' @note We using a recipe as an input, there may be some subset sizes that are
#'  not well-replicated over resamples. The `ggplot` method will only show
#'  subset sizes where at least half of the resamples have associated results.
#' @author Max Kuhn
#' @seealso \code{\link{rfe}}, \code{\link[lattice]{xyplot}},
#' \code{\link[ggplot2]{ggplot}}
#' @references Kuhn (2008), ``Building Predictive Models in R Using the caret''
#' (\doi{10.18637/jss.v028.i05})
#' @keywords hplot
#' @method plot rfe
#' @export
#' @examples
#'
#' \dontrun{
#' data(BloodBrain)
#'
#' x <- scale(bbbDescr[,-nearZeroVar(bbbDescr)])
#' x <- x[, -findCorrelation(cor(x), .8)]
#' x <- as.data.frame(x, stringsAsFactors = TRUE)
#'
#' set.seed(1)
#' lmProfile <- rfe(x, logBBB,
#'                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
#'                  rfeControl = rfeControl(functions = lmFuncs,
#'                                          number = 200))
#' plot(lmProfile)
#' plot(lmProfile, metric = "Rsquared")
#' ggplot(lmProfile)
#' }
#' @export plot.rfe
plot.rfe <- function (x,
                      metric = x$metric,
                      ...) {
  x$results$Selected <- ""
  x$results$Selected[x$results$Variables == x$bestSubset] <- "*"

  results <- x$results[, colnames(x$results) %in% c("Variables", "Selected", metric)]
  metric <- metric[which(metric %in% colnames(results))]

  plotForm <- as.formula(paste(metric, "~ Variables"))
  panel.profile <- function(x, y, groups, ...)
  {
    panel.xyplot(x, y, ...)
    panel.xyplot(x[groups == "*"], y[groups == "*"], pch = 16)
  }
  resampText <- resampName(x, FALSE)
  resampText <- paste(metric, resampText)
  out <- xyplot(plotForm, data = results, groups = Selected, panel =  panel.profile,
                ylab = resampText,
                ...)

  out
}

######################################################################
######################################################################
#' Controlling the Feature Selection Algorithms
#'
#' This function generates a control object that can be used to specify the
#' details of the feature selection algorithms used in this package.
#'
#' More details on this function can be found at
#' \url{http://topepo.github.io/caret/recursive-feature-elimination.html#rfe}.
#'
#' Backwards selection requires function to be specified for some operations.
#'
#' The \code{fit} function builds the model based on the current data set. The
#' arguments for the function must be: \itemize{ \item\code{x} the current
#' training set of predictor data with the appropriate subset of variables
#' \item\code{y} the current outcome data (either a numeric or factor vector)
#' \item\code{first} a single logical value for whether the current predictor
#' set has all possible variables \item\code{last} similar to \code{first}, but
#' \code{TRUE} when the last model is fit with the final subset size and
#' predictors.  \item\code{...}optional arguments to pass to the fit function
#' in the call to \code{rfe} } The function should return a model object that
#' can be used to generate predictions.
#'
#' The \code{pred} function returns a vector of predictions (numeric or
#' factors) from the current model. The arguments are: \itemize{
#' \item\code{object} the model generated by the \code{fit} function
#' \item\code{x} the current set of predictor set for the held-back samples }
#'
#' The \code{rank} function is used to return the predictors in the order of
#' the most important to the least important. Inputs are: \itemize{
#' \item\code{object} the model generated by the \code{fit} function
#' \item\code{x} the current set of predictor set for the training samples
#' \item\code{y} the current training outcomes } The function should return a
#' data frame with a column called \code{var} that has the current variable
#' names. The first row should be the most important predictor etc. Other
#' columns can be included in the output and will be returned in the final
#' \code{rfe} object.
#'
#' The \code{selectSize} function determines the optimal number of predictors
#' based on the resampling output. Inputs for the function are: \itemize{
#' \item\code{x}a matrix with columns for the performance metrics and the
#' number of variables, called "\code{Variables}" \item\code{metric}a character
#' string of the performance measure to optimize (e.g. "RMSE", "Rsquared",
#' "Accuracy" or "Kappa") \item\code{maximize}a single logical for whether the
#' metric should be maximized } This function should return an integer
#' corresponding to the optimal subset size. \pkg{caret} comes with two
#' examples functions for this purpose: \code{\link{pickSizeBest}} and
#' \code{\link{pickSizeTolerance}}.
#'
#' After the optimal subset size is determined, the \code{selectVar} function
#' will be used to calculate the best rankings for each variable across all the
#' resampling iterations. Inputs for the function are: \itemize{ \item\code{y}
#' a list of variables importance for each resampling iteration and each subset
#' size (generated by the user-defined \code{rank} function). In the example,
#' each each of the cross-validation groups the output of the \code{rank}
#' function is saved for each of the subset sizes (including the original
#' subset). If the rankings are not recomputed at each iteration, the values
#' will be the same within each cross-validation iteration.  \item\code{size}
#' the integer returned by the \code{selectSize} function } This function
#' should return a character string of predictor names (of length \code{size})
#' in the order of most important to least important
#'
#' Examples of these functions are included in the package:
#' \code{\link{lmFuncs}}, \code{\link{rfFuncs}}, \code{\link{treebagFuncs}} and
#' \code{\link{nbFuncs}}.
#'
#' Model details about these functions, including examples, are at
#' \url{http://topepo.github.io/caret/recursive-feature-elimination.html}. .
#'
#' @param functions a list of functions for model fitting, prediction and
#' variable importance (see Details below)
#' @param rerank a logical: should variable importance be re-calculated each
#' time features are removed?
#' @param method The external resampling method: \code{boot}, \code{cv},
#' \code{LOOCV} or \code{LGOCV} (for repeated training/test splits
#' @param number Either the number of folds or number of resampling iterations
#' @param repeats For repeated k-fold cross-validation only: the number of
#' complete sets of folds to compute
#' @param saveDetails a logical to save the predictions and variable
#' importances from the selection process
#' @param verbose a logical to print a log for each external resampling
#' iteration
#' @param returnResamp A character string indicating how much of the resampled
#' summary metrics should be saved. Values can be ``final'', ``all'' or
#' ``none''
#' @param p For leave-group out cross-validation: the training percentage
#' @param index a list with elements for each external resampling iteration.
#' Each list element is the sample rows used for training at that iteration.
#' @param indexOut a list (the same length as \code{index}) that dictates which
#' sample are held-out for each resample. If \code{NULL}, then the unique set
#' of samples not contained in \code{index} is used.
#' @param timingSamps the number of training set samples that will be used to
#' measure the time for predicting samples (zero indicates that the prediction
#' time should not be estimated).
#' @param seeds an optional set of integers that will be used to set the seed
#' at each resampling iteration. This is useful when the models are run in
#' parallel. A value of \code{NA} will stop the seed from being set within the
#' worker processes while a value of \code{NULL} will set the seeds using a
#' random set of integers. Alternatively, a list can be used. The list should
#' have \code{B+1} elements where \code{B} is the number of resamples. The
#' first \code{B} elements of the list should be vectors of integers of length
#' \code{P} where \code{P} is the number of subsets being evaluated (including
#' the full set). The last element of the list only needs to be a single
#' integer (for the final model). See the Examples section below.
#' @param allowParallel if a parallel backend is loaded and available, should
#' the function use it?
#' @return A list
#' @author Max Kuhn
#' @seealso \code{\link{rfe}}, \code{\link{lmFuncs}}, \code{\link{rfFuncs}},
#' \code{\link{treebagFuncs}}, \code{\link{nbFuncs}},
#' \code{\link{pickSizeBest}}, \code{\link{pickSizeTolerance}}
#' @keywords utilities
#' @examples
#'
#'   \dontrun{
#' subsetSizes <- c(2, 4, 6, 8)
#' set.seed(123)
#' seeds <- vector(mode = "list", length = 51)
#' for(i in 1:50) seeds[[i]] <- sample.int(1000, length(subsetSizes) + 1)
#' seeds[[51]] <- sample.int(1000, 1)
#'
#' set.seed(1)
#' rfMod <- rfe(bbbDescr, logBBB,
#'              sizes = subsetSizes,
#'              rfeControl = rfeControl(functions = rfFuncs,
#'                                      seeds = seeds,
#'                                      number = 50))
#'   }
#'
#' @export rfeControl
rfeControl <- function(functions = NULL,
                       rerank = FALSE,
                       method = "boot",
                       saveDetails = FALSE,
                       number = ifelse(method %in% c("cv", "repeatedcv"), 10, 25),
                       repeats = ifelse(method %in% c("cv", "repeatedcv"), 1, number),
                       verbose = FALSE,
                       returnResamp = "final",
                       p = .75,
                       index = NULL,
                       indexOut = NULL,
                       timingSamps = 0,
                       seeds = NA,
                       allowParallel = TRUE)
{
  list(
    functions = if(is.null(functions)) caretFuncs else functions,
    rerank = rerank,
    method = method,
    saveDetails = saveDetails,
    number = number,
    repeats = repeats,
    returnResamp = returnResamp,
    verbose = verbose,
    p = p,
    index = index,
    indexOut = indexOut,
    timingSamps = timingSamps,
    seeds = seeds,
    allowParallel = allowParallel)
}

######################################################################
######################################################################
## some built-in functions for certain models

#' @rdname caretFuncs
#' @export
pickSizeBest <- function(x, metric, maximize)
{
  best <- if(maximize) which.max(x[,metric]) else which.min(x[,metric])
  min(x[best, "Variables"])
}

#' @rdname caretFuncs
#' @export
pickSizeTolerance <- function(x, metric, tol = 1.5, maximize)
{
  if(!maximize)
  {
    best <- min(x[,metric])
    perf <- (x[,metric] - best)/best * 100
    flag <- perf <= tol
  } else {
    best <- max(x[,metric])
    perf <- (best - x[,metric])/best * 100
    flag <- perf <= tol
  }
  min(x[flag, "Variables"])
}

#' @rdname caretFuncs
#' @export
pickVars <- function(y, size)
{
  finalImp <- ddply(y[, c("Overall", "var")],
                    .(var),
                    function(x) mean(x$Overall, na.rm = TRUE))
  names(finalImp)[2] <- "Overall"
  finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE),]
  as.character(finalImp$var[1:size])
}




#' Backwards Feature Selection Helper Functions
#'
#' Ancillary functions for backwards selection
#'
#' This page describes the functions that are used in backwards selection (aka
#' recursive feature elimination). The functions described here are passed to
#' the algorithm via the \code{functions} argument of \code{\link{rfeControl}}.
#'
#' See \code{\link{rfeControl}} for details on how these functions should be
#' defined.
#'
#' The 'pick' functions are used to find the appropriate subset size for
#' different situations. \code{pickBest} will find the position associated with
#' the numerically best value (see the \code{maximize} argument to help define
#' this).
#'
#' \code{pickSizeTolerance} picks the lowest position (i.e. the smallest subset
#' size) that has no more of an X percent loss in performances. When
#' maximizing, it calculates (O-X)/O*100, where X is the set of performance
#' values and O is max(X). This is the percent loss. When X is to be minimized,
#' it uses (X-O)/O*100 (so that values greater than X have a positive "loss").
#' The function finds the smallest subset size that has a percent loss less
#' than \code{tol}.
#'
#' Both of the 'pick' functions assume that the data are sorted from smallest
#' subset size to largest.
#'
#' @aliases caretFuncs lmFuncs rfFuncs gamFuncs treebagFuncs ldaFuncs nbFuncs
#' lrFuncs pickSizeBest pickSizeTolerance pickVars
#' @param x a matrix or data frame with the performance metric of interest
#' @param metric a character string with the name of the performance metric
#' that should be used to choose the appropriate number of variables
#' @param maximize a logical; should the metric be maximized?
#' @param tol a scalar to denote the acceptable difference in optimal
#' performance (see Details below)
#' @param y a list of data frames with variables \code{Overall} and \code{var}
#' @param size an integer for the number of variables to retain
#' @author Max Kuhn
#' @seealso \code{\link{rfeControl}}, \code{\link{rfe}}
#' @keywords models
#' @examples
#'
#' ## For picking subset sizes:
#' ## Minimize the RMSE
#' example <- data.frame(RMSE = c(1.2, 1.1, 1.05, 1.01, 1.01, 1.03, 1.00),
#'                       Variables = 1:7)
#' ## Percent Loss in performance (positive)
#' example$PctLoss <- (example$RMSE - min(example$RMSE))/min(example$RMSE)*100
#'
#' xyplot(RMSE ~ Variables, data= example)
#' xyplot(PctLoss ~ Variables, data= example)
#'
#' absoluteBest <- pickSizeBest(example, metric = "RMSE", maximize = FALSE)
#' within5Pct <- pickSizeTolerance(example, metric = "RMSE", maximize = FALSE)
#'
#' cat("numerically optimal:",
#'     example$RMSE[absoluteBest],
#'     "RMSE in position",
#'     absoluteBest, "\n")
#' cat("Accepting a 1.5 pct loss:",
#'     example$RMSE[within5Pct],
#'     "RMSE in position",
#'     within5Pct, "\n")
#'
#' ## Example where we would like to maximize
#' example2 <- data.frame(Rsquared = c(0.4, 0.6, 0.94, 0.95, 0.95, 0.95, 0.95),
#'                       Variables = 1:7)
#' ## Percent Loss in performance (positive)
#' example2$PctLoss <- (max(example2$Rsquared) - example2$Rsquared)/max(example2$Rsquared)*100
#'
#' xyplot(Rsquared ~ Variables, data= example2)
#' xyplot(PctLoss ~ Variables, data= example2)
#'
#' absoluteBest2 <- pickSizeBest(example2, metric = "Rsquared", maximize = TRUE)
#' within5Pct2 <- pickSizeTolerance(example2, metric = "Rsquared", maximize = TRUE)
#'
#' cat("numerically optimal:",
#'     example2$Rsquared[absoluteBest2],
#'     "R^2 in position",
#'     absoluteBest2, "\n")
#' cat("Accepting a 1.5 pct loss:",
#'     example2$Rsquared[within5Pct2],
#'     "R^2 in position",
#'     within5Pct2, "\n")
#'
#' @export caretFuncs
caretFuncs <- list(summary = defaultSummary,
                   fit = function(x, y, first, last, ...) train(x, y, ...),
                   pred = function(object, x) {
                     tmp <- predict(object, x)
                     if(object$modelType == "Classification" & object$control$classProbs) {
                       out <- cbind(data.frame(pred = tmp),
                                    as.data.frame(predict(object, x, type = "prob"), stringsAsFactors = TRUE), stringsAsFactors = TRUE)
                     } else out <- tmp
                     out
                   },
                   rank = function(object, x, y) {
                     vimp <- varImp(object, scale = FALSE)$importance
                     if(!is.data.frame(vimp)) vimp <- as.data.frame(vimp, stringsAsFactors = TRUE)
                     if(object$modelType == "Regression") {
                       vimp <- vimp[order(vimp[,1], decreasing = TRUE),,drop = FALSE]
                     } else {
                       if(all(levels(y) %in% colnames(vimp)) & !("Overall" %in% colnames(vimp))) {
                         avImp <- apply(vimp[, levels(y), drop = TRUE], 1, mean)
                         vimp$Overall <- avImp
                       }
                     }
                     vimp <- vimp[order(vimp$Overall, decreasing = TRUE),, drop = FALSE]
                     vimp$var <- rownames(vimp)
                     vimp
                   },
                   selectSize = pickSizeBest,
                   selectVar = pickVars)

## write a better imp sort function
#' @rdname caretFuncs
#' @importFrom stats predict
#' @export
ldaFuncs <- list(summary = defaultSummary,
                 fit = function(x, y, first, last, ...)
                 {
                   loadNamespace("MASS")
                   MASS::lda(x, y, ...)
                 },
                 pred = function(object, x)
                 {
                   tmp <- predict(object, x)
                   out <- cbind(data.frame(pred = tmp$class),
                                as.data.frame(tmp$posterior, stringsAsFactors = FALSE), stringsAsFactors = TRUE)
                   out
                 },
                 rank = function(object, x, y)
                 {
                   vimp <- filterVarImp(x, y, TRUE)

                   vimp$Overall <- apply(vimp, 1, mean)
                   vimp <- vimp[order(vimp$Overall, decreasing = TRUE),]

                   vimp <- as.data.frame(vimp, stringsAsFactors = TRUE)[, "Overall",drop = FALSE]
                   vimp$var <- rownames(vimp)
                   vimp

                 },
                 selectSize = pickSizeBest,
                 selectVar = pickVars
)

#' @rdname caretFuncs
#' @importFrom stats predict
#' @export
treebagFuncs <- list(summary = defaultSummary,
                     fit = function(x, y, first, last, ...) {
                       loadNamespace("ipred")
                       ipred::ipredbagg(y, x, ...)
                     },
                     pred = function(object, x) {
                       tmp <- predict(object, x)
                       if(is.factor(object$y)) {
                         out <- cbind(data.frame(pred = tmp),
                                      as.data.frame(predict(object, x, type = "prob"), stringsAsFactors = TRUE), stringsAsFactors = TRUE)
                       } else out <- tmp
                       out
                     },
                     rank = function(object, x, y) {
                       vimp <- varImp(object)
                       vimp <- vimp[order(vimp$Overall, decreasing = TRUE),,drop = FALSE]
                       vimp$var <- rownames(vimp)
                       vimp
                     },
                     selectSize = pickSizeBest,
                     selectVar = pickVars)

#' @rdname caretFuncs
#' @importFrom stats predict
#' @export
gamFuncs <- list(summary = defaultSummary,
                 fit = function(x, y, first, last, ...){
                   loaded <- search()
                   gamLoaded <- any(loaded == "package:gam")
                   if(gamLoaded) detach(package:gam)
                   loadNamespace("mgcv")
                   gam <- get("gam", asNamespace("mgcv"))
                   dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                   dat$y <- y
                   args <- list(formula = gamFormula(x, smoother = "s", y = "y"),
                                data = dat,
                                family = if(!is.factor(y)) gaussian else  binomial)
                   do.call("gam", args)
                 },
                 pred = function(object, x) {
                   if(!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
                   loaded <- search()
                   gamLoaded <- any(loaded == "package:gam")
                   if(gamLoaded) detach(package:gam)
                   loadNamespace("mgcv")
                   rsp <- predict(object, newdata = x, type = "response")
                   if(object$family$family == "binomial") {
                     lvl <- levels(object$model$y)
                     out <- data.frame(p1 = rsp,
                                       p2 = 1-rsp,
                                       pred = factor(ifelse(rsp > .5, lvl[2], lvl[1]),
                                                     levels = lvl))
                     colnames(out)[1:2] <- make.names(lvl)
                     out
                   } else out <- data.frame(pred = rsp)
                   out

                 },
                 rank = function(object, x, y) {

                   loaded <- search()
                   gamLoaded <- any(loaded == "package:gam")
                   if(gamLoaded) detach(package:gam)
                   loadNamespace("mgcv")
                   vimp <- varImp(object)
                   vimp$var <- rownames(vimp)
                   if(any(!(colnames(x) %in% rownames(vimp)))) {
                     missing <- colnames(x)[!(colnames(x) %in% rownames(vimp))]
                     tmpdf <- data.frame(var = missing,
                                         Overall = rep(0, length(missing)))
                     vimp <- rbind(vimp, tmpdf)
                   }
                   vimp <- vimp[order(vimp$Overall, decreasing = TRUE),, drop = FALSE]
                   vimp
                 },
                 selectSize = pickSizeBest,
                 selectVar = pickVars)

#' @rdname caretFuncs
#' @importFrom stats predict
#' @export
rfFuncs <-  list(summary = defaultSummary,
                 fit = function(x, y, first, last, ...) {
                   loadNamespace("randomForest")
                   randomForest::randomForest(x, y, importance = TRUE, ...)
                 },
                 pred = function(object, x)  {
                   tmp <- predict(object, x)
                   if(is.factor(object$y)) {
                     out <- cbind(data.frame(pred = tmp),
                                  as.data.frame(predict(object, x, type = "prob"),
                                                stringsAsFactors = TRUE))
                   } else out <- tmp
                   out
                 },
                 rank = function(object, x, y) {
                   vimp <- varImp(object)

                   if(is.factor(y)) {
                     if(all(levels(y) %in% colnames(vimp))) {
                       avImp <- apply(vimp[, levels(y), drop = TRUE], 1, mean)
                       vimp$Overall <- avImp
                     }
                   }

                   vimp <- vimp[order(vimp$Overall, decreasing = TRUE),, drop = FALSE]
                   if (ncol(x) == 1) {
                     vimp$var <- colnames(x)
                   } else vimp$var <- rownames(vimp)
                   vimp
                 },
                 selectSize = pickSizeBest,
                 selectVar = pickVars)


#' @rdname caretFuncs
#' @importFrom stats predict lm
#' @export
lmFuncs <- list(summary = defaultSummary,
                fit = function(x, y, first, last, ...) {
                  tmp <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                  tmp$y <- y
                  lm(y~., data = tmp)
                },
                pred = function(object, x) {
                  if(!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
                  predict(object, x)
                },
                rank = function(object, x, y) {
                  coefs <- abs(coef(object))
                  coefs <- coefs[names(coefs) != "(Intercept)"]
                  coefs[is.na(coefs)] <- 0
                  vimp <- data.frame(Overall = unname(coefs),
                                     var = names(coefs))
                  rownames(vimp) <- names(coefs)
                  vimp <- vimp[order(vimp$Overall, decreasing = TRUE),, drop = FALSE]
                  vimp
                },
                selectSize = pickSizeBest,
                selectVar = pickVars)


#' @rdname caretFuncs
#' @importFrom stats predict
#' @export
nbFuncs <- list(summary = defaultSummary,
                fit = function(x, y, first, last, ...){
                  loadNamespace("klaR")
                  klaR::NaiveBayes(x, y, usekernel = TRUE, fL = 2, ...)
                },
                pred = function(object, x) {
                  tmp <- predict(object, x)
                  out <- cbind(data.frame(pred = tmp$class),
                               as.data.frame(tmp$posterior, stringsAsFactors = TRUE))
                  out
                },
                rank = function(object, x, y) {
                  vimp <- filterVarImp(x, y)
                  if(is.factor(y)) {
                    avImp <- apply(vimp, 1, mean)
                    vimp$Overall <- avImp
                  }

                  vimp <- vimp[order(vimp$Overall,decreasing = TRUE),, drop = FALSE]

                  vimp$var <- rownames(vimp)
                  vimp
                },
                selectSize = pickSizeBest,
                selectVar = pickVars)


#' @rdname caretFuncs
#' @importFrom stats predict glm
#' @export
lrFuncs <- ldaFuncs
lrFuncs$fit <- function (x, y, first, last, ...)  {
  tmp <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
  tmp$Class <- y
  glm(Class ~ ., data = tmp, family = "binomial")
}
lrFuncs$pred <- function (object, x) {
  if(!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
  lvl <- levels(object$data$Class)
  tmp <- predict(object, x, type = "response")
  out <- data.frame(1-tmp, tmp)
  colnames(out) <- lvl
  out$pred <- factor(ifelse(tmp > .5, lvl[2], lvl[1]),
                     levels = lvl)
  out
}

lrFuncs$rank <- function (object, x, y) {
  vimp <- varImp(object, scale = FALSE)
  vimp <- vimp[order(vimp$Overall, decreasing = TRUE),, drop = FALSE]
  vimp$var <- rownames(vimp)
  vimp
}

######################################################################
######################################################################
## lattice functions

#' Lattice functions for plotting resampling results of recursive feature
#' selection
#'
#' A set of lattice functions are provided to plot the resampled performance
#' estimates (e.g. classification accuracy, RMSE) over different subset sizes.
#'
#' By default, only the resampling results for the optimal model are saved in
#' the \code{rfe} object. The function \code{\link{rfeControl}} can be used to
#' save all the results using the \code{returnResamp} argument.
#'
#' If leave-one-out or out-of-bag resampling was specified, plots cannot be
#' produced (see the \code{method} argument of \code{\link{rfeControl}})
#'
#' @aliases xyplot.rfe stripplot.rfe densityplot.rfe histogram.rfe
#' @param x An object produced by \code{\link{rfe}}
#' @param data This argument is not used
#' @param metric A character string specifying the single performance metric
#' that will be plotted
#' @param \dots arguments to pass to either
#' \code{\link[lattice:histogram]{histogram}},
#' \code{\link[lattice:histogram]{densityplot}},
#' \code{\link[lattice:xyplot]{xyplot}} or
#' \code{\link[lattice:xyplot]{stripplot}}
#' @return A lattice plot object
#' @author Max Kuhn
#' @seealso \code{\link{rfe}}, \code{\link{rfeControl}},
#' \code{\link[lattice:histogram]{histogram}},
#' \code{\link[lattice:histogram]{densityplot}},
#' \code{\link[lattice:xyplot]{xyplot}},
#' \code{\link[lattice:xyplot]{stripplot}}
#' @keywords hplot
#' @examples
#'
#' \dontrun{
#' library(mlbench)
#' n <- 100
#' p <- 40
#' sigma <- 1
#' set.seed(1)
#' sim <- mlbench.friedman1(n, sd = sigma)
#' x <- cbind(sim$x,  matrix(rnorm(n * p), nrow = n))
#' y <- sim$y
#' colnames(x) <- paste("var", 1:ncol(x), sep = "")
#'
#' normalization <- preProcess(x)
#' x <- predict(normalization, x)
#' x <- as.data.frame(x, stringsAsFactors = TRUE)
#' subsets <- c(10, 15, 20, 25)
#'
#' ctrl <- rfeControl(
#'                    functions = lmFuncs,
#'                    method = "cv",
#'                    verbose = FALSE,
#'                    returnResamp = "all")
#'
#' lmProfile <- rfe(x, y,
#'                  sizes = subsets,
#'                  rfeControl = ctrl)
#' xyplot(lmProfile)
#' stripplot(lmProfile)
#'
#' histogram(lmProfile)
#' densityplot(lmProfile)
#' }
#'
#' @importFrom stats as.formula
#' @export
densityplot.rfe <- function(x,
                            data = NULL,
                            metric = x$metric,
                            ...)
{
  if (!is.null(match.call()$data))
    warning("explicit 'data' specification ignored")

  if(x$control$method %in%  c("oob", "LOOCV"))
    stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

  data <- as.data.frame(x$resample, stringsAsFactors = TRUE)
  data$Variable <- factor(data$Variable,
                          levels = paste(sort(unique(data$Variable))))

  form <- as.formula(paste("~", metric, "|Variable"))
  densityplot(form, data = data, ...)
}

#' @importFrom stats as.formula
#' @export
histogram.rfe <- function(x,
                          data = NULL,
                          metric = x$metric,
                          ...)
{
  if (!is.null(match.call()$data))
    warning("explicit 'data' specification ignored")

  if(x$control$method %in%  c("oob", "LOOCV"))
    stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

  data <- as.data.frame(x$resample, stringsAsFactors = TRUE)
  data$Variable <- factor(data$Variable,
                          levels = paste(sort(unique(data$Variable))))

  form <- as.formula(paste("~", metric, "|Variable"))
  histogram(form, data = data, ...)
}

#' @importFrom stats as.formula
#' @export
stripplot.rfe <- function(x,
                          data = NULL,
                          metric = x$metric,
                          ...)
{
  if (!is.null(match.call()$data))
    warning("explicit 'data' specification ignored")

  if(x$control$method %in%  c("oob", "LOOCV"))
    stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

  data <- as.data.frame(x$resample, stringsAsFactors = TRUE)
  data$Variable <- factor(data$Variable,
                          levels = paste(sort(unique(data$Variable))))
  theDots <- list(...)
  if(any(names(theDots) == "horizontal"))
  {
    formText <- if(theDots$horizontal) paste("Variable ~", metric) else paste(metric, "~ Variable")
  } else  formText <- paste("Variable ~", metric)

  form <- as.formula(formText)

  stripplot(form, data = data, ...)

}

#' @importFrom stats as.formula
#' @export
xyplot.rfe <- function(x,
                       data = NULL,
                       metric = x$metric,
                       ...)
{
  if (!is.null(match.call()$data))
    warning("explicit 'data' specification ignored")

  if(x$control$method %in%  c("oob", "LOOCV"))
    stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

  data <- as.data.frame(x$resample, stringsAsFactors = TRUE)

  form <- as.formula(paste(metric, " ~ Variables"))
  xyplot(form, data = data, ...)
}

######################################################################
######################################################################
## other functions

#' @export
predictors.rfe <- function(x, ...) x$optVariables

#' @export
varImp.rfe <- function(object, drop = FALSE, ...)
{
  imp <- subset(object$variables, Variables == object$optsize)
  imp <- ddply(imp[, c("Overall", "var")], .(var), function(x) mean(x$Overall, rm.na = TRUE))
  names(imp)[2] <- "Overall"

  if(drop) imp <- subset(imp, var %in% object$optVar)
  rownames(imp) <- imp$var
  imp$var <- NULL
  imp[order(-imp$Overall),,drop = FALSE]
}

#' @importFrom stats .checkMFClasses delete.response model.frame model.matrix na.omit
#' @export
predict.rfe <- function(object, newdata, ...) {
  if(length(list(...)) > 0)
    warning("... are ignored for predict.rfe")

  if(inherits(object, "rfe.formula")) {
    newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
    rn <- row.names(newdata)
    Terms <- delete.response(object$terms)
    m <- model.frame(Terms, newdata, na.action = na.omit,
                     xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    keep <- match(row.names(m), rn)
    newdata <- model.matrix(Terms, m, contrasts = object$contrasts)
    xint <- match("(Intercept)", colnames(newdata), nomatch = 0)
    if (xint > 0)  newdata <- newdata[, -xint, drop = FALSE]
  } else {
    if (any(names(object) == "recipe")) {
      newdata <-
        bake(object$recipe, newdata, all_predictors(), composition = "data.frame")
    }
  }
  checkCols <- object$optVar %in% colnames(newdata)
  if(!all(checkCols))
    stop(paste("missing columns from newdata:",
               paste(object$optVar[!checkCols], collapse = ", ")))

  newdata <- newdata[, object$optVar, drop = FALSE]
  object$control$functions$pred(object$fit, newdata)
}

#' @rdname rfe
#' @method update rfe
#' @export
update.rfe <- function(object, x, y, size, ...) {
  size <- size[1]
  selectedVars <- object$variables
  bestVar <- object$control$functions$selectVar(selectedVars, size)

  if (!is.null(object$recipe)) {
    if (is.null(object$recipe$template))
      stop("Recipe is missing data to be juiced.", call. = FALSE)
    args <-
      list(
        x = juice(object$recipe, all_predictors(), composition = "data.frame"),
        y = juice(object$recipe, all_outcomes(), composition = "data.frame"),
        first = FALSE, last = TRUE
      )
    args$y <- args$y[,1]
  } else {
    args <-
      list(x = x, y = y, first = FALSE, last = TRUE)
  }
  args$x <- args$x[, bestVar, drop = FALSE]

  if (length(object$dots) > 0)
    args <- c(args, object$dots)

  object$fit <- do.call(object$control$functions$fit, args)

  object$bestSubset <- size
  object$bestVar <- bestVar

  if (object$control$returnResamp == "final") {
    warning("The saved resamples are no longer appropriate and were removed")
    object$resampledCM <- object$resample <- NULL
  }
  object
}


repair_rank <- function(imp, nms, fill = -Inf) {
  no_val <- !(nms %in% imp$var)
  missing_rows <- imp[rep(1, sum(no_val)),]
  missing_rows$var <- nms[no_val]
  other_col <- colnames(imp)[colnames(imp) != "var"]
  for(i in other_col) missing_rows[, i] <- NA
  out <- rbind(imp, missing_rows)
  rownames(out) <- NULL
  out
}

###################################################################

rfe_rec <- function(x, y, test_x, test_y, perf_dat,
                    sizes, rfeControl = rfeControl(),
                    label = "", seeds = NA, ...) {
  p <- ncol(x)

  if (length(sizes) > 0 && max(sizes) > p)
    sizes <- sizes[sizes <= p]

  if (all(sizes < 2))
    stop(
      "After the recipe, there are less than two predictors remaining. `rfe` ",
      "requires at least two.",
      call. = FALSE
    )

  if (length(sizes) == 0)
    stop(
      "After the recipe, there are only ",
      p,
      " predictors remaining. ",
      "The `sizes` values are inconsistent with this.",
      call. = FALSE
    )

  predictionMatrix <-
    matrix(NA, nrow = length(test_y), ncol = length(sizes))

  retained <- colnames(x)
  sizeValues <- sort(unique(c(sizes, p)), decreasing = TRUE)
  sizeText <- format(sizeValues)

  finalVariables <- vector(length(sizeValues), mode = "list")
  for (k in seq(along = sizeValues)) {
    if (!any(is.na(seeds)))
      set.seed(seeds[k])

    if (rfeControl$verbose) {
      cat("+(rfe) fit",
          ifelse(label != "",
                 label, ""),
          "size:",
          sizeText[k],
          "\n")
    }
    flush.console()
    fitObject <-
      rfeControl$functions$fit(
        x[, retained, drop = FALSE], y,
        first = p == ncol(x[, retained, drop = FALSE]),
        last = FALSE,
        ...
      )
    if (rfeControl$verbose) {
      cat("-(rfe) fit",
          ifelse(label != "",
                 label, ""),
          "size:",
          sizeText[k],
          "\n")
    }
    modelPred <-
      rfeControl$functions$pred(fitObject, test_x[, retained, drop = FALSE])
    if (is.data.frame(modelPred) | is.matrix(modelPred)) {
      if (is.matrix(modelPred)) {
        modelPred <- as.data.frame(modelPred, stringsAsFactors = TRUE)
        ## in the case where the function returns a matrix with a single column
        ## make sure that it is named pred
        if (ncol(modelPred) == 1)
          names(modelPred) <- "pred"
      }
      modelPred$obs <- test_y
      modelPred$Variables <- sizeValues[k]
    } else
      modelPred <-
      data.frame(pred = modelPred,
                 obs = test_y,
                 Variables = sizeValues[k])
    ## save as a vector and rbind at end
    rfePred <- if (k == 1)
      modelPred
    else
      rbind(rfePred, modelPred)


    if (!exists("modImp")) {
      ##todo: get away from this since it finds object in other spaces

      if (rfeControl$verbose){
        cat("+(rfe) imp",
            ifelse(label != "",
                   label, ""), "\n")
      }
      modImp <-
        rfeControl$functions$rank(fitObject, x[, retained, drop = FALSE], y)
      if (rfeControl$verbose){
        cat("-(rfe) imp",
            ifelse(label != "",
                   label, ""), "\n")
      }
    } else {
      if (rfeControl$rerank){
        if (rfeControl$verbose){
          cat("+(rfe) imp",
              ifelse(label != "",
                     label, ""),
              "size:",
              sizeText[k],
              "\n")
        }
        modImp <-
          rfeControl$functions$rank(fitObject, x[, retained, drop = FALSE], y)
        if (rfeControl$verbose){
          cat("-(rfe) imp",
              ifelse(label != "",
                     label, ""),
              "size:",
              sizeText[k],
              "\n")
        }
      }
    }

    if (nrow(modImp) < sizeValues[k]) {
      msg1 <- paste0(
        "rfe is expecting ",
        sizeValues[k],
        " importance values but only has ",
        nrow(modImp),
        ". ",
        "This may be caused by having zero-variance predictors, ",
        "excessively-correlated predictors, factor predictors ",
        "that were expanded into dummy variables or you may have ",
        "failed to drop one of your dummy variables."
      )
      warning(msg1, call. = FALSE)
      modImp <- repair_rank(modImp, colnames(x))
    }
    if (any(!complete.cases(modImp))) {
      warning(
        paste(
          "There were missing importance values.",
          "There may be linear dependencies in your predictor variables"
        ),
        call. = FALSE
      )
    }
    finalVariables[[k]] <- subset(modImp, var %in% retained)
    finalVariables[[k]]$Variables <- sizeValues[[k]]
    if (k < length(sizeValues))
      retained <- as.character(modImp$var)[1:sizeValues[k + 1]]
  }
  list(finalVariables = finalVariables, pred = rfePred)
}

#' @method rfe recipe
#' @rdname rfe
#' @export
"rfe.recipe" <-
  function(x,
           data,
           sizes = 2 ^ (2:4),
           metric = NULL,
           maximize = NULL,
           rfeControl = rfeControl(),
           ...) {
    startTime <- proc.time()
    funcCall <- match.call(expand.dots = TRUE)
    if (!("caret" %in% loadedNamespaces()))
      loadNamespace("caret")

    ###################################################################

    if(rfeControl$verbose)
      cat("Preparing recipe\n")

    trained_rec <- prep(x, training = data,
                        fresh = TRUE,
                        retain = TRUE,
                        verbose = FALSE,
                        stringsAsFactors = TRUE)
    x_dat <- juice(trained_rec, all_predictors(), composition = "data.frame")
    y_dat <- juice(trained_rec, all_outcomes(), composition = "data.frame")
    if(ncol(y_dat) > 1)
      stop("`rfe` doesn't support multivariate outcomes", call. = FALSE)
    y_dat <- y_dat[[1]]
    is_weight <- summary(trained_rec)$role == "case weight"
    if(any(is_weight))
      stop("`rfe` does not allow for weights.", call. = FALSE)

    is_perf <- summary(trained_rec)$role == "performance var"
    if(any(is_perf)) {
      perf_data <- juice(trained_rec, has_role("performance var"))
    } else perf_data <- NULL

    p <- ncol(x_dat)
    classLevels <- levels(y_dat)

    # now do default metrics:
    if (is.null(metric))
      metric <- ifelse(is.factor(y_dat), "Accuracy", "RMSE")

    maximize <-
      ifelse(metric %in% c("RMSE", "MAE", "logLoss"), FALSE, TRUE) # TODO make a function



    if (is.null(rfeControl$index))
      rfeControl$index <- switch(
        tolower(rfeControl$method),
        cv = createFolds(y_dat, rfeControl$number, returnTrain = TRUE),
        repeatedcv = createMultiFolds(y_dat, rfeControl$number, rfeControl$repeats),
        loocv = createFolds(y_dat, length(y_dat), returnTrain = TRUE),
        boot = ,
        boot632 = createResample(y_dat, rfeControl$number),
        test = createDataPartition(y_dat, 1, rfeControl$p),
        lgocv = createDataPartition(y_dat, rfeControl$number, rfeControl$p)
      )

    if (is.null(names(rfeControl$index)))
      names(rfeControl$index) <- prettySeq(rfeControl$index)
    if (is.null(rfeControl$indexOut)) {
      rfeControl$indexOut <- lapply(rfeControl$index,
                                    function(training, allSamples)
                                      allSamples[-unique(training)],
                                    allSamples = seq(along = y_dat))
      names(rfeControl$indexOut) <- prettySeq(rfeControl$indexOut)
    }

    sizes <- sort(unique(sizes))
    if (any(sizes > p))
      warning("For the training set, the recipe generated fewer predictors ",
              "than the ", max(sizes), " expected in `sizes` and the number ",
              "of subsets will be truncated to be <= ", p, ".",
              call. = FALSE)
    sizes <- sizes[sizes <= p]

    ## check summary function and metric
    testOutput <- data.frame(pred = sample(y_dat, min(10, length(y_dat))),
                             obs = sample(y_dat, min(10, length(y_dat))))
    if (is.factor(y_dat)) {
      for (i in seq(along = classLevels))
        testOutput[, classLevels[i]] <- runif(nrow(testOutput))
    }
    if(!is.null(perf_data))
      testOutput <- cbind(testOutput, perf_data)


    test <-
      rfeControl$functions$summary(testOutput, lev = classLevels)
    perfNames <- names(test)

    if (!(metric %in% perfNames)) {
      warning(
        paste(
          "Metric '",
          metric,
          "' is not created by the summary function; '",
          perfNames[1],
          "' will be used instead",
          sep = ""
        )
      )
      metric <- perfNames[1]
    }

    ## Set or check the seeds when needed
    totalSize <-
      if (any(sizes == p))
        length(sizes)
    else
      length(sizes) + 1
    if (is.null(rfeControl$seeds)) {
      seeds <- vector(mode = "list", length = length(rfeControl$index))
      seeds <-
        lapply(seeds, function(x)
          sample.int(n = 1000000, size = totalSize))
      seeds[[length(rfeControl$index) + 1]] <-
        sample.int(n = 1000000, size = 1)
      rfeControl$seeds <- seeds
    } else {
      if (!(length(rfeControl$seeds) == 1 && is.na(rfeControl$seeds))) {
        ## check versus number of tasks
        numSeeds <- unlist(lapply(rfeControl$seeds, length))
        badSeed <-
          (length(rfeControl$seeds) < length(rfeControl$index) + 1) ||
          (any(numSeeds[-length(numSeeds)] < totalSize))
        if (badSeed)
          stop(
            paste(
              "Bad seeds: the seed object should be a list of length",
              length(rfeControl$index) + 1,
              "with",
              length(rfeControl$index),
              "integer vectors of size",
              totalSize,
              "and the last list element having a",
              "single integer"
            )
          )
      }
    }

    if (rfeControl$method == "LOOCV") {
      tmp <-
        rfe_rec_loo(
          rec = x,
          data = data,
          sizes = sizes,
          ctrl = rfeControl,
          lev = classLevels,
          ...
        )
      selectedVars <-
        do.call("c", tmp$everything[names(tmp$everything) == "finalVariables"])
      selectedVars <- do.call("rbind", selectedVars)
      externPerf <- tmp$performance
    } else {
      tmp <-
        rfe_rec_workflow(
          rec = x,
          data = data,
          sizes = sizes,
          ctrl = rfeControl,
          lev = classLevels,
          ...
        )

      selectedVars <-
        do.call("rbind", tmp$everything[names(tmp$everything) == "selectedVars"])
      resamples <-
        do.call("rbind", tmp$everything[names(tmp$everything) == "resamples"])
      rownames(resamples) <- NULL
      externPerf <- tmp$performance
    }
    rownames(selectedVars) <- NULL

    ## There may be variables selected that are not generated by the recipe
    ## created on the traning set.

    all_var <- as.character(unique(selectedVars$var))
    x_names <- colnames(x_dat)
    orphans <- all_var[!(all_var %in% x_names)]

    externPerf <- subset(externPerf, Variables <= length(x_names))

    numResamples <- length(rfeControl$index)
    bestSubset <-
      rfeControl$functions$selectSize(
        x = subset(externPerf, Num_Resamples >= floor(.5*numResamples)),
        metric = metric,
        maximize = maximize
      )

    bestVar <-
      rfeControl$functions$selectVar(subset(selectedVars, var %in% x_names), bestSubset)
    # In case of orpahns:
    bestVar <- bestVar[!is.na(bestVar)]
    bestSubset <- length(bestVar)

    finalTime <-
      system.time(
        fit <- rfeControl$functions$fit(
          x_dat[, bestVar, drop = FALSE],
          y_dat,
          first = FALSE,
          last = TRUE,
          ...
        )
      )

    if (is.factor(y_dat) & any(names(tmp$performance) == ".cell1")) {
      keepers <-
        c("Resample",
          "Variables",
          grep("\\.cell", names(tmp$performance), value = TRUE))
      resampledCM <-
        subset(tmp$performance, Variables == bestSubset)
      tmp$performance <-
        tmp$performance[,-grep("\\.cell", names(tmp$performance))]
    } else
      resampledCM <- NULL

    if (!(rfeControl$method %in% c("LOOCV"))) {
      resamples <- switch(
        rfeControl$returnResamp,
        none = NULL,
        all = resamples,
        final = subset(resamples, Variables == bestSubset)
      )
    } else
      resamples <- NULL

    endTime <- proc.time()
    times <- list(everything = endTime - startTime,
                  final = finalTime)

    #########################################################################
    ## Now, based on probability or static ranking, figure out the best vars
    ## and the best subset size and fit final model

    out <- structure(
      list(
        pred = if (rfeControl$saveDetails)
          do.call("rbind", tmp$everything[names(tmp$everything) == "predictions"])
        else
          NULL,
        variables = selectedVars,
        results = as.data.frame(externPerf, stringsAsFactors = FALSE),
        bestSubset = bestSubset,
        fit = fit,
        optVariables = bestVar,
        optsize = bestSubset,
        call = funcCall,
        control = rfeControl,
        resample = resamples,
        metric = metric,
        maximize = maximize,
        perfNames = perfNames,
        times = times,
        resampledCM = resampledCM,
        obsLevels = classLevels,
        dots = list(...),
        recipe = trained_rec
      ),
      class = "rfe"
    )
    if (rfeControl$timingSamps > 0) {
      out$times$prediction <-
        system.time(predict(out, x_dat[1:min(nrow(x_dat), rfeControl$timingSamps), , drop = FALSE]))
    } else
      out$times$prediction <- rep(NA, 3)
    out
  }


rfe_rec_workflow <- function(rec, data, sizes, ctrl, lev, ...) {
  loadNamespace("caret")
  loadNamespace("recipes")

  resampleIndex <- ctrl$index
  if (ctrl$method %in% c("boot632")) {
    resampleIndex <- c(list("AllData" = rep(0, nrow(data))), resampleIndex)
    ctrl$indexOut <-
      c(list("AllData" = rep(0, nrow(data))),  ctrl$indexOut)
  }

  `%op%` <- getOper(ctrl$allowParallel && foreach::getDoParWorkers() > 1)
  result <-
    foreach(
      iter = seq(along = resampleIndex),
      .combine = "c",
      .verbose = FALSE,
      .errorhandling = "stop",
      .packages = "caret"
    ) %op% {
      loadNamespace("caret")
      requireNamespace("plyr")
      requireNamespace("methods")
      loadNamespace("recipes")

      if (names(resampleIndex)[iter] != "AllData") {
        modelIndex <- resampleIndex[[iter]]
        holdoutIndex <- ctrl$indexOut[[iter]]
      } else {
        modelIndex <- 1:nrow(data)
        holdoutIndex <- modelIndex
      }

      seeds <-
        if (!(length(ctrl$seeds) == 1 &&
              is.na(ctrl$seeds)))
          ctrl$seeds[[iter]] else
            NA

      if (ctrl$verbose)
        cat("+(rfe)",
            names(resampleIndex)[iter],
            "recipe",
            "\n")

      trained_rec <- prep(
        rec, training = data[modelIndex,,drop = FALSE], fresh = TRUE,
        verbose = FALSE, stringsAsFactors = TRUE,
        retain = TRUE
      )

      x <- juice(trained_rec, all_predictors(), composition = "data.frame")
      y <- juice(trained_rec, all_outcomes())[[1]]
      test_x <- bake(
        trained_rec,
        new_data = data[-modelIndex, , drop = FALSE],
        all_predictors(),
        composition = "data.frame"
      )
      test_y <- bake(
        trained_rec,
        new_data = data[-modelIndex, , drop = FALSE],
        all_outcomes()
      )[[1]]

      is_perf <- summary(trained_rec)$role == "performance var"
      if(any(is_perf)) {
        test_perf <- bake(
          trained_rec,
          new_data = data[-modelIndex, , drop = FALSE],
          has_role("performance var"),
          composition = "data.frame"
        )
      } else test_perf <- NULL

      p <- ncol(x)

      if(length(sizes) > 0 && max(sizes) > p)
        sizes <- sizes[sizes <= p]

      if (all(sizes < 2))
        stop(
          "After the recipe, there are less than two predictors remaining. `rfe` ",
          "requires at least two.",
          call. = FALSE
        )

      if (length(sizes) == 0)
        stop(
          "After the recipe, there are only ",
          p,
          " predictors remaining. ",
          "The `sizes` values are inconsistent with this.",
          call. = FALSE
        )

      if (ctrl$verbose)
        cat("-(rfe)",
            names(resampleIndex)[iter],
            "recipe",
            "\n")

      rfeResults <- rfe_rec(
        x, y,
        test_x, test_y,
        test_perf,
        sizes, ctrl,
        label = names(resampleIndex)[iter],
        seeds = seeds,
        ...
      )
      resamples <-
        plyr::ddply(rfeResults$pred,
                    .(Variables),
                    ctrl$functions$summary,
                    lev = lev)

      if (ctrl$saveDetails) {
        rfeResults$pred$Resample <- names(resampleIndex)[iter]
        ## If the user did not have nrow(x) in 'sizes', rfeIter added it.
        ## So, we need to find out how many set of predictions there are:
        nReps <- length(table(rfeResults$pred$Variables))
        rfeResults$pred$rowIndex <-
          rep(seq(along = y)[unique(holdoutIndex)], nReps)
      }

      if (is.factor(y) && length(lev) <= 50) {
        cells <-
          plyr::ddply(rfeResults$pred, .(Variables), function(x)
            flatTable(x$pred, x$obs))
        resamples <- merge(resamples, cells)
      }

      resamples$Resample <- names(resampleIndex)[iter]
      vars <- do.call("rbind", rfeResults$finalVariables)
      vars$Resample <- names(resampleIndex)[iter]
      list(
        resamples = resamples,
        selectedVars = vars,
        predictions = if (ctrl$saveDetails)
          rfeResults$pred else NULL
      )
    }

  resamples <-
    do.call("rbind", result[names(result) == "resamples"])
  rownames(resamples) <- NULL

  if (ctrl$method %in% c("boot632")) {
    perfNames <- names(resamples)
    perfNames <-
      perfNames[!(perfNames %in% c("Resample", "Variables"))]
    perfNames <- perfNames[!grepl("^cell[0-9]", perfNames)]
    apparent <- subset(resamples, Resample == "AllData")
    apparent <-
      apparent[, !grepl("^\\.cell|Resample", colnames(apparent)), drop = FALSE]
    names(apparent)[which(names(apparent) %in% perfNames)] <-
      paste(names(apparent)[which(names(apparent) %in% perfNames)],
            "Apparent", sep = "")
    names(apparent) <- gsub("^\\.", "", names(apparent))
    resamples <- subset(resamples, Resample != "AllData")
  }

  externPerf <-
    plyr::ddply(resamples[, !grepl("\\.cell|Resample", colnames(resamples)), drop = FALSE],
                .(Variables),
                MeanSD,
                exclude = "Variables")
  numVars <-
    plyr::ddply(resamples[, !grepl("\\.cell|Resample", colnames(resamples)), drop = FALSE],
                .(Variables),
                function(x) c(Num_Resamples = nrow(x)))

  externPerf <- merge(externPerf, numVars, by = "Variables", all = TRUE)
  externPerf <- externPerf[order(externPerf$Variables),, drop = FALSE]

  if (ctrl$method %in% c("boot632")) {
    externPerf <- merge(externPerf, apparent)
    for (p in seq(along = perfNames)) {
      const <- 1 - exp(-1)
      externPerf[, perfNames[p]] <-
        (const * externPerf[, perfNames[p]]) +  ((1 - const) * externPerf[, paste(perfNames[p], "Apparent", sep = "")])
    }
    externPerf <-
      externPerf[,!(names(externPerf) %in% paste(perfNames, "Apparent", sep = ""))]
  }
  list(performance = externPerf, everything = result)
}

rfe_rec_loo <- function(rec, data, sizes, ctrl, lev, ...) {
  loadNamespace("caret")
  loadNamespace("recipes")

  resampleIndex <- ctrl$index
  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  result <-
    foreach(
      iter = seq(along = resampleIndex),
      .combine = "c",
      .verbose = FALSE,
      .errorhandling = "stop",
      .packages = "caret"
    ) %op% {

      loadNamespace("caret")
      loadNamespace("recipes")

      requireNamespaceQuietStop("methods")

      modelIndex <- resampleIndex[[iter]]
      holdoutIndex <- -unique(resampleIndex[[iter]])

      seeds <-
        if (!(length(ctrl$seeds) == 1 &&
              is.na(ctrl$seeds)))
          ctrl$seeds[[iter]]  else NA
      if(ctrl$verbose)
        cat("Preparing recipe\n")
      trained_rec <- prep(
        rec, training = data[modelIndex,,drop = FALSE], fresh = TRUE,
        verbose = FALSE, stringsAsFactors = TRUE,
        retain = TRUE
      )

      x <- juice(trained_rec, all_predictors(), composition = "data.frame")
      y <- juice(trained_rec, all_outcomes())[[1]]
      test_x <- bake(
        trained_rec,
        new_data = data[-modelIndex, , drop = FALSE],
        all_predictors(),
        composition = "data.frame"
      )
      test_y <- bake(
        trained_rec,
        new_data = data[-modelIndex, , drop = FALSE],
        all_outcomes()
      )[[1]]

      is_perf <- summary(trained_rec)$role == "performance var"
      if(any(is_perf)) {
        test_perf <- bake(
          trained_rec,
          new_data = data[-modelIndex, , drop = FALSE],
          has_role("performance var"),
          composition = "data.frame"
        )
      } else test_perf <- NULL

      p <- ncol(x)

      if(length(sizes) > 0 && max(sizes) > p)
        sizes <- sizes[sizes <= p]

      if (all(sizes < 2))
        stop(
          "After the recipe, there are less than two predictors remaining. `rfe` ",
          "requires at least two.",
          call. = FALSE
        )

      if (length(sizes) == 0)
        stop(
          "After the recipe, there are only ",
          p,
          " predictors remaining. ",
          "The `sizes` values are inconsistent with this.",
          call. = FALSE
        )

      rfeResults <- rfe_rec(
        x, y,
        test_x, test_y,
        test_perf,
        sizes, ctrl,
        label = names(resampleIndex)[iter],
        seeds = seeds,
        ...
      )
      rfeResults
    }
  preds <- do.call("rbind", result[names(result) == "pred"])
  resamples <-
    ddply(preds, .(Variables), ctrl$functions$summary, lev = lev)
  list(performance = resamples, everything = result)
}



