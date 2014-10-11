
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
          if(is.matrix(modelPred)) modelPred <- as.data.frame(modelPred)
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

      if(nrow(modImp) < sizeValues[k]) stop(paste("rfe is expecting", sizeValues[k], 
                                                  "importance values but only has", nrow(modImp)))
      if(any(!complete.cases(modImp))) stop("There were missing importance values")
      finalVariables[[k]] <- subset(modImp, var %in% retained)
      finalVariables[[k]]$Variables <- sizeValues[[k]]
      if(k < length(sizeValues)) retained <- as.character(modImp$var)[1:sizeValues[k+1]]
    }
  list(finalVariables = finalVariables, pred = rfePred)

}

######################################################################
######################################################################

rfe <- function (x, ...) UseMethod("rfe")

"rfe.default" <-
  function(x, y,
           sizes = 2^(2:4),
           metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
           maximize = ifelse(metric == "RMSE", FALSE, TRUE),
           rfeControl = rfeControl(), ...)
{
  startTime <- proc.time()
  funcCall <- match.call(expand.dots = TRUE)
  if(!("caret" %in% loadedNamespaces())) library(caret)

  if(nrow(x) != length(y)) stop("there should be the same number of samples in x and y")
  numFeat <- ncol(x)
  classLevels <- levels(y)

  if(is.null(rfeControl$index)) rfeControl$index <- switch(tolower(rfeControl$method),
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
                        results = as.data.frame(externPerf),
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

rfe.formula <- function (form, data, ..., subset, na.action, contrasts = NULL) 
{
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval.parent(m$data))) m$data <- as.data.frame(data)
  m$... <- m$contrasts <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  xint <- match("(Intercept)", colnames(x), nomatch = 0)
  if (xint > 0)  x <- x[, -xint, drop = FALSE]
  y <- model.response(m)
  res <- rfe(as.data.frame(x), y, ...)
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

pickSizeBest <- function(x, metric, maximize)
  {
    best <- if(maximize) which.max(x[,metric]) else which.min(x[,metric])
    min(x[best, "Variables"])
  }

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



pickVars <- function(y, size)
  {
    finalImp <- ddply(y[, c("Overall", "var")],
                      .(var),
                      function(x) mean(x$Overall, na.rm = TRUE))
    names(finalImp)[2] <- "Overall"
    finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE),]
    as.character(finalImp$var[1:size])
  }


caretFuncs <- list(summary = defaultSummary,
                   fit = function(x, y, first, last, ...) train(x, y, ...),
                   pred = function(object, x)
                   {
                     tmp <- predict(object, x)
                     if(object$modelType == "Classification" &
                          !is.null(object$modelInfo$prob))
                       {
                         out <- cbind(data.frame(pred = tmp),
                                      as.data.frame(predict(object, x, type = "prob")))
                       } else out <- tmp
                     out
                   },
                   rank = function(object, x, y)
                   {
                     vimp <- varImp(object, scale = FALSE)$importance
                     if(object$modelType == "Regression")
                       {
                         vimp <- vimp[
                                      order(vimp[,1], decreasing = TRUE)
                                      ,,drop = FALSE]
                       } else {
                         if(all(levels(y) %in% colnames(vimp)))
                           {
                             avImp <- apply(vimp[, levels(y), drop = TRUE],
                                            1,
                                            mean)
                             vimp$Overall <- avImp
                           } 
                         
                       } 
                     vimp$var <- rownames(vimp)
                     vimp
                   },
                   selectSize = pickSizeBest,
                   selectVar = pickVars
                   )



## write a better imp sort function
ldaFuncs <- list(summary = defaultSummary,
                 fit = function(x, y, first, last, ...)
                 {
                   library(MASS)
                   lda(x, y, ...)
                 },
                 pred = function(object, x)
                 {
                   tmp <- predict(object, x)
                   out <- cbind(data.frame(pred = tmp$class),
                                as.data.frame(tmp$posterior))
                   out
                 },
                 rank = function(object, x, y)
                 {
                   vimp <- filterVarImp(x, y, TRUE)
                   
                   vimp$Overall <- apply(vimp, 1, mean)
                   vimp <- vimp[
                                order(vimp$Overall, decreasing = TRUE)
                                ,]
                   
                   vimp <- as.data.frame(vimp)[, "Overall",drop = FALSE]
                   vimp$var <- rownames(vimp)
                   vimp
                   
                 },
                 selectSize = pickSizeBest,
                 selectVar = pickVars
                 )


treebagFuncs <- list(summary = defaultSummary,
                     fit = function(x, y, first, last, ...)
                     {
                       library(ipred)
                       ipredbagg(y, x, ...)
                     },
                     pred = function(object, x)
                     {
                       tmp <- predict(object, x)
                       if(is.factor(object$y))
                         {
                           out <- cbind(data.frame(pred = tmp),
                                        as.data.frame(predict(object, x, type = "prob")))
                         } else out <- tmp
                       out
                     },
                     rank = function(object, x, y)
                     {
                       vimp <- varImp(object)
                       vimp <- vimp[
                                    order(vimp$Overall, decreasing = TRUE)
                                    ,,drop = FALSE]
                       vimp$var <- rownames(vimp)
                       vimp
                     },
                     selectSize = pickSizeBest,
                     selectVar = pickVars)




gamFuncs <- list(summary = defaultSummary,
                 fit = function(x, y, first, last, ...)
                 {
                   loaded <- search()
                   gamLoaded <- any(loaded == "package:gam")
                   if(gamLoaded) detach(package:gam)
                   library(mgcv)
                   dat <- if(is.data.frame(x)) x else as.data.frame(x)
                   dat$y <- y
                   args <- list(formula = gamFormula(x, smoother = "s", y = "y"),
                                data = dat,
                                family = if(!is.factor(y)) gaussian else  binomial)
                   do.call("gam", args)
                 },
                 pred = function(object, x)
                 {
                                        #browser()
                   loaded <- search()
                   gamLoaded <- any(loaded == "package:gam")
                   if(gamLoaded) detach(package:gam)
                   library(mgcv)
                   rsp <- predict(object, newdata = x, type = "response")
                   if(object$family$family == "binomial")
                     {
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
                 rank = function(object, x, y)
                 {

                   loaded <- search()
                   gamLoaded <- any(loaded == "package:gam")
                   if(gamLoaded) detach(package:gam)
                   library(mgcv)
                   vimp <- varImp(object)
                   vimp$var <- rownames(vimp)
                   if(any(!(colnames(x) %in% rownames(vimp))))
                     {
                       missing <- colnames(x)[!(colnames(x) %in% rownames(vimp))]
                       tmpdf <- data.frame(var = missing,
                                           Overall = rep(0, length(missing)))
                       vimp <- rbind(vimp, tmpdf)
                     }
                   vimp
                 },
                 selectSize = pickSizeBest,
                 selectVar = pickVars)


rfFuncs <-  list(summary = defaultSummary,
                 fit = function(x, y, first, last, ...)
                 {
                   library(randomForest)
                   randomForest(x, y, importance = first, ...)
                 },
                 pred = function(object, x)
                 {
                   tmp <- predict(object, x)
                   if(is.factor(object$y))
                     {
                       out <- cbind(data.frame(pred = tmp),
                                    as.data.frame(predict(object, x, type = "prob")))
                     } else out <- tmp
                   out
                 },
                 rank = function(object, x, y)
                 {
                   vimp <- varImp(object)

                   if(is.factor(y))
                     {
                       if(all(levels(y) %in% colnames(vimp)))
                         {
                           avImp <- apply(vimp[, levels(y), drop = TRUE],
                                          1,
                                          mean)
                           vimp$Overall <- avImp
                         }

                     }
                   
                   vimp <- vimp[
                                order(
                                      vimp$Overall,
                                      decreasing = TRUE)
                                ,,
                                drop = FALSE]
                   
                   vimp$var <- rownames(vimp)                  
                   vimp
                 },
                 selectSize = pickSizeBest,
                 selectVar = pickVars)


lmFuncs <- list(summary = defaultSummary,
                fit = function(x, y, first, last, ...)
                {
                  tmp <- as.data.frame(x)
                  tmp$y <- y
                  lm(y~., data = tmp)
                },
                pred = function(object, x)
                {
                  predict(object, x)
                },
                rank = function(object, x, y)
                {
                  
                  vimp <- varImp(object, scale = FALSE)        
                  vimp <- vimp[
                               order(
                                     vimp$Overall,
                                     decreasing = TRUE)
                               ,,
                               drop = FALSE]
                  vimp$var <- rownames(vimp)
                  vimp
                },
                selectSize = pickSizeBest,
                selectVar = pickVars)


nbFuncs <- list(summary = defaultSummary,
                fit = function(x, y, first, last, ...)
                {
                  library(klaR)
                  NaiveBayes(x, y, usekernel = TRUE, fL = 2, ...)
                },
                pred = function(object, x)
                {
                  tmp <- predict(object, x)
                  out <- cbind(data.frame(pred = tmp$class),
                               as.data.frame(tmp$posterior))
                  out
                },
                rank = function(object, x, y)
                {
                  vimp <- filterVarImp(x, y)
                  if(is.factor(y))
                    {
                      avImp <- apply(vimp, 1, mean)
                      vimp$Overall <- avImp
                    }
                  
                  vimp <- vimp[
                               order(
                                     vimp$Overall,
                                     decreasing = TRUE)
                               ,,
                               drop = FALSE]
                  
                  vimp$var <- rownames(vimp)                  
                  vimp
                },
                selectSize = pickSizeBest,
                selectVar = pickVars)

lrFuncs <- ldaFuncs
lrFuncs$fit <- function (x, y, first, last, ...) 
{
  tmp <- x
  tmp$Class <- y
  glm(Class ~ ., data = tmp, family = "binomial")
}
lrFuncs$pred <- function (object, x) 
{
  lvl <- levels(object$data$Class)
  tmp <- predict(object, x, type = "response")
  out <- data.frame(1-tmp, tmp)
  colnames(out) <- lvl
  out$pred <- factor(ifelse(tmp > .5, lvl[2], lvl[1]),
                     levels = lvl)
  out
}

lrFuncs$rank <- function (object, x, y) 
{
    vimp <- varImp(object, scale = FALSE)
    vimp <- vimp[order(vimp$Overall, decreasing = TRUE),, drop = FALSE]
    vimp$var <- rownames(vimp)
    vimp
}

######################################################################
######################################################################
## lattice functions


densityplot.rfe <- function(x,
                            data = NULL,
                            metric = x$metric,
                            ...)
  {
    if (!is.null(match.call()$data))
      warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

    data <- as.data.frame(x$resample)
    data$Variable <- factor(data$Variable,
                            levels = paste(sort(unique(data$Variable))))

    form <- as.formula(paste("~", metric, "|Variable"))
    densityplot(form, data = data, ...)
  }

histogram.rfe <- function(x,
                          data = NULL,
                          metric = x$metric,
                          ...)
  {
    if (!is.null(match.call()$data))
      warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

    data <- as.data.frame(x$resample)
    data$Variable <- factor(data$Variable,
                            levels = paste(sort(unique(data$Variable))))

    form <- as.formula(paste("~", metric, "|Variable"))
    histogram(form, data = data, ...)
  }

stripplot.rfe <- function(x,
                          data = NULL,
                          metric = x$metric,
                          ...)
  {
    if (!is.null(match.call()$data))
      warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

    data <- as.data.frame(x$resample)
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


xyplot.rfe <- function(x,
                       data = NULL,
                       metric = x$metric,
                       ...)
  {
    if (!is.null(match.call()$data))
      warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

    data <- as.data.frame(x$resample)

    form <- as.formula(paste(metric, " ~ Variables"))
    xyplot(form, data = data, ...)
  }

######################################################################
######################################################################
## other functions

predictors.rfe <- function(x, ...) x$optVariables

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

predict.rfe <- function(object, newdata, ...)
  {
    if(length(list(...)) > 0)
      warning("... are ignored for predict.rfe")

    if(inherits(object, "rfe.formula"))
      {
        newdata <- as.data.frame(newdata)
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
      }

    checkCols <- object$optVar %in% colnames(newdata) 
    if(!all(checkCols))
      stop(paste("missing columns from newdata:",
                 paste(object$optVar[!checkCols], collapse = ", ")))
    
    newdata <- newdata[, object$optVar, drop = FALSE]
    object$control$functions$pred(object$fit, newdata)
  }


update.rfe <- function(object, x, y, size, ...) {
  size <- size[1]
  selectedVars <- object$variables
  bestVar <- object$control$functions$selectVar(selectedVars, size)  
  object$fit <- object$control$functions$fit(x[, bestVar, drop = FALSE],
                                             y,
                                             first = FALSE,
                                             last = TRUE,
                                             ...)
  object$bestSubset <- size
  object$bestVar <- bestVar
  
  if(object$control$returnResamp == "final") {
    warning("The saved resamples are no longer appropriate and were removed")
    object$resampledCM <- object$resample <- NULL
  }
  object
}


