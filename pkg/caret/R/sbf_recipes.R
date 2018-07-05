#' @export
"sbf.recipe" <-
  function(x, data,
           sbfControl = sbfControl(), ...) {
    startTime <- proc.time()
    funcCall <- match.call(expand.dots = TRUE)
    
    orig_rec <- x
    trained_rec <- prep(
      x, training = data,
      fresh = TRUE,
      retain = TRUE,
      verbose = FALSE,
      stringsAsFactors = TRUE
    )
    x <- juice(trained_rec, all_predictors(), composition = "data.frame")
    y <- juice(trained_rec, all_outcomes(), composition = "data.frame")
    if(ncol(y) > 1)
      stop("`safs` doesn't support multivariate outcomes", call. = FALSE)
    y <- y[[1]]
    is_weight <- summary(trained_rec)$role == "case weight"
    if(any(is_weight))
      stop("`safs` does not allow for weights.", call. = FALSE)
    
    is_perf <- summary(trained_rec)$role == "performance var"
    if(any(is_perf)) {
      perf_data <- juice(trained_rec, has_role("performance var"))
    } else perf_data <- NULL
    
    numFeat <- ncol(x)
    classLevels <- levels(y)
    
    if (sbfControl$method == "oob")
      stop("out-of-bag resampling cannot be used with this function")
    
    if(is.null(sbfControl$index)) sbfControl$index <- switch(
      tolower(sbfControl$method),
      cv = createFolds(y, sbfControl$number, returnTrain = TRUE),
      repeatedcv = createMultiFolds(y, sbfControl$number, sbfControl$repeats),
      loocv = createFolds(y, length(y), returnTrain = TRUE),
      boot =, boot632 = createResample(y, sbfControl$number),
      test = createDataPartition(y, 1, sbfControl$p),
      lgocv = createDataPartition(y, sbfControl$number, sbfControl$p))
    
    if(is.null(names(sbfControl$index)))
      names(sbfControl$index) <- prettySeq(sbfControl$index)
    if(is.null(sbfControl$indexOut)){
      sbfControl$indexOut <- lapply(sbfControl$index,
                                    function(training, allSamples) allSamples[-unique(training)],
                                    allSamples = seq(along = y))
      names(sbfControl$indexOut) <- prettySeq(sbfControl$indexOut)
    }
    ## check summary function and metric
    testOutput <- data.frame(pred = sample(y, min(10, length(y))),
                             obs = sample(y, min(10, length(y))))
    
    if(is.factor(y))
      for(i in seq(along = classLevels))
        testOutput[, classLevels[i]] <- runif(nrow(testOutput))
    if(!is.null(perf_data))
      testOutput <- cbind(
        testOutput,
        perf_data[sample(1:nrow(perf_data), nrow(testOutput)),, drop = FALSE]
      )
    
    test <- sbfControl$functions$summary(testOutput, lev = classLevels)
    perfNames <- names(test)
    
    ## Set or check the seeds when needed
    if(is.null(sbfControl$seeds)) {
      sbfControl$seeds <- sample.int(n = 1000000, size = length(sbfControl$index) + 1)
    } else {
      if(!(length(sbfControl$seeds) == 1 && is.na(sbfControl$seeds))) {
        if(length(sbfControl$seeds) != length(sbfControl$index) + 1)
          stop(paste("Bad seeds: the seed object should be an integer vector of length",
                     length(sbfControl$index) + 1))
      }
    }
    
    
    
    #########################################################################
    
    if(sbfControl$method == "LOOCV") {
      tmp <- sbf_loo_rec(x = x, y = y, 
                         ctrl = sbfControl, lev = classLevels, ...)
      resamples <- do.call("rbind", tmp$everything[names(tmp$everything) == "pred"])
      rownames(resamples) <- 1:nrow(resamples)
      selectedVars <- tmp$everything[names(tmp$everything) == "variables"]
      performance <- tmp$performance
    } else {
      tmp <- sbf_rec(rec = orig_rec, data = data,
                     ctrl = sbfControl, lev = classLevels, ...)
      resamples <- do.call("rbind", tmp$everything[names(tmp$everything) == "resamples"])
      rownames(resamples) <- 1:nrow(resamples)
      selectedVars <- tmp$everything[names(tmp$everything) == "selectedVars"]
      performance <- tmp$performance
    }
    
    #########################################################################
    
    varList <- unique(unlist(selectedVars))
    if(sbfControl$multivariate) {
      scores <- sbfControl$functions$score(x, y)
      if(length(scores) != ncol(x))
        stop(paste("when control$multivariate == TRUE, 'scores'",
                   "should return a vector with", ncol(x), "numeric values"))
    } else  {
      scores <- apply(x, 2, sbfControl$functions$score, y = y)
    }
    retained <- sbfControl$functions$filter(scores, x, y)
    
    finalTime <- system.time(
      fit <-
        sbfControl$functions$fit(
          x = x[, retained, drop = FALSE],
          y = y,
          ...
        )
    )
    
    performance <- data.frame(t(performance))
    performance <- performance[,!grepl("\\.cell|Resample", colnames(performance))]
    
    if(is.factor(y) & any(names(resamples) == ".cell1")) {
      keepers <- c("Resample", grep("\\.cell", names(resamples), value = TRUE))
      resampledCM <- resamples[,keepers]
      resamples <- resamples[, -grep("\\.cell", names(resamples))]
    } else resampledCM <- NULL
    
    resamples <- switch(sbfControl$returnResamp,
                        none = NULL,
                        all =, final = resamples)
    
    endTime <- proc.time()
    times <- list(everything = endTime - startTime,
                  final = finalTime)
    
    #########################################################################
    ## Now, based on probability or static ranking, figure out the best vars
    ## and the best subset size and fit final model
    
    out <- structure(
      list(
        pred = if(sbfControl$saveDetails) tmp else NULL,
        variables = selectedVars,
        results = performance,
        fit = fit,
        optVariables = names(retained)[retained],
        call = funcCall,
        control = sbfControl,
        resample = resamples,
        metrics = perfNames,
        times = times,
        resampledCM = resampledCM,
        obsLevels = classLevels,
        dots = list(...)),
      class = "sbf")
    if(sbfControl$timingSamps > 0) {
      out$times$prediction <-
        system.time(
          predict(out, x[1:min(nrow(x), sbfControl$timingSamps),,drop = FALSE])
        )
    } else  out$times$prediction <- rep(NA, 3)
    out
  }


#' @import foreach
sbf_rec <- function(rec, data, ppOpts, ctrl, lev, ...) {
  loadNamespace("caret")
  
 
  resampleIndex <- ctrl$index
  if(ctrl$method %in% c("boot632")){
    resampleIndex <- c(list("AllData" = rep(0, nrow(x))), resampleIndex)
    ctrl$indexOut <- c(list("AllData" = rep(0, nrow(x))),  ctrl$indexOut)
  }
  
  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  result <- foreach(
    iter = seq(along = resampleIndex),
    .combine = "c",
    .verbose = FALSE,
    .errorhandling = "stop",
    .packages = c("caret", "recipes")) %op% {
      if (!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds)))
        set.seed(ctrl$seeds[iter])
      
      loadNamespace("caret")
      requireNamespaceQuietStop("methods")
      
      if(names(resampleIndex)[iter] != "AllData") {
        modelIndex <- resampleIndex[[iter]]
        holdoutIndex <- ctrl$indexOut[[iter]]
      } else {
        modelIndex <- 1:nrow(data)
        holdoutIndex <- modelIndex
      }
      
      # reprocess recipe
      resampled_rec <- prep(
        rec,
        training = data[modelIndex, ],
        fresh = TRUE,
        retain = TRUE,
        verbose = FALSE,
        stringsAsFactors = TRUE
      )
      x_tr <- juice(resampled_rec, all_predictors(), composition = "data.frame")
      y_tr <- juice(resampled_rec, all_outcomes(), composition = "data.frame")
      y_tr <- y_tr[[1]]
      x_te <- bake(resampled_rec, newdata = data[ holdoutIndex, ],
                   all_predictors(), composition = "data.frame")
      y_te <- bake(resampled_rec, newdata = data[ holdoutIndex, ],
                   all_outcomes(), composition = "data.frame")
      y_te <- y_te[[1]]
      is_perf <- summary(resampled_rec)$role == "performance var"
      if(any(is_perf)) {
        perf_tr <- juice(resampled_rec, has_role("performance var"))
        perf_te <- bake(
          resampled_rec,
          newdata = data[ holdoutIndex, ],
          has_role("performance var")
        )
      } else {
        perf_tr <- NULL
        perf_te <- NULL
      }
      
      sbfResults <- sbfIter(x = x_tr,
                            y = y_tr,
                            testX = x_te,
                            testY = y_te,
                            testPerf = perf_te,
                            sbfControl = ctrl,
                            ...)
      if(ctrl$saveDetails) {
        tmpPred <- sbfResults$pred
        tmpPred$Resample <- names(resampleIndex)[iter]
        tmpPred$rowIndex <- (1:nrow(data))[unique(holdoutIndex)]
      } else tmpPred <- NULL
      resamples <- ctrl$functions$summary(sbfResults$pred, lev = lev)
      if(is.factor(y_tr) && length(lev) <= 50)
        resamples <- c(resamples, flatTable(sbfResults$pred$pred, sbfResults$pred$obs))
      resamples <- data.frame(t(resamples))
      resamples$Resample <- names(resampleIndex)[iter]
      
      list(resamples = resamples, selectedVars = sbfResults$variables, pred = tmpPred)
    }
    
    resamples <- rbind.fill(result[names(result) == "resamples"])
    pred <- if(ctrl$saveDetails) rbind.fill(result[names(result) == "pred"]) else NULL
    performance <- MeanSD(resamples[,!grepl("Resample", colnames(resamples)),drop = FALSE])
    
    if(ctrl$method %in% c("boot632")) {
      modelIndex <- 1:nrow(x)
      holdoutIndex <- modelIndex
      appResults <- sbfIter(x = x_tr,
                            y = y_tr,
                            testX = x_te,
                            testY = y_te,
                            testPerf = perf_te,
                            ctrl,
                            ...)
      apparent <- ctrl$functions$summary(appResults$pred, lev = lev)
      perfNames <- names(apparent)
      perfNames <- perfNames[perfNames != "Resample"]
      
      const <- 1-exp(-1)
      
      for(p in seq(along = perfNames))
        performance[perfNames[p]] <-
        (const * performance[perfNames[p]]) +  ((1-const) * apparent[perfNames[p]])
    }
    
    list(
      performance = performance,
      everything = result,
      predictions = if (ctrl$saveDetails)
        pred
      else
        NULL
    )
}


