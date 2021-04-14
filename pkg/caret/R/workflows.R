
### In this file, there are a lot of functions from packages that are
### referenced using `getFromNamespace`. For some
### reason, with _some_ parallel processing technologies and foreach,
### functions inside of caret cannot be found despite using the
### ".packages" argument or even calling the caret package via library().

getOper <- function(x) if(x)  `%dopar%` else  `%do%`
getTrainOper <- function(x) if(x)  `%dopar%` else  `%do%`


#' @rdname caret-internal
#' @export
#' @keywords internal
progress <- function(x, names, iter, start = TRUE)
{
  text <- paste(ifelse(start, "+ ", "- "),
                names[iter], ": ",
                paste(colnames(x), x, sep = "=", collapse = ", "),
                sep = "")
  cat(text, "\n")
}

#' @rdname caret-internal
#' @importFrom stats sd
#' @export
MeanSD <- function(x, exclude = NULL)
{
  if(!is.null(exclude)) x <- x[, !(colnames(x) %in% exclude), drop = FALSE]
  out <- c(colMeans(x, na.rm = TRUE), sapply(x, sd, na.rm = TRUE))
  names(out)[-(1:ncol(x))] <- paste(names(out)[-(1:ncol(x))], "SD", sep = "")
  out
}

#' @rdname caret-internal
#' @export
expandParameters <- function(fixed, seq)
{
  if(is.null(seq)) return(fixed)

  isSeq <- names(fixed) %in% names(seq)
  out <- fixed
  for(i in 1:nrow(seq))
  {
    tmp <- fixed
    tmp[,isSeq] <- seq[i,]
    out <- rbind(out, tmp)
  }
  out
}

#' @importFrom utils head
#' @importFrom stats complete.cases
#' @import foreach
nominalTrainWorkflow <- function(x, y, wts, info, method, ppOpts, ctrl, lev, testing = FALSE, ...)
{
  loadNamespace("caret")
  ppp <- list(options = ppOpts)
  ppp <- c(ppp, ctrl$preProcOptions)

  printed <- format(info$loop, digits = 4)
  colnames(printed) <- gsub("^\\.", "", colnames(printed))

  ## For 632 estimator, add an element to the index of zeros to trick it into
  ## fitting and predicting the full data set.

  resampleIndex <- ctrl$index
  if(ctrl$method %in% c("boot632", "optimism_boot", "boot_all"))
  {
    resampleIndex <- c(list("AllData" = rep(0, nrow(x))), resampleIndex)
    ctrl$indexOut <- c(list("AllData" = rep(0, nrow(x))),  ctrl$indexOut)
    if(!is.null(ctrl$indexExtra)) ctrl$indexExtra <- c(list("AllData" = NULL), ctrl$indexExtra)
  }
  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  keep_pred <- isTRUE(ctrl$savePredictions) || ctrl$savePredictions %in% c("all", "final")
  pkgs <- c("methods", "caret")
  if(!is.null(method$library)) pkgs <- c(pkgs, method$library)
  export <- c()

  result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .export = export, .packages = "caret") %:%
    foreach(parm = 1L:nrow(info$loop), .combine = "c", .verbose = FALSE, .export = export , .packages = "caret")  %op%
    {
      if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) set.seed(ctrl$seeds[[iter]][parm])

      loadNamespace("caret")
      lapply(pkgs, requireNamespaceQuietStop)
      if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE],
                                    names(resampleIndex), iter)

      if(names(resampleIndex)[iter] != "AllData") {

        modelIndex <- resampleIndex[[iter]]
        holdoutIndex <- ctrl$indexOut[[iter]]
      } else {
        modelIndex <- 1:nrow(x)
        holdoutIndex <- modelIndex
      }

      if(testing) cat("pre-model\n")

      if(!is.null(info$submodels[[parm]]) && nrow(info$submodels[[parm]]) > 0) {
        submod <- info$submodels[[parm]]
      } else submod <- NULL

      mod <- try(
        createModel(x = subset_x(x, modelIndex),
                    y = y[modelIndex],
                    wts = wts[modelIndex],
                    method = method,
                    tuneValue = info$loop[parm,,drop = FALSE],
                    obsLevels = lev,
                    pp = ppp,
                    classProbs = ctrl$classProbs,
                    sampling = ctrl$sampling,
                    ...),
        silent = TRUE)

      if(testing) print(mod)

      if(!model_failed(mod)) {
          predicted <- try(
          predictionFunction(method = method,
                             modelFit = mod$fit,
                             newdata = subset_x(x, holdoutIndex),
                             preProc = mod$preProc,
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
                     msg  = mod,
                     iter = names(resampleIndex)[iter],
                     verb = ctrl$verboseIter)
        predicted <- fill_failed_pred(index = holdoutIndex, lev = lev, submod)
      }

      if(testing) print(head(predicted))

      if(ctrl$classProbs) {
        if(!model_failed(mod)) {
          probValues <- probFunction(method = method,
                                     modelFit = mod$fit,
                                     newdata = subset_x(x, holdoutIndex),
                                     preProc = mod$preProc,
                                     param = submod)
       } else {
          probValues <- fill_failed_prob(holdoutIndex, lev, submod)
        }
        if(testing) print(head(probValues))
      }

      ##################################

      predicted <- trim_values(predicted, ctrl, is.null(lev))

      ##################################

      if(!is.null(submod))
      {
        ## merge the fixed and seq parameter values together
        allParam <- expandParameters(info$loop[parm,,drop = FALSE], info$submodels[[parm]])
        allParam <- allParam[complete.cases(allParam),, drop = FALSE]

        ## collate the predicitons across all the sub-models
        predicted <- lapply(predicted,
                            function(x, y, wts, lv, rows) {
                              x <- outcome_conversion(x, lv = lev)
                              out <- data.frame(pred = x, obs = y, stringsAsFactors = FALSE)
                              if(!is.null(wts)) out$weights <- wts
                              out$rowIndex <- rows
                              out
                            },
                            y = y[holdoutIndex],
                            wts = wts[holdoutIndex],
                            lv = lev,
                            rows = holdoutIndex)

        if(ctrl$classProbs)
          predicted <- mapply(cbind, predicted, probValues, SIMPLIFY = FALSE)

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
          for(ind in seq(along = cells)) thisResample[[ind]] <- c(thisResample[[ind]], cells[[ind]])
        }
        thisResample <- do.call("rbind", thisResample)
        thisResample <- cbind(allParam, thisResample)

      } else {
        if(is.factor(y)) predicted <- outcome_conversion(predicted, lv = lev)
        tmp <-  data.frame(pred = predicted,
                           obs = y[holdoutIndex],
                           stringsAsFactors = FALSE)
        ## Sometimes the code above does not coerce the first
        ## columnn to be named "pred" so force it
        names(tmp)[1] <- "pred"
        if(!is.null(wts)) tmp$weights <- wts[holdoutIndex]
        if(ctrl$classProbs) tmp <- cbind(tmp, probValues)
        tmp$rowIndex <- holdoutIndex

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
        thisResample <- as.data.frame(t(thisResample), stringsAsFactors = FALSE)
        thisResample <- cbind(thisResample, info$loop[parm,,drop = FALSE])
      }
      thisResample$Resample <- names(resampleIndex)[iter]

      thisResampleExtra <- optimism_xy(ctrl, x, y, wts, iter, lev, method, mod, predicted,
                                       submod, info$loop[parm,, drop = FALSE])

      if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE],
                                    names(resampleIndex), iter, FALSE)

      if(testing) print(thisResample)
      list(resamples = thisResample, pred = tmpPred, resamplesExtra = thisResampleExtra)
    }

  resamples <- rbind.fill(result[names(result) == "resamples"])
  pred <- rbind.fill(result[names(result) == "pred"])
  resamplesExtra <- rbind.fill(result[names(result) == "resamplesExtra"])
  if(ctrl$method %in% c("boot632", "optimism_boot", "boot_all"))
  {
    perfNames <- names(resamples)
    perfNames <- perfNames[!(perfNames %in% c("Resample", as.character(method$parameters$parameter)))]
    perfNames <- perfNames[!grepl("^\\.cell[0-9]", perfNames)]
    apparent <- subset(resamples, Resample == "AllData")
    apparent <- apparent[,!grepl("^\\.cell|Resample", colnames(apparent)),drop = FALSE]
    names(apparent)[which(names(apparent) %in% perfNames)] <- paste(names(apparent)[which(names(apparent) %in% perfNames)],
                                                                    "Apparent", sep = "")
    names(apparent) <- gsub("^\\.", "", names(apparent))
    if(any(!complete.cases(apparent[,!grepl("^cell|Resample", colnames(apparent)),drop = FALSE])))
    {
      warning("There were missing values in the apparent performance measures.")
    }
    resamples <- subset(resamples, Resample != "AllData")
    if(!is.null(pred))
    {
      predHat <- subset(pred, Resample == "AllData")
      pred <- subset(pred, Resample != "AllData")
    }
  }
  names(resamples) <- gsub("^\\.", "", names(resamples))

  if(any(!complete.cases(resamples[,!grepl("^cell|Resample", colnames(resamples)),drop = FALSE])))
  {
    warning("There were missing values in resampled performance measures.")
  }

  out <- ddply(resamples[,!grepl("^cell|Resample", colnames(resamples)),drop = FALSE],
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


#' @import foreach
looTrainWorkflow <- function(x, y, wts, info, method, ppOpts, ctrl, lev, testing = FALSE, ...)
{
  loadNamespace("caret")

  ppp <- list(options = ppOpts)
  ppp <- c(ppp, ctrl$preProcOptions)

  printed <- format(info$loop)
  colnames(printed) <- gsub("^\\.", "", colnames(printed))

  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)

  pkgs <- c("methods", "caret")
  if(!is.null(method$library)) pkgs <- c(pkgs, method$library)

  result <- foreach(iter = seq(along = ctrl$index), .combine = "rbind", .verbose = FALSE, .errorhandling = "stop", .packages = "caret") %:%
    foreach(parm = 1:nrow(info$loop), .combine = "rbind", .verbose = FALSE, .errorhandling = "stop", .packages = "caret") %op% {

      if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) set.seed(ctrl$seeds[[iter]][parm])
      if(testing) cat("after loops\n")
      loadNamespace("caret")
      lapply(pkgs, requireNamespaceQuietStop)
      if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE],
                                    names(ctrl$index), iter, TRUE)
      if(is.null(info$submodels[[parm]]) || nrow(info$submodels[[parm]]) > 0) {
        submod <- info$submodels[[parm]]
      } else submod <- NULL

      mod <- try(
        createModel(x = subset_x(x, ctrl$index[[iter]]),
                    y = y[ctrl$index[[iter]] ],
                    wts = wts[ctrl$index[[iter]] ],
                    method = method,
                    tuneValue = info$loop[parm,,drop = FALSE],
                    obsLevels = lev,
                    pp = ppp,
                    classProbs = ctrl$classProbs,
                    sampling = ctrl$sampling,
                    ...),
        silent = TRUE)

      holdoutIndex <- ctrl$indexOut[[iter]]

      if(!model_failed(mod)) {
        predicted <- try(
          predictionFunction(method = method,
                             modelFit = mod$fit,
                             newdata = subset_x(x, -ctrl$index[[iter]]),
                             preProc = mod$preProc,
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
                     msg  = mod,
                     iter = names(ctrl$index)[iter],
                     verb = ctrl$verboseIter)
        predicted <- fill_failed_pred(index = holdoutIndex, lev = lev, submod)
      }

      if(testing) print(head(predicted))
      if(ctrl$classProbs) {
        if(!model_failed(mod)) {
          probValues <- probFunction(method = method,
                                     modelFit = mod$fit,
                                     newdata = subset_x(x, holdoutIndex),
                                     preProc = mod$preProc,
                                     param = submod)
        } else {
          probValues <- fill_failed_prob(holdoutIndex, lev, submod)
        }
        if(testing) print(head(probValues))
      }

      predicted <- trim_values(predicted, ctrl, is.null(lev))

      ##################################

      if(!is.null(info$submodels)) {
        ## collate the predictions across all the sub-models
        predicted <- lapply(predicted,
                            function(x, y, wts, lv, rows) {
                              x <- outcome_conversion(x, lv = lev)
                              out <- data.frame(pred = x, obs = y, stringsAsFactors = FALSE)
                              if(!is.null(wts)) out$weights <- wts
                              out$rowIndex <- rows
                              out
                            },
                            y = y[holdoutIndex],
                            wts = wts[holdoutIndex],
                            lv = lev,
                            rows = seq(along = y)[holdoutIndex])
        if(testing) print(head(predicted))

        ## same for the class probabilities
        if(ctrl$classProbs)
          for(k in seq(along = predicted))
            predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
        predicted <- do.call("rbind", predicted)
        allParam <- expandParameters(info$loop[parm,,drop = FALSE], submod)
        rownames(predicted) <- NULL
        predicted <- cbind(predicted, allParam)
        ## if saveDetails then save and export 'predicted'
      } else {
        predicted <- outcome_conversion(predicted, lv = lev)
        predicted <-  data.frame(pred = predicted,
                                 obs = y[holdoutIndex],
                                 stringsAsFactors = FALSE)
        if(!is.null(wts)) predicted$weights <- wts[holdoutIndex]
        if(ctrl$classProbs) predicted <- cbind(predicted, probValues)
        predicted$rowIndex <- seq(along = y)[holdoutIndex]
        predicted <- cbind(predicted, info$loop[parm,,drop = FALSE])

      }
      if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE],
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

#' @import foreach
oobTrainWorkflow <- function(x, y, wts, info, method, ppOpts, ctrl, lev, testing = FALSE, ...)
{
  loadNamespace("caret")
  ppp <- list(options = ppOpts)
  ppp <- c(ppp, ctrl$preProcOptions)
  printed <- format(info$loop)
  colnames(printed) <- gsub("^\\.", "", colnames(printed))
  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  pkgs <- c("methods", "caret")
  if(!is.null(method$library)) pkgs <- c(pkgs, method$library)
  result <- foreach(parm = 1:nrow(info$loop), .combine = "rbind", .packages = "caret") %op%
  {
    loadNamespace("caret")
    lapply(pkgs, requireNamespaceQuietStop)
    if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE], "", 1, TRUE)

    if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) set.seed(ctrl$seeds[[1L]][parm])

    mod <- createModel(x = x,
                       y = y,
                       wts = wts,
                       method = method,
                       tuneValue = info$loop[parm,,drop = FALSE],
                       obsLevels = lev,
                       pp = ppp,
                       classProbs = ctrl$classProbs,
                       sampling = ctrl$sampling,
                       ...)

    out <- method$oob(mod$fit)

    if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE], "", 1, FALSE)

    cbind(as.data.frame(t(out), stringsAsFactors = TRUE), info$loop[parm,,drop = FALSE])
  }
  names(result) <- gsub("^\\.", "", names(result))
  result
}

################################################################################################

#' @import foreach
nominalSbfWorkflow <- function(x, y, ppOpts, ctrl, lev, ...) {
  loadNamespace("caret")
  ppp <- list(options = ppOpts)
  ppp <- c(ppp, ctrl$preProcOptions)

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
    .packages = c("caret")) %op% {
      if (!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds)))
        set.seed(ctrl$seeds[iter])

    loadNamespace("caret")
    requireNamespaceQuietStop("methods")
    if(names(resampleIndex)[iter] != "AllData") {
      modelIndex <- resampleIndex[[iter]]
      holdoutIndex <- ctrl$indexOut[[iter]]
    } else {
      modelIndex <- 1:nrow(x)
      holdoutIndex <- modelIndex
    }

    sbfResults <- sbfIter(x = subset_x(x, modelIndex),
                          y = y[modelIndex],
                          testX = subset_x(x, holdoutIndex),
                          testY = y[holdoutIndex],
                          sbfControl = ctrl,
                          ...)
    if(ctrl$saveDetails) {
      tmpPred <- sbfResults$pred
      tmpPred$Resample <- names(resampleIndex)[iter]
      tmpPred$rowIndex <- seq(along = y)[unique(holdoutIndex)]
    } else tmpPred <- NULL
    resamples <- ctrl$functions$summary(sbfResults$pred, lev = lev)
    if(is.factor(y) && length(lev) <= 50)
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
    appResults <- sbfIter(subset_x(x, modelIndex),
                          y[modelIndex],
                          subset_x(x, holdoutIndex),
                          y[holdoutIndex],
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

#' @import foreach
looSbfWorkflow <- function(x, y, ppOpts, ctrl, lev, ...) {
  loadNamespace("caret")
  ppp <- list(options = ppOpts)
  ppp <- c(ppp, ctrl$preProcOptions)

  resampleIndex <- ctrl$index

  vars <- vector(mode = "list", length = length(y))

  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  result <- foreach(
    iter = seq(along = resampleIndex),
    .combine = "c",
    .verbose = FALSE,
    .errorhandling = "stop",
    .packages = "caret") %op% {

    if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds)))
      set.seed(ctrl$seeds[iter])

    loadNamespace("caret")
    requireNamespaceQuietStop("methods")
    modelIndex <- resampleIndex[[iter]]
    holdoutIndex <- -unique(resampleIndex[[iter]])

    sbfResults <- sbfIter(subset_x(x, modelIndex),
                          y[modelIndex],
                          subset_x(x, holdoutIndex),
                          y[holdoutIndex],
                          ctrl,
                          ...)

    sbfResults
  }
  resamples <- do.call("rbind", result[names(result) == "pred"])
  performance <- ctrl$functions$summary(resamples, lev = lev)

  list(
    performance = performance,
    everything = result,
    predictions = if (ctrl$saveDetails)
      resamples
    else
      NULL
  )
}


################################################################################################

#' @import foreach
nominalRfeWorkflow <- function(x, y, sizes, ppOpts, ctrl, lev, ...)
{
  loadNamespace("caret")
  ppp <- list(options = ppOpts)
  ppp <- c(ppp, ctrl$preProcOptions)

  resampleIndex <- ctrl$index
  if(ctrl$method %in% c("boot632")) {
    resampleIndex <- c(list("AllData" = rep(0, nrow(x))), resampleIndex)
    ctrl$indexOut <- c(list("AllData" = rep(0, nrow(x))),  ctrl$indexOut)
  }

  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .errorhandling = "stop", .packages = "caret") %op%
  {
    loadNamespace("caret")
    requireNamespace("plyr")
    requireNamespace("methods")

    if(names(resampleIndex)[iter] != "AllData") {
      modelIndex <- resampleIndex[[iter]]
      holdoutIndex <- ctrl$indexOut[[iter]]
    } else {
      modelIndex <- 1:nrow(x)
      holdoutIndex <- modelIndex
    }

    seeds <- if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) ctrl$seeds[[iter]] else NA
    rfeResults <- rfeIter(subset_x(x, modelIndex),
                          y[modelIndex],
                          subset_x(x, holdoutIndex),
                          y[holdoutIndex],
                          sizes,
                          ctrl,
                          label = names(resampleIndex)[iter],
                          seeds = seeds,
                          ...)
    resamples <- plyr::ddply(rfeResults$pred, .(Variables), ctrl$functions$summary, lev = lev)

    if(ctrl$saveDetails)
    {
      rfeResults$pred$Resample <- names(resampleIndex)[iter]
      ## If the user did not have nrow(x) in 'sizes', rfeIter added it.
      ## So, we need to find out how many set of predictions there are:
      nReps <- length(table(rfeResults$pred$Variables))
      rfeResults$pred$rowIndex <- rep(seq(along = y)[unique(holdoutIndex)], nReps)
    }

    if(is.factor(y) && length(lev) <= 50) {
      cells <- plyr::ddply(rfeResults$pred, .(Variables), function(x) flatTable(x$pred, x$obs))
      resamples <- merge(resamples, cells)
    }

    resamples$Resample <- names(resampleIndex)[iter]
    vars <- do.call("rbind", rfeResults$finalVariables)
    vars$Resample <- names(resampleIndex)[iter]
    list(resamples = resamples, selectedVars = vars, predictions = if(ctrl$saveDetails) rfeResults$pred else NULL)
  }
  resamples <- do.call("rbind", result[names(result) == "resamples"])
  rownames(resamples) <- NULL

  if(ctrl$method %in% c("boot632"))
  {
    perfNames <- names(resamples)
    perfNames <- perfNames[!(perfNames %in% c("Resample", "Variables"))]
    perfNames <- perfNames[!grepl("^cell[0-9]", perfNames)]
    apparent <- subset(resamples, Resample == "AllData")
    apparent <- apparent[,!grepl("^\\.cell|Resample", colnames(apparent)),drop = FALSE]
    names(apparent)[which(names(apparent) %in% perfNames)] <- paste(names(apparent)[which(names(apparent) %in% perfNames)],
                                                                    "Apparent", sep = "")
    names(apparent) <- gsub("^\\.", "", names(apparent))
    resamples <- subset(resamples, Resample != "AllData")
  }

  externPerf <- plyr::ddply(resamples[,!grepl("\\.cell|Resample", colnames(resamples)),drop = FALSE],
                      .(Variables),
                      MeanSD,
                      exclude = "Variables")
  if(ctrl$method %in% c("boot632"))
  {
    externPerf <- merge(externPerf, apparent)
    for(p in seq(along = perfNames))
    {
      const <- 1-exp(-1)
      externPerf[, perfNames[p]] <- (const * externPerf[, perfNames[p]]) +  ((1-const) * externPerf[, paste(perfNames[p],"Apparent", sep = "")])
    }
    externPerf <- externPerf[, !(names(externPerf) %in% paste(perfNames,"Apparent", sep = ""))]
  }
  list(performance = externPerf, everything = result)
}

#' @import foreach
looRfeWorkflow <- function(x, y, sizes, ppOpts, ctrl, lev, ...)
{
  loadNamespace("caret")
  ppp <- list(options = ppOpts)
  ppp <- c(ppp, ctrl$preProcOptions)

  resampleIndex <- ctrl$index
  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  result <- foreach(iter = seq(along = resampleIndex), .combine = "c", .verbose = FALSE, .errorhandling = "stop", .packages = "caret") %op%
  {
    loadNamespace("caret")
    requireNamespaceQuietStop("methods")
    modelIndex <- resampleIndex[[iter]]
    holdoutIndex <- -unique(resampleIndex[[iter]])

    seeds <- if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) ctrl$seeds[[iter]] else NA
    rfeResults <- rfeIter(subset_x(x, modelIndex),
                          y[modelIndex],
                          subset_x(x, holdoutIndex),
                          y[holdoutIndex],
                          sizes,
                          ctrl,
                          seeds = seeds,
                          ...)
    rfeResults
  }
  preds <- do.call("rbind", result[names(result) == "pred"])
  resamples <- ddply(preds, .(Variables), ctrl$functions$summary, lev = lev)
  list(performance = resamples, everything = result)
}


