## TODO: clean up code duplication?
##       add na.action to all eval functions
##       change multiplier and alpha to confidence

#' @importFrom stats complete.cases
#' @importFrom utils head
#' @import foreach
adaptiveWorkflow <- function(x, y, wts, info, method, ppOpts, ctrl, lev,
                             metric, maximize, testing = FALSE, ...) {
  loadNamespace("caret")
  ppp <- list(options = ppOpts)
  ppp <- c(ppp, ctrl$preProcOptions)

  printed <- format(info$loop, digits = 4)
  colnames(printed) <- gsub("^\\.", "", colnames(printed))

  ## no 632, oob or loo
  resampleIndex <- ctrl$index

  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)

  pkgs <- c("methods", "caret")
  if(!is.null(method$library)) pkgs <- c(pkgs, method$library)

  init_index <- seq(along = resampleIndex)[1:(ctrl$adaptive$min-1)]
  extra_index <- seq(along = resampleIndex)[-(1:(ctrl$adaptive$min-1))]

  keep_pred <- isTRUE(ctrl$savePredictions) || ctrl$savePredictions %in% c("all", "final")

  init_result <- foreach(iter = seq(along = init_index),
                         .combine = "c",
                         .verbose = FALSE,
                         .errorhandling = "stop") %:%
    foreach(parm = 1:nrow(info$loop),
            .combine = "c",
            .verbose = FALSE,
            .packages = pkgs,
            .errorhandling = "stop")  %op% {
              testing <- FALSE
              if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) set.seed(ctrl$seeds[[iter]][parm])

              loadNamespace("caret")
              lapply(pkgs, requireNamespaceQuietStop)

              if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE],
                                            names(resampleIndex), iter)

              modelIndex <- resampleIndex[[iter]]
              holdoutIndex <- ctrl$indexOut[[iter]]

              if(testing) cat("pre-model\n")

              if(is.null(info$submodels[[parm]]) || nrow(info$submodels[[parm]]) > 0) {
                submod <- info$submodels[[parm]]
              } else submod <- NULL

              mod <- try(
                createModel(x = x[modelIndex,,drop = FALSE ],
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

              if(class(mod)[1] != "try-error") {
                predicted <- try(
                  predictionFunction(method = method,
                                     modelFit = mod$fit,
                                     newdata = x[holdoutIndex,, drop = FALSE],
                                     preProc = mod$preProc,
                                     param = submod),
                  silent = TRUE)

                if (inherits(predicted, "try-error")) {
                  wrn <- paste(colnames(printed[parm,,drop = FALSE]),
                               printed[parm,,drop = FALSE],
                               sep = "=",
                               collapse = ", ")
                  wrn <- paste("predictions failed for ", names(resampleIndex)[iter],
                               ": ", wrn, " ", as.character(predicted), sep = "")
                  if(ctrl$verboseIter) cat(wrn, "\n")
                  warning(wrn)
                  rm(wrn)

                  ## setup a dummy results with NA values for all predictions
                  nPred <- length(holdoutIndex)
                  if(!is.null(lev)) {
                    predicted <- rep("", nPred)
                    predicted[seq(along = predicted)] <- NA
                  } else {
                    predicted <- rep(NA, nPred)
                  }
                  if(!is.null(submod)) {
                    tmp <- predicted
                    predicted <- vector(mode = "list", length = nrow(info$submodels[[parm]]) + 1)
                    for(i in seq(along = predicted)) predicted[[i]] <- tmp
                    rm(tmp)
                  }
                }
              } else {
                wrn <- paste(colnames(printed[parm,,drop = FALSE]),
                             printed[parm,,drop = FALSE],
                             sep = "=",
                             collapse = ", ")
                wrn <- paste("model fit failed for ", names(resampleIndex)[iter],
                             ": ", wrn, " ", as.character(mod), sep = "")
                if(ctrl$verboseIter) cat(wrn, "\n")
                warning(wrn)
                rm(wrn)

                ## setup a dummy results with NA values for all predictions
                nPred <- length(holdoutIndex)
                if(!is.null(lev)) {
                  predicted <- rep("", nPred)
                  predicted[seq(along = predicted)] <- NA
                } else {
                  predicted <- rep(NA, nPred)
                }
                if(!is.null(submod)) {
                  tmp <- predicted
                  predicted <- vector(mode = "list", length = nrow(info$submodels[[parm]]) + 1)
                  for(i in seq(along = predicted)) predicted[[i]] <- tmp
                  rm(tmp)
                }
              }

              if(testing) print(head(predicted))
              if(ctrl$classProbs)
              {
                if(class(mod)[1] != "try-error") {
                  probValues <- probFunction(method = method,
                                             modelFit = mod$fit,
                                             newdata = x[holdoutIndex,, drop = FALSE],
                                             preProc = mod$preProc,
                                             param = submod)
                } else {
                  probValues <- as.data.frame(matrix(NA, nrow = nPred, ncol = length(lev)), stringsAsFactors = FALSE)
                  colnames(probValues) <- lev
                  if(!is.null(submod))
                  {
                    tmp <- probValues
                    probValues <- vector(mode = "list", length = nrow(info$submodels[[parm]]) + 1)
                    for(i in seq(along = probValues)) probValues[[i]] <- tmp
                    rm(tmp)
                  }
                }
                if(testing) print(head(probValues))
              }

              ##################################

              if(!is.null(submod))
              {
                ## merge the fixed and seq parameter values together
                allParam <- expandParameters(info$loop[parm,,drop = FALSE], info$submodels[[parm]])
                allParam <- allParam[complete.cases(allParam),, drop = FALSE]

                ## collate the predicitons across all the sub-models
                predicted <- lapply(predicted,
                                    function(x, y, wts, lv) {
                                      if(!is.factor(x) & is.character(x)) x <- factor(as.character(x), levels = lv)
                                      out <- data.frame(pred = x, obs = y, stringsAsFactors = FALSE)
                                      if(!is.null(wts)) out$weights <- wts
                                      out
                                    },
                                    y = y[holdoutIndex],
                                    wts = wts[holdoutIndex],
                                    lv = lev)
                if(testing) print(head(predicted))

                ## same for the class probabilities
                if(ctrl$classProbs) {
                  for(k in seq(along = predicted)) predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
                }

                if(keep_pred) {
                  tmpPred <- predicted
                  for(modIndex in seq(along = tmpPred))
                  {
                    tmpPred[[modIndex]]$rowIndex <- holdoutIndex
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
                if(length(lev) > 1) {
                  cells <- lapply(predicted,
                                  function(x) flatTable(x$pred, x$obs))
                  for(ind in seq(along = cells)) thisResample[[ind]] <- c(thisResample[[ind]], cells[[ind]])
                }
                thisResample <- do.call("rbind", thisResample)
                thisResample <- cbind(allParam, thisResample)

              } else {
                if(is.factor(y)) predicted <- factor(as.character(predicted), levels = lev)
                tmp <-  data.frame(pred = predicted,
                                   obs = y[holdoutIndex],
                                   stringsAsFactors = FALSE)
                ## Sometimes the code above does not coerce the first
                ## columnn to be named "pred" so force it
                names(tmp)[1] <- "pred"
                if(!is.null(wts)) tmp$weights <- wts[holdoutIndex]
                if(ctrl$classProbs) tmp <- cbind(tmp, probValues)

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
                if(length(lev) > 1) thisResample <- c(thisResample, flatTable(tmp$pred, tmp$obs))
                thisResample <- as.data.frame(t(thisResample), stringsAsFactors = FALSE)
                thisResample <- cbind(thisResample, info$loop[parm,,drop = FALSE])

              }
              thisResample$Resample <- names(resampleIndex)[iter]
              if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE],
                                            names(resampleIndex), iter, FALSE)
              list(resamples = thisResample, pred = tmpPred)
            } ## end initial loop over resamples and models


  init_resamp <- rbind.fill(init_result[names(init_result) == "resamples"])
  init_pred <- if(keep_pred)  rbind.fill(init_result[names(init_result) == "pred"]) else NULL
  names(init_resamp) <- gsub("^\\.", "", names(init_resamp))
  if(any(!complete.cases(init_resamp[,!grepl("^cell|Resample", colnames(init_resamp)),drop = FALSE])))
    warning("There were missing values in resampled performance measures.")

  init_summary <- ddply(init_resamp[,!grepl("^cell|Resample", colnames(init_resamp)),drop = FALSE],
                        ## TODO check this for seq models
                        gsub("^\\.", "", colnames(info$loop)),
                        MeanSD,
                        exclude = gsub("^\\.", "", colnames(info$loop)))

  new_info <- info
  num_left <- Inf
  for(iter in ctrl$adaptive$min:length(resampleIndex)) {

    if(num_left > 1) {
      modelIndex <- resampleIndex[[iter]]
      holdoutIndex <- ctrl$indexOut[[iter]]

      printed <- format(new_info$loop, digits = 4)
      colnames(printed) <- gsub("^\\.", "", colnames(printed))

      adapt_results <- foreach(parm = 1:nrow(new_info$loop),
                               .combine = "c",
                               .verbose = FALSE,
                               .packages = pkgs)  %op% {
                                 requireNamespaceQuietStop("methods")
                                 requireNamespaceQuietStop("caret")
                                 if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE],
                                                               names(resampleIndex), iter, TRUE)

                                 if(is.null(new_info$submodels[[parm]]) || nrow(new_info$submodels[[parm]]) > 0) {
                                   submod <- new_info$submodels[[parm]]
                                 } else submod <- NULL
                                 mod <- try(
                                   createModel(x = x[modelIndex,,drop = FALSE ],
                                               y = y[modelIndex],
                                               wts = wts[modelIndex],
                                               method = method,
                                               tuneValue = new_info$loop[parm,,drop = FALSE],
                                               obsLevels = lev,
                                               pp = ppp,
                                               classProbs = ctrl$classProbs,
                                               sampling = ctrl$sampling,
                                               ...),
                                   silent = TRUE)

                                 if(class(mod)[1] != "try-error") {
                                   predicted <- try(
                                     predictionFunction(method = method,
                                                        modelFit = mod$fit,
                                                        newdata = x[holdoutIndex,, drop = FALSE],
                                                        preProc = mod$preProc,
                                                        param = submod),
                                     silent = TRUE)

                                   if (inherits(predicted, "try-error"))  {
                                     wrn <- paste(colnames(printed[parm,,drop = FALSE]),
                                                  printed[parm,,drop = FALSE],
                                                  sep = "=",
                                                  collapse = ", ")
                                     wrn <- paste("predictions failed for ", names(resampleIndex)[iter],
                                                  ": ", wrn, " ", as.character(predicted), sep = "")
                                     if(ctrl$verboseIter) cat(wrn, "\n")
                                     warning(wrn)
                                     rm(wrn)

                                     ## setup a dummy results with NA values for all predictions
                                     nPred <- length(holdoutIndex)
                                     if(!is.null(lev)) {
                                       predicted <- rep("", nPred)
                                       predicted[seq(along = predicted)] <- NA
                                     } else {
                                       predicted <- rep(NA, nPred)
                                     }
                                     if(!is.null(submod)) {
                                       tmp <- predicted
                                       predicted <- vector(mode = "list", length = nrow(new_info$submodels[[parm]]) + 1)
                                       for(i in seq(along = predicted)) predicted[[i]] <- tmp
                                       rm(tmp)
                                     }
                                   }
                                 } else {
                                   wrn <- paste(colnames(printed[parm,,drop = FALSE]),
                                                printed[parm,,drop = FALSE],
                                                sep = "=",
                                                collapse = ", ")
                                   wrn <- paste("model fit failed for ", names(resampleIndex)[iter],
                                                ": ", wrn, " ", as.character(mod), sep = "")
                                   if(ctrl$verboseIter) cat(wrn, "\n")
                                   warning(wrn)
                                   rm(wrn)

                                   ## setup a dummy results with NA values for all predictions
                                   nPred <- length(holdoutIndex)
                                   if(!is.null(lev)) {
                                     predicted <- rep("", nPred)
                                     predicted[seq(along = predicted)] <- NA
                                   } else {
                                     predicted <- rep(NA, nPred)
                                   }
                                   if(!is.null(submod)) {
                                     tmp <- predicted
                                     predicted <- vector(mode = "list", length = nrow(new_info$submodels[[parm]]) + 1)
                                     for(i in seq(along = predicted)) predicted[[i]] <- tmp
                                     rm(tmp)
                                   }
                                 }

                                 if(testing) print(head(predicted))
                                 if(ctrl$classProbs)
                                 {
                                   if(class(mod)[1] != "try-error") {
                                     probValues <- probFunction(method = method,
                                                                modelFit = mod$fit,
                                                                newdata = x[holdoutIndex,, drop = FALSE],
                                                                preProc = mod$preProc,
                                                                param = submod)
                                   } else {
                                     probValues <- as.data.frame(matrix(NA, nrow = nPred, ncol = length(lev)), stringsAsFactors = FALSE)
                                     colnames(probValues) <- lev
                                     if(!is.null(submod))
                                     {
                                       tmp <- probValues
                                       probValues <- vector(mode = "list", length = nrow(new_info$submodels[[parm]]) + 1)
                                       for(i in seq(along = probValues)) probValues[[i]] <- tmp
                                       rm(tmp)
                                     }
                                   }
                                   if(testing) print(head(probValues))
                                 }

                                 ##################################

                                 if(!is.null(submod))
                                 {
                                   ## merge the fixed and seq parameter values together
                                   allParam <- expandParameters(new_info$loop[parm,,drop = FALSE],
                                                                submod)
                                   allParam <- allParam[complete.cases(allParam),, drop = FALSE]

                                   ## collate the predicitons across all the sub-models
                                   predicted <- lapply(predicted,
                                                       function(x, y, wts, lv) {
                                                         if(!is.factor(x) & is.character(x)) x <- factor(as.character(x), levels = lv)
                                                         out <- data.frame(pred = x, obs = y, stringsAsFactors = FALSE)
                                                         if(!is.null(wts)) out$weights <- wts
                                                         out
                                                       },
                                                       y = y[holdoutIndex],
                                                       wts = wts[holdoutIndex],
                                                       lv = lev)
                                   if(testing) print(head(predicted))

                                   ## same for the class probabilities
                                   if(ctrl$classProbs) {
                                     for(k in seq(along = predicted)) predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
                                   }

                                   if(keep_pred) {
                                     tmpPred <- predicted
                                     for(modIndex in seq(along = tmpPred))
                                     {
                                       tmpPred[[modIndex]]$rowIndex <- holdoutIndex
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
                                   if(length(lev) > 1) {
                                     cells <- lapply(predicted,
                                                     function(x) flatTable(x$pred, x$obs))
                                     for(ind in seq(along = cells)) thisResample[[ind]] <- c(thisResample[[ind]], cells[[ind]])
                                   }
                                   thisResample <- do.call("rbind", thisResample)
                                   thisResample <- cbind(allParam, thisResample)

                                 } else {
                                   if(is.factor(y)) predicted <- factor(as.character(predicted), levels = lev)
                                   tmp <-  data.frame(pred = predicted,
                                                      obs = y[holdoutIndex],
                                                      stringsAsFactors = FALSE)
                                   ## Sometimes the code above does not coerce the first
                                   ## columnn to be named "pred" so force it
                                   names(tmp)[1] <- "pred"
                                   if(!is.null(wts)) tmp$weights <- wts[holdoutIndex]
                                   if(ctrl$classProbs) tmp <- cbind(tmp, probValues)

                                   if(keep_pred) {
                                     tmpPred <- tmp
                                     tmpPred$rowIndex <- holdoutIndex
                                     tmpPred <- merge(tmpPred, new_info$loop[parm,,drop = FALSE],
                                                      all = TRUE)
                                     tmpPred$Resample <- names(resampleIndex)[iter]
                                   } else tmpPred <- NULL

                                   ##################################
                                   thisResample <- ctrl$summaryFunction(tmp,
                                                                        lev = lev,
                                                                        model = method)

                                   ## if classification, get the confusion matrix
                                   if(length(lev) > 1) thisResample <- c(thisResample, flatTable(tmp$pred, tmp$obs))
                                   thisResample <- as.data.frame(t(thisResample), stringsAsFactors = FALSE)
                                   thisResample <- cbind(thisResample, new_info$loop[parm,,drop = FALSE])

                                 }
                                 thisResample$Resample <- names(resampleIndex)[iter]
                                 if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE],
                                                               names(resampleIndex), iter, FALSE)
                                 list(resamples = thisResample, pred = tmpPred)
                               } ## end initial loop over resamples and models

    }

    init_result <- c(init_result, adapt_results)
    rs <- do.call("rbind", init_result[names(init_result) == "resamples"])


    current_mods <- get_id(rs, as.character(method$param$parameter))
    if(iter > ctrl$adaptive$min) {
      latest <- do.call("rbind", adapt_results[names(adapt_results) == "resamples"])
      latest <- latest[,as.character(method$param$parameter),drop = FALSE]
      latest <- latest[!duplicated(latest),,drop = FALSE]

      current_mods <- merge(current_mods, latest)
    }
    rs <- merge(rs, current_mods)

    if(iter == ctrl$adaptive$min+1) {
      rs <- filter_on_diff(rs, metric,
                           cutoff = .001,
                           maximize = maximize,
                           verbose = ctrl$verboseIter)
    }

    if(ctrl$adaptive$method == "BT") {
      filtered_mods <- try(bt_eval(rs, metric = metric, maximize = maximize,
                                   alpha = ctrl$adaptive$alpha),
                           silent = TRUE)
    } else {
      filtered_mods <- try(gls_eval(rs, metric = metric, maximize = maximize,
                                    alpha = ctrl$adaptive$alpha),
                           silent = TRUE)
    }

    if(class(filtered_mods)[1] == "try-error") {
      if(ctrl$verboseIter) {
        cat("x parameter filtering failed:")
        print(filtered_mods)
        cat("\n")
      }
      filtered_mods <- current_mods
    }

    if(ctrl$verboseIter) {
      excluded <- unique(rs$model_id)[!(unique(rs$model_id) %in% filtered_mods)]
      if(length(excluded) > 0) {
        cat(paste("o", length(excluded), "eliminated;"))
      } else cat("o no models eliminated;",
                 nrow(current_mods),
                 ifelse(nrow(current_mods) > 1, "remain\n", "remains\n"))
    }

    current_mods <- current_mods[current_mods$model_id %in% filtered_mods,,drop = FALSE]
    if(iter == ctrl$adaptive$min) {
      last_mods <- current_mods
    }
    current_mods$model_id <- NULL
    num_left <- nrow(current_mods)

    if(ctrl$verboseIter && length(excluded) > 0) cat(num_left,
                                                     ifelse(num_left > 1, "remain\n", "remains\n"))

    if(!is.null(method$loop)) {
      new_info <- method$loop(current_mods)
    } else new_info$loop <- current_mods

    last_iter <- iter

    if(num_left == 1) break
  }
  ## finish up last resamples
  if(ctrl$adaptive$complete && last_iter < length(ctrl$index)) {
    printed <- format(new_info$loop, digits = 4)
    colnames(printed) <- gsub("^\\.", "", colnames(printed))

    final_index <- seq(along = resampleIndex)[(last_iter+1):length(ctrl$index)]
    final_result <- foreach(iter = final_index,
                            .combine = "c",
                            .verbose = FALSE) %:%
      foreach(parm = 1:nrow(new_info$loop),
              .combine = "c",
              .verbose = FALSE,
              .packages = pkgs)  %op% {
                testing <- FALSE
                if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) set.seed(ctrl$seeds[[iter]][parm])

                loadNamespace("caret")
                lapply(pkgs, requireNamespaceQuietStop)

                if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE],
                                              names(resampleIndex), iter)

                modelIndex <- resampleIndex[[iter]]
                holdoutIndex <- ctrl$indexOut[[iter]]

                if(testing) cat("pre-model\n")

                if(is.null(info$submodels[[parm]]) || nrow(info$submodels[[parm]]) > 0) {
                  submod <- info$submodels[[parm]]
                } else submod <- NULL

                mod <- try(
                  createModel(x = x[modelIndex,,drop = FALSE ],
                              y = y[modelIndex],
                              wts = wts[modelIndex],
                              method = method,
                              tuneValue = new_info$loop[parm,,drop = FALSE],
                              obsLevels = lev,
                              pp = ppp,
                              classProbs = ctrl$classProbs,
                              sampling = ctrl$sampling,
                              ...),
                  silent = TRUE)

                if(class(mod)[1] != "try-error") {
                  predicted <- try(
                    predictionFunction(method = method,
                                       modelFit = mod$fit,
                                       newdata = x[holdoutIndex,, drop = FALSE],
                                       preProc = mod$preProc,
                                       param = submod),
                    silent = TRUE)

                  if (inherits(predicted, "try-error")) {
                    wrn <- paste(colnames(printed[parm,,drop = FALSE]),
                                 printed[parm,,drop = FALSE],
                                 sep = "=",
                                 collapse = ", ")
                    wrn <- paste("predictions failed for ", names(resampleIndex)[iter],
                                 ": ", wrn, " ", as.character(predicted), sep = "")
                    if(ctrl$verboseIter) cat(wrn, "\n")
                    warning(wrn)
                    rm(wrn)

                    ## setup a dummy results with NA values for all predictions
                    nPred <- length(holdoutIndex)
                    if(!is.null(lev)) {
                      predicted <- rep("", nPred)
                      predicted[seq(along = predicted)] <- NA
                    } else {
                      predicted <- rep(NA, nPred)
                    }
                    if(!is.null(submod)) {
                      tmp <- predicted
                      predicted <- vector(mode = "list", length = nrow(info$submodels[[parm]]) + 1)
                      for(i in seq(along = predicted)) predicted[[i]] <- tmp
                      rm(tmp)
                    }
                  }
                } else {
                  wrn <- paste(colnames(printed[parm,,drop = FALSE]),
                               printed[parm,,drop = FALSE],
                               sep = "=",
                               collapse = ", ")
                  wrn <- paste("model fit failed for ", names(resampleIndex)[iter],
                               ": ", wrn, " ", as.character(mod), sep = "")
                  if(ctrl$verboseIter) cat(wrn, "\n")
                  warning(wrn)
                  rm(wrn)

                  ## setup a dummy results with NA values for all predictions
                  nPred <- length(holdoutIndex)
                  if(!is.null(lev)) {
                    predicted <- rep("", nPred)
                    predicted[seq(along = predicted)] <- NA
                  } else {
                    predicted <- rep(NA, nPred)
                  }
                  if(!is.null(submod)) {
                    tmp <- predicted
                    predicted <- vector(mode = "list", length = nrow(info$submodels[[parm]]) + 1)
                    for(i in seq(along = predicted)) predicted[[i]] <- tmp
                    rm(tmp)
                  }
                }

                if(testing) print(head(predicted))
                if(ctrl$classProbs)
                {
                  if(class(mod)[1] != "try-error") {
                    probValues <- probFunction(method = method,
                                               modelFit = mod$fit,
                                               newdata = x[holdoutIndex,, drop = FALSE],
                                               preProc = mod$preProc,
                                               param = submod)
                  } else {
                    probValues <- as.data.frame(matrix(NA, nrow = nPred, ncol = length(lev)), stringsAsFactors = FALSE)
                    colnames(probValues) <- lev
                    if(!is.null(submod))
                    {
                      tmp <- probValues
                      probValues <- vector(mode = "list", length = nrow(info$submodels[[parm]]) + 1)
                      for(i in seq(along = probValues)) probValues[[i]] <- tmp
                      rm(tmp)
                    }
                  }
                  if(testing) print(head(probValues))
                }

                ##################################

                if(!is.null(submod))
                {
                  ## merge the fixed and seq parameter values together
                  allParam <- expandParameters(new_info$loop[parm,,drop = FALSE],
                                               new_info$submodels[[parm]])
                  allParam <- allParam[complete.cases(allParam),, drop = FALSE]

                  ## collate the predicitons across all the sub-models
                  predicted <- lapply(predicted,
                                      function(x, y, wts, lv) {
                                        if(!is.factor(x) & is.character(x)) x <- factor(as.character(x), levels = lv)
                                        out <- data.frame(pred = x, obs = y, stringsAsFactors = FALSE)
                                        if(!is.null(wts)) out$weights <- wts
                                        out
                                      },
                                      y = y[holdoutIndex],
                                      wts = wts[holdoutIndex],
                                      lv = lev)
                  if(testing) print(head(predicted))

                  ## same for the class probabilities
                  if(ctrl$classProbs) {
                    for(k in seq(along = predicted)) predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
                  }

                  if(keep_pred) {
                    tmpPred <- predicted
                    for(modIndex in seq(along = tmpPred))
                    {
                      tmpPred[[modIndex]]$rowIndex <- holdoutIndex
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
                  if(length(lev) > 1) {
                    cells <- lapply(predicted,
                                    function(x) flatTable(x$pred, x$obs))
                    for(ind in seq(along = cells)) thisResample[[ind]] <- c(thisResample[[ind]], cells[[ind]])
                  }
                  thisResample <- do.call("rbind", thisResample)
                  thisResample <- cbind(allParam, thisResample)

                } else {
                  if(is.factor(y)) predicted <- factor(as.character(predicted), levels = lev)
                  tmp <-  data.frame(pred = predicted,
                                     obs = y[holdoutIndex],
                                     stringsAsFactors = FALSE)
                  ## Sometimes the code above does not coerce the first
                  ## columnn to be named "pred" so force it
                  names(tmp)[1] <- "pred"
                  if(!is.null(wts)) tmp$weights <- wts[holdoutIndex]
                  if(ctrl$classProbs) tmp <- cbind(tmp, probValues)

                  if(keep_pred) {
                    tmpPred <- tmp
                    tmpPred$rowIndex <- holdoutIndex
                    tmpPred <- merge(tmpPred, new_info$loop[parm,,drop = FALSE],
                                     all = TRUE)
                    tmpPred$Resample <- names(resampleIndex)[iter]
                  } else tmpPred <- NULL

                  ##################################
                  thisResample <- ctrl$summaryFunction(tmp,
                                                       lev = lev,
                                                       model = method)

                  ## if classification, get the confusion matrix
                  if(length(lev) > 1) thisResample <- c(thisResample, flatTable(tmp$pred, tmp$obs))
                  thisResample <- as.data.frame(t(thisResample), stringsAsFactors = FALSE)
                  thisResample <- cbind(thisResample, new_info$loop[parm,,drop = FALSE])

                }
                thisResample$Resample <- names(resampleIndex)[iter]
                if(ctrl$verboseIter) progress(printed[parm,,drop = FALSE],
                                              names(resampleIndex), iter, FALSE)
                list(resamples = thisResample, pred = tmpPred)
              } ## end final loop to finish cleanup resamples and models
    init_result <- c(init_result, final_result)
  }
  resamples <- rbind.fill(init_result[names(init_result) == "resamples"])
  pred <- if(keep_pred)  rbind.fill(init_result[names(init_result) == "pred"]) else NULL
  names(resamples) <- gsub("^\\.", "", names(resamples))

  if(any(!complete.cases(resamples[,!grepl("^cell|Resample", colnames(resamples)),drop = FALSE])))
    warning("There were missing values in resampled performance measures.")

  out <- ddply(resamples[,!grepl("^cell|Resample", colnames(resamples)),drop = FALSE],
               ## TODO check this for seq models
               gsub("^\\.", "", colnames(info$loop)),
               MeanSD,
               exclude = gsub("^\\.", "", colnames(info$loop)))
  num_resamp <- ddply(resamples,
                      gsub("^\\.", "", colnames(info$loop)),
                      function(x) c(.B = nrow(x)))
  out <- merge(out, num_resamp)

  list(performance = out, resamples = resamples, predictions = if(keep_pred) pred else NULL)
}

#' @importFrom stats reshape
long2wide <- function(x, metric) {
  x2 <- reshape(x[, c(metric, "model_id", "Resample")],
                idvar = "Resample",
                timevar = "model_id",
                v.names = metric,
                direction = "wide")
  colnames(x2) <- gsub(paste(metric, "\\.", sep = ""), "", colnames(x2))
  x2[order(x2$Resample), c("Resample", sort(as.character(unique(x$model_id))))]
}

get_id <- function(x, param) {
  params <- x[,param,drop=FALSE]
  params <- params[!duplicated(params),,drop=FALSE]
  params$model_id <- paste("m", gsub(" ", "0", format(1:nrow(params))), sep = "")
  rownames(params) <- NULL
  params
}

#' @importFrom stats qnorm
bt_eval <- function(rs, metric, maximize, alpha = 0.05) {
  if (!requireNamespace("BradleyTerry2")) stop("BradleyTerry2 package missing")
  se_thresh <- 100
  constant <- qnorm(1 - alpha)
  rs <- rs[order(rs$Resample, rs$model_id),]
  rs <- rs[!is.na(rs[, metric]), ]
  scores <- ddply(rs, .(Resample), get_scores, maximize = maximize, metric = metric)
  scores <- ddply(scores, .(player1, player2), function(x) c(win1 = sum(x$win1),
                                                             win2 = sum(x$win2)))
  if(length(unique(rs$Resample)) >= 5) {
    tmp_scores <- try(skunked(scores), silent = TRUE)
    if (inherits(tmp_scores, "try-error")) scores <- tmp_scores
  }
  best_mod <- ddply(rs, .(model_id), function(x, metric) mean(x[, metric], na.rm = TRUE), metric = metric)
  best_mod <- if(maximize)
    best_mod$model_id[which.max(best_mod$V1)] else
      best_mod$model_id[which.min(best_mod$V1)]
  btModel <- BradleyTerry2::BTm(cbind(win1, win2), player1, player2, data = scores, refcat = best_mod)
  btCoef <- summary(btModel)$coef
  ## Recent versions of the BradleyTerry2 package tag on some dots in the names
  rownames(btCoef) <- gsub("^\\.\\.", "", rownames(btCoef))
  upperBound <- btCoef[, "Estimate"] + constant*btCoef[, "Std. Error"]
  if(any(btCoef[, "Std. Error"] > se_thresh)) {
    ## These players either are uniformly dominated (='dom') or dominating
    dom1 <- btCoef[, "Std. Error"] > se_thresh
    dom2 <- if(maximize) btCoef[, "Estimate"] <= 0 else btCoef[, "Estimate"] >= 0
    dom <- dom1 & dom2
  } else dom <- rep(FALSE, length(upperBound))
  bound <- upperBound >= 0
  keepers <- names(upperBound)[bound & !dom]
  unique(c(best_mod, keepers))
}

get_scores <- function(x, maximize = NULL, metric = NULL)
{
  if (!requireNamespace("BradleyTerry2")) stop("BradleyTerry2 package missing")
  delta <- outer(x[,metric], x[,metric], "-")
  tied <- ifelse(delta == 0, 1, 0)*.5
  diag(tied) <- 0
  binary <- if(maximize) ifelse(delta > 0, 1, 0) else ifelse(delta > 0, 0, 1)
  binary <- binary + tied
  diag(binary) <- 0
  rownames(binary) <- colnames(binary) <- x$model_id
  BradleyTerry2::countsToBinomial(as.table(binary))
}

## check to see if there are any models/players that never won a game
skunked <- function(scores, verbose = TRUE) {
  p1 <- ddply(scores, .(player1), function(x) sum(x$win1))
  p2 <- ddply(scores, .(player2), function(x) sum(x$win2))
  names(p1)[1] <- names(p2)[1] <- "playa"
  by_player <- ddply(rbind(p1, p2), .(playa), function(x) c(wins = sum(x$V1)))
  if(any(by_player$wins < 1)) {
    skunked <- as.character(by_player$playa[by_player$wins < 1])
    if(verbose) cat("o", sum(by_player$wins < 1),
                    ifelse(sum(by_player$wins < 1) > 1, "models were", "model was"),
                    "skunked\n")
    scores <- subset(scores, !(player1 %in% skunked))
    scores <- subset(scores, !(player2 %in% skunked))
    levs <- sort(unique(c(as.character(scores$player1),
                          as.character(scores$player2))))
    scores$player1 <- factor(as.character(scores$player1), levels = levs)
    scores$player2 <- factor(as.character(scores$player2), levels = levs)
  }
  scores
}


#' @importFrom stats cor t.test na.omit
gls_eval <- function(x, metric, maximize, alpha = 0.05) {
  x <- x[!is.na(x[, metric]), ]
  means <- ddply(x[, c(metric, "model_id")],
                 .(model_id),
                 function(x, met) c(mean = mean(x[, met], na.rm = TRUE)),
                 met = metric)
  means <- if(maximize) means[order(-means$mean),] else means[order(means$mean),]
  levs <- as.character(means$model_id)
  bl <- levs[1]
  bldat <- subset(x, model_id == bl)[, c("Resample", metric)]
  colnames(bldat)[2] <- ".baseline"
  x2 <- merge(bldat, x[, c("Resample", "model_id", metric)])
  x2$value <- if(maximize) x2$.baseline - x2[, metric] else x2[, metric] - x2$.baseline
  x2 <- subset(x2, model_id != bl)
  x2$model_id <- factor(x2$model_id, levels = levs[-1])
  if(length(levs) > 2) {
    gls_fit <- try(gls(value ~ model_id - 1, data = x2,
                       corCompSymm(form = ~ 1 | Resample),
                       na.action = na.omit),
                   silent = TRUE)
    if(class(gls_fit)[1] != "try-error") {
      lvl2 <- 1 - (2*alpha) ## convert to one sided
      ci <- intervals(gls_fit, which = "coef", level = lvl2)$coef[,1]
      keepers <- ci <= 0
      if(any(is.na(keepers))) keepers[is.na(keepers)] <- TRUE
      keepers <- names(ci)[keepers]
      keepers <- gsub("model_id", "", keepers)
    } else keepers <- levs
  } else {
    ttest <- t.test(x2$value, alternative = "greater")$p.value
    keepers <- if(!is.na(ttest) && ttest >= alpha) levs[-1] else NULL
  }
  unique(c(bl, keepers))
}

#' @importFrom stats4 coef
#' @importFrom stats t.test lm pt
seq_eval <- function(x, metric, maximize, alpha = 0.05) {
  means <- ddply(x[, c(metric, "model_id")],
                 .(model_id),
                 function(x, met) c(mean = mean(x[, met], na.rm = TRUE)),
                 met = metric)
  means <- if(maximize) means[order(-means$mean),] else means[order(means$mean),]
  levs <- as.character(means$model_id)
  bl <- levs[1]
  bldat <- subset(x, model_id == bl)[, c("Resample", metric)]
  colnames(bldat)[2] <- ".baseline"
  x2 <- merge(bldat, x[, c("Resample", "model_id", metric)])
  x2$value <- if(maximize) x2$.baseline - x2[, metric] else x2[, metric] - x2$.baseline
  x2 <- subset(x2, model_id != bl)
  x2$model_id <- factor(x2$model_id, levels = levs[-1])
  if(length(levs) > 2) {
    fit <- lm(value ~ . - 1, data = x2[, c("model_id", "value")])
    fit <- summary(fit)
    pvals <- pt(coef(fit)[, 3], fit$df[2], lower.tail = FALSE)
    names(pvals) <- gsub("model_id", "", names(pvals))
    keepers <- pvals >= alpha
    if(any(is.na(keepers))) keepers[is.na(keepers)] <- TRUE
    keepers <- names(pvals)[keepers]
    keepers <- gsub("model_id", "", keepers)
  } else {
    ttest <- t.test(x2$value, alternative = "greater")$p.value
    keepers <- if(!is.na(ttest) && ttest >= alpha) levs[-1] else NULL
  }
  unique(c(bl, keepers))
}

retrospective <- function(x, B = 5, method = "BT", alpha = 0.05) {
  rs <- x$resample
  if(!is.factor(rs$Resample)) rs$Resample <- factor(rs$Resample)
  rs <- subset(rs, as.numeric(Resample) <= B)
  current_mods <- get_id(rs, as.character(x$modelInfo$param$parameter))
  #   current_mods <- merge(current_mods, new_loop)
  rs <- merge(rs, current_mods)

  if(method == "BT") {
    filtered_mods <- try(bt_eval(rs, metric = x$metric, maximize = x$maximize,
                                 alpha = alpha),
                         silent = TRUE)
  } else {
    filtered_mods <- try(gls_eval(rs, metric = x$metric, maximize = x$maximize,
                                  alpha = alpha),
                         silent = TRUE)
  }


  list(models = filtered_mods, mods = subset(current_mods, model_id %in% filtered_mods),
       long = rs, wide = long2wide(rs, x$metric))
}

cccmat <- function(dat) {
  p <- ncol(dat)
  out <- matrix(1, ncol = p, nrow = p)
  for(i in 1:p) {
    for(j in i:p) {
      if(i > j) {
        tmp <- ccc(dat[,i], dat[,j])
        out[i, j] <- out[j, i] <- tmp
      }
    }
  }
  out[lower.tri(out)] <- out[upper.tri(out)]
  colnames(out) <- rownames(out) <- colnames(dat)
  out
}

#' @importFrom stats cov
ccc <- function(x, y) {
  covm <- cov(cbind(x, y), use = "pairwise.complete.obs")
  mnx <- mean(x, na.rm = TRUE)
  mny <- mean(y, na.rm = TRUE)
  2*covm[1,2]/(covm[1,1] + covm[2,2] + (mnx - mny)^2)
}

#' @importFrom stats sd
diffmat <- function(dat) {
  p <- ncol(dat)
  out <- matrix(NA, ncol = p, nrow = p)
  for(i in 1:p) {
    for(j in i:p) {
      if(i < j) {
        x <- dat[,i]  - dat[,j]
        tmpm <- abs(mean(x, na.rm = TRUE))
        tmps <- sd(x, na.rm = TRUE)
        out[i, j] <- out[j, i] <- if(tmps < .Machine$double.eps^0.5) 0 else tmpm/tmps
      }
    }
  }
  out[lower.tri(out)] <- out[upper.tri(out)]
  colnames(out) <- rownames(out) <- colnames(dat)
  out
}



filter_on_diff <- function(dat, metric, cutoff = 0.01, maximize = TRUE, verbose = FALSE) {
  x <- long2wide(x = dat, metric = metric)
  mns <- colMeans(x[, -1], na.rm = TRUE)
  if(!maximize) mns <- -mns
  x <- diffmat(x[, -1])
  tmp <- x
  diag(tmp) <- Inf
  if(!any(tmp < cutoff)) return(dat)
  varnum <- dim(x)[1]
  originalOrder <- 1:varnum
  averageDiff <- function(x) mean(x, na.rm = TRUE)
  tmp <- x
  diag(tmp) <- NA
  maxAbsCorOrder <- order(apply(tmp, 2, averageDiff), decreasing = TRUE)
  x <- x[maxAbsCorOrder, maxAbsCorOrder]
  mns <- mns[maxAbsCorOrder]
  newOrder <- originalOrder[maxAbsCorOrder]
  deletecol <- 0
  for (i in 1:(varnum - 1)) {
    for (j in (i + 1):varnum) {
      if (!any(i == deletecol) & !any(j == deletecol)) {
        if (abs(x[i, j]) < cutoff) {
          if (mns[i] < mns[j]) {
            deletecol <- unique(c(deletecol, i))
          } else {
            deletecol <- unique(c(deletecol, j))
          }
        }
      }
    }
  }

  deletecol <- deletecol[deletecol != 0]
  if(length(deletecol) > 0) {
    dumped <- colnames(x)[newOrder[deletecol]]
    if (verbose)  cat(paste("o", length(deletecol),
                            ifelse(length(deletecol) > 1, "models of", "model of"),
                            varnum,
                            ifelse(length(deletecol) > 1, "were", "was"),
                            "eliminated due to linear dependencies\n"))
    dat <- subset(dat, !(model_id %in% dumped))
  }
  dat
}



filter_on_corr <- function(dat, metric, cutoff, verbose = FALSE) {
  x <- long2wide(x = dat, metric = metric)
#   x <- cor(x[, -1], use = "pairwise.complete.obs")
  x <- cccmat(x[, -1])
  varnum <- dim(x)[1]
  if (!isTRUE(all.equal(x, t(x))))
    stop("correlation matrix is not symmetric")
  if (varnum == 1)
    stop("only one variable given")
  x <- abs(x)
  originalOrder <- 1:varnum
  averageCorr <- function(x) mean(x, na.rm = TRUE)
  tmp <- x
  diag(tmp) <- NA
  maxAbsCorOrder <- order(apply(tmp, 2, averageCorr), decreasing = TRUE)
  x <- x[maxAbsCorOrder, maxAbsCorOrder]
  newOrder <- originalOrder[maxAbsCorOrder]
  deletecol <- 0
  for (i in 1:(varnum - 1)) {
    for (j in (i + 1):varnum) {
      if (!any(i == deletecol) & !any(j == deletecol)) {
        if (abs(x[i, j]) > cutoff) {
          if (mean(x[i, -i], na.rm = TRUE) > mean(x[-j, j], na.rm = TRUE)) {
            deletecol <- unique(c(deletecol, i))
          } else {
            deletecol <- unique(c(deletecol, j))
          }
        }
      }
    }
  }
  deletecol <- deletecol[deletecol != 0]
  if(length(deletecol) > 0) {
    dumped <- colnames(x)[newOrder[deletecol]]
    if (verbose)  cat(paste("o", length(deletecol),
                            ifelse(length(deletecol) > 1, "models were", "model was"),
                            "eliminated due to linear dependencies\n"))
    dat <- subset(dat, !(model_id %in% dumped))
  }
  dat
}
