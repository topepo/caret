
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
    if (!is.null(object$levels) && all(!is.na(object$levels))) {
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

## Convert the recipe to holdout data.
#' @importFrom recipes bake all_predictors all_outcomes has_role
holdout_rec <- function(object, dat, index) {
  ##
  ho_data <- bake(object$recipe,
                  new_data = subset_x(dat, index),
                  all_outcomes())
  names(ho_data) <- "obs"
  ## ~~~~~~ move these two to other functions:
  wt_cols <- role_cols(object$recipe, "case weight")
  if(length(wt_cols) > 0) {
    wts <- bake(object$recipe,
                new_data = subset_x(dat, index),
                has_role("case weight"))
    ho_data$weights <- get_vector(wts)
    rm(wts)
  }
  perf_cols <- role_cols(object$recipe, "performance var")
  if(length(perf_cols) > 0) {
    perf_data <- bake(object$recipe,
                      new_data = subset_x(dat, index),
                      has_role("performance var"))
    ho_data <- cbind(ho_data, perf_data)
  }
  ## ~~~~~~

  ho_data$rowIndex <- (1:nrow(dat))[index]
  ho_data <- as.data.frame(ho_data, stringsAsFactors = FALSE)
}

#' @importFrom recipes bake prep juice has_role
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
    other_dat <- if (is.matrix(dat) |
                     (is.data.frame(dat) & !inherits(dat, "tbl_df")))
      dat[, other_cols, drop = FALSE]
    else
      dat[, other_cols]

    tmp <- sampling$func(other_dat, y)
    orig_dat <- dat
    dat <- tmp$x
    dat[, y_cols] <- tmp$y
    rm(tmp, y, other_cols, other_dat, orig_dat)
  }

  trained_rec <- prep(rec, training = dat, fresh = TRUE,
                      verbose = FALSE, strings_as_factors = TRUE,
                      retain = TRUE)
  x <- juice(trained_rec, all_predictors())
  y <- juice(trained_rec, all_outcomes())
  y <- get_vector(y)

  is_weight <- summary(trained_rec)$role == "case weight"
  if(any(is_weight)) {
    if(sum(is_weight) > 1)
      stop("Ony one column can be used as a case weight.")
    weights <- juice(trained_rec, has_role("case weight"))
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
  x <- bake(object$recipe, new_data = newdata, all_predictors())
  out <- method$predict(modelFit = object$fit, newdata = x,
                        submodels = param)
  if(is.matrix(out) | is.data.frame(out))
    out <- out[,1]
  out
}

#' @importFrom recipes bake all_predictors
rec_prob <- function (method, object, newdata = NULL, param = NULL)  {
  x <- bake(object$recipe, new_data = newdata, all_predictors())
  obsLevels <- levels(object$fit)
  classProb <- method$prob(modelFit = object$fit, newdata = x,
                           submodels = param)
  if (!is.data.frame(classProb) & is.null(param)) {
    classProb <- as.data.frame(classProb, stringsAsFactors = FALSE)
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

              predicted <- trim_values(predicted, ctrl, is.null(lev))

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

      cbind(as.data.frame(t(out), stringsAsFactors = TRUE), info$loop[parm,,drop = FALSE])
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


  export <- c()

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

      predicted <- trim_values(predicted, ctrl, is.null(lev))

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

      thisResampleExtra <- optimism_rec(ctrl, dat, iter, lev, method, mod_rec, predicted,
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

train_adapt_rec <- function(rec, dat, info, method, ctrl, lev, metric, maximize, testing = FALSE, ...) {
  loadNamespace("caret")

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
                         .packages = pkgs,
                         .errorhandling = "stop") %:%
    foreach(parm = 1:nrow(info$loop),
            .combine = "c",
            .verbose = FALSE,
            .packages = pkgs,
            .errorhandling = "stop")  %op% {
              testing <- FALSE
              if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds)))
                set.seed(ctrl$seeds[[iter]][parm])

              loadNamespace("caret")
              loadNamespace("recipes")

              if(ctrl$verboseIter)
                progress(printed[parm,,drop = FALSE],
                         names(resampleIndex), iter)

              modelIndex <- resampleIndex[[iter]]
              holdoutIndex <- ctrl$indexOut[[iter]]

              if(testing) cat("pre-model\n")

              if(is.null(info$submodels[[parm]]) || nrow(info$submodels[[parm]]) > 0) {
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

              predicted <- trim_values(predicted, ctrl, is.null(lev))

              ##################################

              ho_data <- holdout_rec(mod_rec, dat, holdoutIndex)

              ##################################

              if(!is.null(submod)) {
                ## merge the fixed and seq parameter values together
                allParam <- expandParameters(info$loop[parm,,drop = FALSE], info$submodels[[parm]])
                allParam <- allParam[complete.cases(allParam),, drop = FALSE]

                ## collate the predicitons across all the sub-models
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
                  for(k in seq(along = predicted))
                    predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
                }

                if(keep_pred) {
                  tmpPred <- predicted
                  for(modIndex in seq(along = tmpPred)) {
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
                if(length(lev) > 1 && length(lev) <= 50) {
                  cells <- lapply(predicted,
                                  function(x) flatTable(x$pred, x$obs))
                  for(ind in seq(along = cells)) thisResample[[ind]] <- c(thisResample[[ind]], cells[[ind]])
                }
                thisResample <- do.call("rbind", thisResample)
                thisResample <- cbind(allParam, thisResample)

              } else {
                pred_val <- outcome_conversion(predicted, lv = lev)
                tmp <-  ho_data
                tmp$pred <- pred_val
                if(ctrl$classProbs) tmp <- cbind(tmp, probValues)

                if(keep_pred) {
                  tmpPred <- tmp
                  tmpPred$rowIndex <- holdoutIndex
                  tmpPred$Resample <- names(resampleIndex)[iter]
                } else tmpPred <- NULL

                ##################################
                thisResample <- ctrl$summaryFunction(tmp,
                                                     lev = lev,
                                                     model = method)

                ## if classification, get the confusion matrix
                if(length(lev) > 1 && length(lev) <= 5)
                  thisResample <- c(thisResample, flatTable(tmp$pred, tmp$obs))
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

      adapt_results <-
        foreach(parm = 1:nrow(new_info$loop),
                .combine = "c",
                .verbose = FALSE,
                .packages = c("methods", "caret"),
                .errorhandling = "stop")  %op%  {


                  if(ctrl$verboseIter)
                    progress(printed[parm,,drop = FALSE],
                             names(resampleIndex), iter, TRUE)

                  if(is.null(new_info$submodels[[parm]]) || nrow(new_info$submodels[[parm]]) > 0) {
                    submod <- new_info$submodels[[parm]]
                  } else submod <- NULL

                  mod_rec <- try(
                    rec_model(rec,
                              subset_x(dat, modelIndex),
                              method = method,
                              tuneValue = new_info$loop[parm,,drop = FALSE],
                              obsLevels = lev,
                              classProbs = ctrl$classProbs,
                              sampling = ctrl$sampling,
                              ...),
                    silent = TRUE)

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

                  predicted <- trim_values(predicted, ctrl, is.null(lev))

                  ##################################

                  ho_data <- holdout_rec(mod_rec, dat, holdoutIndex)

                  ##################################

                  if(!is.null(submod)) {
                    ## merge the fixed and seq parameter values together
                    allParam <- expandParameters(new_info$loop[parm,,drop = FALSE],
                                                 submod)
                    allParam <- allParam[complete.cases(allParam),, drop = FALSE]

                    ## collate the predicitons across all the sub-models
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
                      for(k in seq(along = predicted))
                        predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
                    }

                    if(keep_pred) {
                      tmpPred <- predicted
                      for(modIndex in seq(along = tmpPred)) {
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
                    if(length(lev) > 1 && length(lev) <= 50)
                      thisResample <- c(thisResample, flatTable(tmp$pred, tmp$obs))
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

    if(ctrl$verboseIter && length(excluded) > 0)
      cat(num_left, ifelse(num_left > 1, "remain\n", "remains\n"))

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
                            .verbose = FALSE,
                            .packages = pkgs,
                            .errorhandling = "stop") %:%
      foreach(parm = 1:nrow(new_info$loop),
              .combine = "c",
              .verbose = FALSE,
              .packages = pkgs,
              .errorhandling = "stop")  %op% {
                testing <- FALSE
                if(!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds)))
                  set.seed(ctrl$seeds[[iter]][parm])

                loadNamespace("caret")
                if(ctrl$verboseIter)
                  progress(printed[parm,,drop = FALSE],
                           names(resampleIndex), iter)

                modelIndex <- resampleIndex[[iter]]
                holdoutIndex <- ctrl$indexOut[[iter]]

                if(testing) cat("pre-model\n")

                if(is.null(info$submodels[[parm]]) || nrow(info$submodels[[parm]]) > 0) {
                  submod <- info$submodels[[parm]]
                } else submod <- NULL

                mod_rec <- try(
                  rec_model(rec,
                            subset_x(dat, modelIndex),
                            method = method,
                            tuneValue = new_info$loop[parm,,drop = FALSE],
                            obsLevels = lev,
                            classProbs = ctrl$classProbs,
                            sampling = ctrl$sampling,
                            ...),
                  silent = TRUE)

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

                predicted <- trim_values(predicted, ctrl, is.null(lev))

                ##################################

                ho_data <- holdout_rec(mod_rec, dat, holdoutIndex)

                ##################################

                if(!is.null(submod)) {
                  ## merge the fixed and seq parameter values together
                  allParam <- expandParameters(new_info$loop[parm,,drop = FALSE],
                                               new_info$submodels[[parm]])
                  allParam <- allParam[complete.cases(allParam),, drop = FALSE]

                  ## collate the predicitons across all the sub-models
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
                    for(k in seq(along = predicted))
                      predicted[[k]] <- cbind(predicted[[k]], probValues[[k]])
                  }

                  if(keep_pred) {
                    tmpPred <- predicted
                    for(modIndex in seq(along = tmpPred)) {
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
                  if(length(lev) > 1 && length(lev) <= 50)
                    thisResample <- c(thisResample, flatTable(tmp$pred, tmp$obs))
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
               gsub("^\\.", "", colnames(info$loop)),
               MeanSD,
               exclude = gsub("^\\.", "", colnames(info$loop)))
  num_resamp <- ddply(resamples,
                      gsub("^\\.", "", colnames(info$loop)),
                      function(x) c(Num_Resamples = nrow(x)))
  out <- merge(out, num_resamp)

  list(performance = out, resamples = resamples, predictions = if(keep_pred) pred else NULL)
}



