#' Create Data to Plot a Learning Curve
#'
#' For a given model, this function fits several versions on different sizes of
#' the total training set and returns the results
#'
#' This function creates a data set that can be used to plot how well the model
#' performs over different sized versions of the training set. For each data
#' set size, the performance metrics are determined and saved. If
#' \code{test_prop == 0}, the apparent measure of performance (i.e.
#' re-predicting the training set) and the resampled estimate of performance
#' are available. Otherwise, the test set results are also added.
#'
#' If the model being fit has tuning parameters, the results are based on the
#' optimal settings determined by \code{\link{train}}.
#'
#' @param dat the training data
#' @param outcome a character string identifying the outcome column name
#' @param proportion the incremental proportions of the training set that are
#' used to fit the model
#' @param test_prop an optional proportion of the data to be used to measure
#' performance.
#' @param verbose a logical to print logs to the screen as models are fit
#' @param \dots options to pass to \code{\link{train}} to specify the model.
#' These should not include \code{x}, \code{y}, \code{formula}, or \code{data}.
#' If \code{trainControl} is used here, do not use \code{method = "none"}.
#' @return a data frame with columns for each performance metric calculated by
#' \code{\link{train}} as well as columns: \item{Training_Size }{the number of
#' data points used in the current model fit} \item{Data }{which data were used
#' to calculate performance. Values are "Resampling", "Training", and
#' (optionally) "Testing"} In the results, each data set size will have one row
#' for the apparent error rate, one row for the test set results (if used) and
#' as many rows as resamples (e.g. 10 rows if 10-fold CV is used).
#' @author Max Kuhn
#' @seealso \code{\link{train}}
#' @keywords models
#' @examples
#'
#' \dontrun{
#' set.seed(1412)
#' class_dat <- twoClassSim(1000)
#'
#' ctrl <- trainControl(classProbs = TRUE,
#'                      summaryFunction = twoClassSummary)
#'
#' set.seed(29510)
#' lda_data <-
#'   learning_curve_dat(dat = class_dat,
#'                      outcome = "Class",
#'                      test_prop = 1/4,
#'                      ## `train` arguments:
#'                      method = "lda",
#'                      metric = "ROC",
#'                      trControl = ctrl)
#'
#'
#'
#' ggplot(lda_data, aes(x = Training_Size, y = ROC, color = Data)) +
#'   geom_smooth(method = loess, span = .8) +
#'   theme_bw()
#'  }
#'
#' @export learning_curve_dat
learning_curve_dat <- function(dat,
                              outcome = NULL,
                              proportion = (1:10)/10,
                              test_prop = 0,
                              verbose = TRUE, ...) {
  if(is.null(outcome))
    stop("Please give a character stirng for the outcome column name")
  proportion <- sort(unique(proportion))
  n_size <- length(proportion)

  if(test_prop > 0) {
    for_model <- createDataPartition(dat[, outcome], p = 1 - test_prop, list = FALSE)
  } else for_model <- 1:nrow(dat)

  n <- length(for_model)

  resampled <- vector(mode = "list", length = n_size)
  tested <- if(test_prop > 0) resampled else NULL
  apparent <- resampled
  for(i in seq(along = proportion)) {
    if(verbose) cat("Training for ", round(proportion[i]*100, 1),
                    "% (n = ", floor(n*proportion[i]), ")\n", sep = "")
    in_mod <- if(proportion[i] < 1) sample(for_model, size = floor(n*proportion[i])) else for_model
    mod <- train(x = dat[in_mod, colnames(dat) != outcome, drop = FALSE],
                 y = dat[in_mod, outcome],
                 ...)
    if (mod$control$method == "none")
      stop("`learning_curve_dat` uses resampling so please choose a value of ",
           "`method` that is not 'none'", call. = FALSE)

    if(i == 1) perf_names <- mod$perfNames
    resampled[[i]] <- merge(mod$resample, mod$bestTune)
    resampled[[i]]$Training_Size <- length(in_mod)

    if(test_prop > 0) {
      if(!mod$control$classProbs) {
        test_preds <- extractPrediction(list(model = mod),
                                        testX = dat[-for_model, colnames(dat) != outcome, drop = FALSE],
                                        testY = dat[-for_model, outcome])
      } else {
        test_preds <- extractProb(list(model = mod),
                                  testX = dat[-for_model, colnames(dat) != outcome, drop = FALSE],
                                  testY = dat[-for_model, outcome])
      }
      test_perf <- mod$control$summaryFunction(test_preds, lev = mod$finalModel$obsLevels)
      test_perf <- as.data.frame(t(test_perf), stringsAsFactors = FALSE)
      test_perf$Training_Size <- length(in_mod)
      tested[[i]] <- test_perf
      try(rm(test_preds, test_perf), silent = TRUE)
    }

    if(!mod$control$classProbs) {
      app_preds <- extractPrediction(list(model = mod),
                                     testX = dat[in_mod, colnames(dat) != outcome, drop = FALSE],
                                     testY = dat[in_mod, outcome])
    } else {
      app_preds <- extractProb(list(model = mod),
                               testX = dat[in_mod, colnames(dat) != outcome, drop = FALSE],
                               testY = dat[in_mod, outcome])
    }
    app_perf <- mod$control$summaryFunction(app_preds, lev = mod$finalModel$obsLevels)
    app_perf <- as.data.frame(t(app_perf), stringsAsFactors = FALSE)
    app_perf$Training_Size <- length(in_mod)
    apparent[[i]] <- app_perf

    try(rm(mod, in_mod, app_preds, app_perf), silent = TRUE)
  }

  resampled <- do.call("rbind", resampled)
  resampled <- resampled[, c(perf_names, "Training_Size")]
  resampled$Data <- "Resampling"
  apparent <- do.call("rbind", apparent)
  apparent <- apparent[, c(perf_names, "Training_Size")]
  apparent$Data <- "Training"
  out <- rbind(resampled, apparent)
  if(test_prop > 0) {
    tested <- do.call("rbind", tested)
    tested <- tested[, c(perf_names, "Training_Size")]
    tested$Data <- "Testing"
    out <- rbind(out, tested)
  }
  out
}

