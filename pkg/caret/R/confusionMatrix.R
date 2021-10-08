#' Create a confusion matrix
#'
#' Calculates a cross-tabulation of observed and predicted classes with
#' associated statistics.
#'
#' The functions requires that the factors have exactly the same levels.
#'
#' For two class problems, the sensitivity, specificity, positive predictive
#' value and negative predictive value is calculated using the \code{positive}
#' argument. Also, the prevalence of the "event" is computed from the data
#' (unless passed in as an argument), the detection rate (the rate of true
#' events also predicted to be events) and the detection prevalence (the
#' prevalence of predicted events).
#'
#' Suppose a 2x2 table with notation
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Event \tab No Event
#' \cr Event \tab A \tab B \cr No Event \tab C \tab D \cr }
#'
#' The formulas used here are: \deqn{Sensitivity = A/(A+C)} \deqn{Specificity =
#' D/(B+D)} \deqn{Prevalence = (A+C)/(A+B+C+D)} \deqn{PPV = (sensitivity *
#' prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))}
#' \deqn{NPV = (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) +
#' ((specificity)*(1-prevalence)))} \deqn{Detection Rate = A/(A+B+C+D)}
#' \deqn{Detection Prevalence = (A+B)/(A+B+C+D)} \deqn{Balanced Accuracy =
#' (sensitivity+specificity)/2}
#'
#' \deqn{Precision = A/(A+B)} \deqn{Recall = A/(A+C)} \deqn{F1 =
#' (1+beta^2)*precision*recall/((beta^2 * precision)+recall)}
#'
#' where \code{beta = 1} for this function.
#'
#' See the references for discussions of the first five formulas.
#'
#' For more than two classes, these results are calculated comparing each
#' factor level to the remaining levels (i.e. a "one versus all" approach).
#'
#' The overall accuracy and unweighted Kappa statistic are calculated. A
#' p-value from McNemar's test is also computed using
#' \code{\link[stats]{mcnemar.test}} (which can produce \code{NA} values with
#' sparse tables).
#'
#' The overall accuracy rate is computed along with a 95 percent confidence
#' interval for this rate (using \code{\link[stats]{binom.test}}) and a
#' one-sided test to see if the accuracy is better than the "no information
#' rate," which is taken to be the largest class percentage in the data.
#'
#' @aliases confusionMatrix.table confusionMatrix.default confusionMatrix
#' @param data a factor of predicted classes (for the default method) or an
#' object of class \code{\link[base]{table}}.
#' @param reference a factor of classes to be used as the true results
#' @param positive an optional character string for the factor level that
#' corresponds to a "positive" result (if that makes sense for your data). If
#' there are only two factor levels, the first level will be used as the
#' "positive" result. When \code{mode = "prec_recall"}, \code{positive} is the
#' same value used for \code{relevant} for functions \code{\link{precision}},
#' \code{\link{recall}}, and \code{\link{F_meas.table}}.
#' @param dnn a character vector of dimnames for the table
#' @param prevalence a numeric value or matrix for the rate of the "positive"
#' class of the data. When \code{data} has two levels, \code{prevalence} should
#' be a single numeric value. Otherwise, it should be a vector of numeric
#' values with elements for each class. The vector should have names
#' corresponding to the classes.
#' @param mode a single character string either "sens_spec", "prec_recall", or
#' "everything"
#' @param \dots options to be passed to \code{table}. NOTE: do not include
#' \code{dnn} here
#' @return a list with elements \item{table}{the results of \code{table} on
#' \code{data} and \code{reference}} \item{positive}{the positive result level}
#' \item{overall}{a numeric vector with overall accuracy and Kappa statistic
#' values} \item{byClass}{the sensitivity, specificity, positive predictive
#' value, negative predictive value, precision, recall, F1, prevalence,
#' detection rate, detection prevalence and balanced accuracy for each class.
#' For two class systems, this is calculated once using the \code{positive}
#' argument}
#' @note If the reference and data factors have the same levels, but in the
#' incorrect order, the function will reorder them to the order of the data and
#' issue a warning.
#' @author Max Kuhn
#' @seealso \code{\link{as.table.confusionMatrix}},
#' \code{\link{as.matrix.confusionMatrix}}, \code{\link{sensitivity}},
#' \code{\link{specificity}}, \code{\link{posPredValue}},
#' \code{\link{negPredValue}}, \code{\link{print.confusionMatrix}},
#' \code{\link[stats]{binom.test}}
#' @references Kuhn, M. (2008), ``Building predictive models in R using the
#' caret package, '' \emph{Journal of Statistical Software},
#' (\doi{10.18637/jss.v028.i05}).
#'
#' Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 1: sensitivity and
#' specificity,'' \emph{British Medical Journal}, vol 308, 1552.
#'
#' Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 2: predictive values,''
#' \emph{British Medical Journal}, vol 309, 102.
#'
#' Velez, D.R., et. al. (2008) ``A balanced accuracy function for epistasis
#' modeling in imbalanced datasets using multifactor dimensionality
#' reduction.,'' \emph{Genetic Epidemiology}, vol 4, 306.
#' @keywords utilities
#' @examples
#'
#' ###################
#' ## 2 class example
#'
#' lvs <- c("normal", "abnormal")
#' truth <- factor(rep(lvs, times = c(86, 258)),
#'                 levels = rev(lvs))
#' pred <- factor(
#'                c(
#'                  rep(lvs, times = c(54, 32)),
#'                  rep(lvs, times = c(27, 231))),
#'                levels = rev(lvs))
#'
#' xtab <- table(pred, truth)
#'
#' confusionMatrix(xtab)
#' confusionMatrix(pred, truth)
#' confusionMatrix(xtab, prevalence = 0.25)
#'
#' ###################
#' ## 3 class example
#'
#' confusionMatrix(iris$Species, sample(iris$Species))
#'
#' newPrior <- c(.05, .8, .15)
#' names(newPrior) <- levels(iris$Species)
#'
#' confusionMatrix(iris$Species, sample(iris$Species))
#'
#'
#' @export confusionMatrix
confusionMatrix <-
  function(data, ...){
    UseMethod("confusionMatrix")
  }

#' @rdname confusionMatrix
#' @method confusionMatrix default
#' @importFrom utils getFromNamespace
#' @export
confusionMatrix.default <- function(data, reference,
                                    positive = NULL,
                                    dnn = c("Prediction", "Reference"),
                                    prevalence = NULL,
                                    mode = "sens_spec",
                                    ...) {
  if(!(mode %in% c("sens_spec", "prec_recall", "everything")))
    stop("`mode` should be either 'sens_spec', 'prec_recall', or 'everything'")
  if(!is.factor(data) | !is.factor(reference)) {
    stop("`data` and `reference` should be factors with the same levels.", call. = FALSE)
  }
  if(!is.character(positive) & !is.null(positive)) stop("positive argument must be character")

  if(length(levels(data)) > length(levels(reference)))
    stop("the data cannot have more levels than the reference")

  if(!any(levels(data) %in% levels(reference))){
    stop("The data must contain some levels that overlap the reference.")
  }

  if(!all(levels(data) %in% levels(reference))){
    badLevel <- levels(data)[!levels(data) %in% levels(reference)]
    if(sum(table(data)[badLevel]) > 0){
      stop("The data contain levels not found in the data.")
    } else{
      warning("The data contains levels not found in the data, but they are empty and will be dropped.")
      data <- factor(as.character(data))
    }
  }

  if(any(levels(reference) != levels(data))) {
    warning("Levels are not in the same order for reference and data. Refactoring data to match.")
    data <- as.character(data)
    data <- factor(data, levels = levels(reference))
  }
  classLevels <- levels(data)
  numLevels <- length(classLevels)
  if(numLevels < 2)
    stop("there must be at least 2 factors levels in the data")

  if(numLevels == 2 & is.null(positive))  positive <- levels(reference)[1]

  classTable <- table(data, reference, dnn = dnn, ...)

  getFromNamespace("confusionMatrix.table", "caret")(classTable, positive, prevalence = prevalence, mode = mode)
}

#' @rdname confusionMatrix
#' @method confusionMatrix matrix
#' @importFrom utils getFromNamespace
#' @export
confusionMatrix.matrix <- function(data,
                                   positive = NULL,
                                   prevalence = NULL,
                                   mode = "sens_spec",
                                   ...) {
  if (length(unique(dim(data))) != 1) {
    stop("matrix must have equal dimensions")
  }
  classTable <- as.table(data, ...)
  confusionMatrix(classTable, positive, prevalence = prevalence, mode = mode)
}

#' @rdname confusionMatrix
#' @importFrom stats binom.test mcnemar.test
#' @export
confusionMatrix.table <- function(data, positive = NULL,
                                  prevalence = NULL, mode = "sens_spec", ...){
  if(!(mode %in% c("sens_spec", "prec_recall", "everything")))
    stop("`mode` should be either 'sens_spec', 'prec_recall', or 'everything'")
  if(length(dim(data)) != 2) stop("the table must have two dimensions")
  if(!all.equal(nrow(data), ncol(data))) stop("the table must nrow = ncol")
  if(!isTRUE(all.equal(rownames(data), colnames(data)))) stop("the table must the same classes in the same order")
  if(!is.character(positive) & !is.null(positive)) stop("positive argument must be character")

  classLevels <- rownames(data)
  numLevels <- length(classLevels)
  if(numLevels < 2)
    stop("there must be at least 2 factors levels in the data")

  if(numLevels == 2 & is.null(positive))  positive <- rownames(data)[1]


  if(numLevels == 2 & !is.null(prevalence) && length(prevalence) != 1)
    stop("with two levels, one prevalence probability must be specified")

  if(numLevels > 2 & !is.null(prevalence) && length(prevalence) != numLevels)
    stop("the number of prevalence probability must be the same as the number of levels")

  if(numLevels > 2 & !is.null(prevalence) && is.null(names(prevalence)))
    stop("with >2 classes, the prevalence vector must have names")

  propCI <- function(x) {
    res <- try(binom.test(sum(diag(x)), sum(x))$conf.int, silent = TRUE)
    if(inherits(res, "try-error"))
      res <- rep(NA, 2)
    res
  }

  propTest <- function(x){
    res <- try(
      binom.test(sum(diag(x)),
                 sum(x),
                 p = max(apply(x, 2, sum)/sum(x)),
                 alternative = "greater"),
      silent = TRUE)
    res <- if(inherits(res, "try-error"))
      c("null.value.probability of success" = NA, p.value = NA)
    else
      res <- unlist(res[c("null.value", "p.value")])
    res
  }

  overall <- c(unlist(e1071::classAgreement(data))[c("diag", "kappa")],
               propCI(data),
               propTest(data),
               mcnemar.test(data)$p.value)

  names(overall) <- c("Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper", "AccuracyNull", "AccuracyPValue", "McnemarPValue")

  if(numLevels == 2) {
    if(is.null(prevalence)) prevalence <- sum(data[, positive])/sum(data)
    negative <- classLevels[!(classLevels %in% positive)]
    tableStats <- c(sensitivity.table(data, positive),
                    specificity.table(data, negative),
                    posPredValue.table(data, positive, prevalence = prevalence),
                    negPredValue.table(data, negative, prevalence = prevalence),
                    precision.table(data, relevant = positive),
                    recall.table(data, relevant = positive),
                    F_meas.table(data, relevant = positive),
                    prevalence,
                    sum(data[positive, positive])/sum(data),
                    sum(data[positive, ])/sum(data))
    names(tableStats) <- c("Sensitivity", "Specificity",
                           "Pos Pred Value", "Neg Pred Value",
                           "Precision", "Recall", "F1",
                           "Prevalence", "Detection Rate",
                           "Detection Prevalence")
    tableStats["Balanced Accuracy"] <- (tableStats["Sensitivity"]+tableStats["Specificity"])/2

  } else {

    tableStats <- matrix(NA, nrow = length(classLevels), ncol = 11)

    for(i in seq(along = classLevels)) {
      pos <- classLevels[i]
      neg <- classLevels[!(classLevels %in% classLevels[i])]
      prev <- if(is.null(prevalence)) sum(data[, pos])/sum(data) else prevalence[pos]
      tableStats[i,] <- c(sensitivity.table(data, pos),
                          specificity.table(data, neg),
                          posPredValue.table(data, pos, prevalence = prev),
                          negPredValue.table(data, neg, prevalence = prev),
                          precision.table(data, relevant = pos),
                          recall.table(data, relevant = pos),
                          F_meas.table(data, relevant = pos),
                          prev,
                          sum(data[pos, pos])/sum(data),
                          sum(data[pos, ])/sum(data), NA)
      tableStats[i,11] <- (tableStats[i,1] + tableStats[i,2])/2
    }
    rownames(tableStats) <- paste("Class:", classLevels)
    colnames(tableStats) <- c("Sensitivity", "Specificity",
                              "Pos Pred Value", "Neg Pred Value",
                              "Precision", "Recall", "F1",
                              "Prevalence", "Detection Rate",
                              "Detection Prevalence", "Balanced Accuracy")
  }

  structure(
    list(positive = positive,
         table = data,
         overall = overall,
         byClass = tableStats,
         mode = mode,
         dots = list(...)),
    class = "confusionMatrix")
}

#' Confusion matrix as a table
#' @name as.matrix.confusionMatrix
#' @aliases as.table.confusionMatrix
#' @description Conversion functions for class \code{confusionMatrix}
#'
#' @param x an object of class \code{\link{confusionMatrix}}
#' @param what data to convert to matrix. Either \code{"xtabs"}, \code{"overall"} or  \code{"classes"}
#' @param \dots not currently used
#'
#' @details For \code{as.table}, the cross-tabulations are saved. For \code{as.matrix}, the three object types are saved in matrix format.
#'
#' @return A matrix or table
#'
#' @author Max Kuhn
#'
#' @examples
#' ###################
#' ## 2 class example
#'
#' lvs <- c("normal", "abnormal")
#' truth <- factor(rep(lvs, times = c(86, 258)),
#'                 levels = rev(lvs))
#' pred <- factor(
#'                c(
#'                  rep(lvs, times = c(54, 32)),
#'                  rep(lvs, times = c(27, 231))),
#'                levels = rev(lvs))
#'
#' xtab <- table(pred, truth)
#'
#' results <- confusionMatrix(xtab)
#' as.table(results)
#' as.matrix(results)
#' as.matrix(results, what = "overall")
#' as.matrix(results, what = "classes")
#'
#' ###################
#' ## 3 class example
#'
#' xtab <- confusionMatrix(iris$Species, sample(iris$Species))
#' as.matrix(xtab)
#'
#' @keywords utilities
#'
#' @export
as.matrix.confusionMatrix <- function(x, what = "xtabs", ...){
  if(!(what %in% c("xtabs", "overall", "classes")))
    stop("what must be either xtabs, overall or classes")
  out <- switch(what,
                xtabs = matrix(as.vector(x$table),
                               nrow = length(colnames(x$table)),
                               dimnames = list(rownames(x$table), colnames(x$table))),
                overall = as.matrix(x$overall),
                classes = as.matrix(x$byClass))
  if(what == "classes"){
    if(length(colnames(x$table)) > 2){
      out <- t(out)
      colnames(out) <- gsub("Class: ", "", colnames(out), fixed = TRUE)
    }
  }
  out
}

sbf_resampledCM <- function(x) {
  lev <- x$obsLevels
  if("pred" %in% names(x) && !is.null(x$pred)) {
    resampledCM <- do.call("rbind", x$pred[names(x$pred) == "predictions"])
    resampledCM <- ddply(resampledCM, .(Resample), function(y) flatTable(pred  = y$pred, obs = y$obs))
  } else stop(paste("When there are 50+ classes, the function does not automatically pre-compute the",
                    "resampled confusion matrices. You can get them when the option",
                    "`saveDetails = TRUE`."))
  resampledCM
}
rfe_resampledCM <- function(x) {
  lev <- x$obsLevels
  if("resample" %in% names(x) &&
     !is.null(x$resample) &&
     sum(grepl("\\.cell[1-9]", names(x$resample))) > 3) {
    resampledCM <- subset(x$resample, Variables == x$optsize)
    resampledCM <- resampledCM[,grepl("\\.cell[1-9]", names(resampledCM))]
  } else {
    if(!is.null(x$pred)) {
      resampledCM <- ddply(x$pred, .(Resample), function(y) flatTable(pred  = y$pred, obs = y$obs))
    } else {
      if(length(lev) > 50)
        stop(paste("When there are 50+ classes, `the function does not automatically pre-compute the",
                   "resampled confusion matrices. You can get them when the object",
                   "has a `pred` element."))
    }
  }
  resampledCM
}
train_resampledCM <- function(x) {
  if(x$modelType == "Regression")
    stop("confusion matrices are only valid for classification models")

  lev <- levels(x)

  ## For problems with large numbers of classes, `train`, `rfe`, and `sbf` do not pre-compute the
  ## the resampled matrices. If the predictions have been saved, we can get them from there.

  if("resampledCM" %in% names(x) && !is.null(x$resampledCM)) {
    ## get only best tune
    names(x$bestTune) <- gsub("^\\.", "", names(x$bestTune))
    resampledCM <- merge(x$bestTune, x$resampledCM)
  } else {
    if(!is.null(x$pred)) {
      resampledCM <- ddply(merge(x$pred, x$bestTune), .(Resample), function(y) flatTable(pred  = y$pred, obs = y$obs))
    } else {
      if(length(lev) > 50)
        stop(paste("When there are 50+ classes, `train` does not automatically pre-compute the",
                   "resampled confusion matrices. You can get them from this function",
                   "using a value of `savePredictions` other than FALSE."))
    }
  }
  resampledCM
}


#' @export
as.table.confusionMatrix <- function(x, ...)  x$table



#' Estimate a Resampled Confusion Matrix
#'
#' Using a \code{\link{train}}, \code{\link{rfe}}, \code{\link{sbf}} object,
#' determine a confusion matrix based on the resampling procedure
#'
#' When \code{\link{train}} is used for tuning a model, it tracks the confusion
#' matrix cell entries for the hold-out samples. These can be aggregated and
#' used for diagnostic purposes. For \code{\link{train}}, the matrix is
#' estimated for the final model tuning parameters determined by
#' \code{\link{train}}. For \code{\link{rfe}}, the matrix is associated with
#' the optimal number of variables.
#'
#' There are several ways to show the table entries. Using \code{norm = "none"}
#' will show the aggregated counts of samples on each of the cells (across all
#' resamples). For \code{norm = "average"}, the average number of cell counts
#' across resamples is computed (this can help evaluate how many holdout
#' samples there were on average). The default is \code{norm = "overall"},
#' which is equivalento to \code{"average"} but in percentages.
#'
#' @aliases confusionMatrix.train confusionMatrix.rfe confusionMatrix.sbf
#' @param data An object of class \code{\link{train}}, \code{\link{rfe}},
#' \code{\link{sbf}} that did not use out-of-bag resampling or leave-one-out
#' cross-validation.
#' @param norm A character string indicating how the table entries should be
#' normalized. Valid values are "none", "overall" or "average".
#' @param dnn A character vector of dimnames for the table
#' @param \dots not used here
#' @return a list of class \code{confusionMatrix.train},
#' \code{confusionMatrix.rfe} or \code{confusionMatrix.sbf} with elements
#' \item{table}{the normalized matrix} \item{norm}{an echo fo the call}
#' \item{text}{a character string with details about the resampling procedure
#' (e.g. "Bootstrapped (25 reps) Confusion Matrix"}
#' @author Max Kuhn
#' @seealso \code{\link{confusionMatrix}}, \code{\link{train}},
#' \code{\link{rfe}}, \code{\link{sbf}}, \code{\link{trainControl}}
#' @keywords utilities
#' @export
#' @examples
#'
#'
#' data(iris)
#' TrainData <- iris[,1:4]
#' TrainClasses <- iris[,5]
#'
#' knnFit <- train(TrainData, TrainClasses,
#'                 method = "knn",
#'                 preProcess = c("center", "scale"),
#'                 tuneLength = 10,
#'                 trControl = trainControl(method = "cv"))
#' confusionMatrix(knnFit)
#' confusionMatrix(knnFit, "average")
#' confusionMatrix(knnFit, "none")
#'
#'
#' @export confusionMatrix.train
confusionMatrix.train <- function(data, norm = "overall", dnn = c("Prediction", "Reference"), ...){
  if(data$control$method %in% c("oob", "LOOCV", "none"))
    stop("cannot compute confusion matrices for leave-one-out, out-of-bag resampling, or no resampling")

  if (inherits(data, "train")) {
    if(data$modelType == "Regression")
      stop("confusion matrices are only valid for classification models")
    lev <- levels(data)
    ## For problems with large numbers of classes, `train`, `rfe`, and `sbf` do not pre-compute the
    ## the resampled matrices. If the predictions have been saved, we can get them from there.
    resampledCM <- train_resampledCM(data)
  } else {
    lev <- data$obsLevels
    if (inherits(data, "rfe")) resampledCM <- rfe_resampledCM(data)
    if (inherits(data, "sbf")) resampledCM <- sbf_resampledCM(data)
  }

  if(!is.null(data$control$index)) {
    resampleN <- unlist(lapply(data$control$index, length))
    numResamp <- length(resampleN)
    resampText <- resampName(data)
  } else {
    resampText <- ""
    numResamp <- 0
  }

  counts <- as.matrix(resampledCM[ , grep("^\\.?cell", colnames(resampledCM))])

  ## normalize?
  norm <- match.arg(norm, c("none", "overall", "average"))

  if(norm == "none") counts <- matrix(apply(counts, 2, sum), nrow = length(lev))
  else counts <- matrix(apply(counts, 2, mean), nrow = length(lev))

  if(norm == "overall") counts <- counts / sum(counts) * 100

  ## names
  rownames(counts) <- colnames(counts) <- lev
  names(dimnames(counts)) <- dnn

  ## out
  out <- list(table = as.table(counts),
              norm = norm,
              B = length(data$control$index),
              text = paste(resampText, "Confusion Matrix"))
  class(out) <- paste0("confusionMatrix.", class(data))
  out
}

#' @export
confusionMatrix.rfe <- confusionMatrix.train

#' @export
confusionMatrix.sbf <- confusionMatrix.train

#' @importFrom utils getFromNamespace
#' @method print confusionMatrix.train
#' @export
print.confusionMatrix.train <- function(x, digits = 1, ...){
  cat(x$text, "\n")
  normText <- switch(x$norm,
                     none = "\n(entries are un-normalized aggregated counts)\n",
                     average = "\n(entries are average cell counts across resamples)\n",
                     overall = "\n(entries are percentual average cell counts across resamples)\n",
                     "")
  cat(normText, "\n")
  if(x$norm == "none" & x$B == 1) {
    print(getFromNamespace("confusionMatrix.table", "caret")(x$table))
  } else {
    print(round(x$table, digits))

    out <- cbind("Accuracy (average)", ":", formatC(sum(diag(x$table) / sum(x$table))))

    dimnames(out) <- list(rep("", nrow(out)), rep("", ncol(out)))
    print(out, quote = FALSE)
    cat("\n")
  }
  invisible(x)
}

#' @method print confusionMatrix.rfe
#' @export
print.confusionMatrix.rfe <- print.confusionMatrix.train

#' @method print confusionMatrix.sbf
#' @export
print.confusionMatrix.sbf <- print.confusionMatrix.train

resampName <- function(x, numbers = TRUE){
  if(!("control" %in% names(x))) return("")
  if(numbers) {
    resampleN <- unlist(lapply(x$control$index, length))
    numResamp <- length(resampleN)
    out <- switch(tolower(x$control$method),
                  none = "None",
                  apparent = "Apparent",
                  custom = paste("Custom Resampling (", numResamp, " reps)", sep = ""),
                  timeslice = paste("Rolling Forecasting Origin Resampling (",
                                    x$control$horizon, " held-out with",
                                    ifelse(x$control$fixedWindow, " a ", " no "),
                                    "fixed window)", sep = ""),
                  oob = "Out of Bag Resampling",
                  boot =, optimism_boot =, boot_all =,
                  boot632 = paste("Bootstrapped (", numResamp, " reps)", sep = ""),
                  cv = paste("Cross-Validated (", x$control$number, " fold)", sep = ""),
                  repeatedcv = paste("Cross-Validated (", x$control$number, " fold, repeated ",
                                     x$control$repeats, " times)", sep = ""),
                  lgocv = paste("Repeated Train/Test Splits Estimated (", numResamp, " reps, ",
                                round(x$control$p*100, 1), "%)", sep = ""),
                  loocv = "Leave-One-Out Cross-Validation",
                  adaptive_boot = paste("Adaptively Bootstrapped (", numResamp, " reps)", sep = ""),
                  adaptive_cv = paste("Adaptively Cross-Validated (", x$control$number, " fold, repeated ",
                                      x$control$repeats, " times)", sep = ""),
                  adaptive_lgocv = paste("Adaptive Repeated Train/Test Splits Estimated (", numResamp, " reps, ",
                                         round(x$control$p, 2), "%)", sep = "")
    )
  } else {
    out <- switch(tolower(x$control$method),
                  none = "None",
                  apparent = "(Apparent)",
                  custom = "Custom Resampling",
                  timeslice = "Rolling Forecasting Origin Resampling",
                  oob = "Out of Bag Resampling",
                  boot = "(Bootstrap)",
                  optimism_boot = "(Optimism Bootstrap)",
                  boot_all = "(Bootstrap All)",
                  boot632 = "(Bootstrap 632 Rule)",
                  cv = "(Cross-Validation)",
                  repeatedcv = "(Repeated Cross-Validation)",
                  loocv = "Leave-One-Out Cross-Validation",
                  lgocv = "(Repeated Train/Test Splits)")

  }
  out
}


#' Print method for confusionMatrix
#'
#' a print method for \code{confusionMatrix}
#'
#'
#' @param x an object of class \code{confusionMatrix}
#' @param mode a single character string either "sens_spec", "prec_recall", or
#' "everything"
#' @param digits number of significant digits when printed
#' @param printStats a logical: if \code{TRUE} then table statistics are also
#' printed
#' @param \dots optional arguments to pass to \code{print.table}
#' @return \code{x} is invisibly returned
#' @author Max Kuhn
#' @seealso \code{\link{confusionMatrix}}
#' @keywords utilities
#' @export

print.confusionMatrix <- function(x, mode = x$mode, digits = max(3, getOption("digits") - 3), printStats = TRUE, ...){
  if(is.null(mode)) mode <- "sens_spec"
  if(!(mode %in% c("sens_spec", "prec_recall", "everything")))
    stop("`mode` should be either 'sens_spec', 'prec_recall', or 'everything'")
  cat("Confusion Matrix and Statistics\n\n")
  print(x$table, ...)

  if(printStats) {
    tmp <- round(x$overall, digits = digits)
    pIndex <- grep("PValue", names(x$overall))
    tmp[pIndex] <- format.pval(x$overall[pIndex], digits = digits)
    overall <- tmp

    accCI <- paste("(",
                   paste(
                     overall[ c("AccuracyLower", "AccuracyUpper")],
                     collapse = ", "),
                   ")",
                   sep = "")

    overallText <- c(paste(overall["Accuracy"]),
                     accCI,
                     paste(overall[c("AccuracyNull", "AccuracyPValue")]),
                     "",
                     paste(overall["Kappa"]),
                     "",
                     paste(overall["McnemarPValue"]))

    overallNames <- c("Accuracy", "95% CI",
                      "No Information Rate",
                      "P-Value [Acc > NIR]",
                      "",
                      "Kappa",
                      "",
                      "Mcnemar's Test P-Value")

    if(dim(x$table)[1] > 2){
      cat("\nOverall Statistics\n")
      overallNames <- ifelse(overallNames == "",
                             "",
                             paste(overallNames, ":"))
      out <- cbind(format(overallNames, justify = "right"), overallText)
      colnames(out) <- rep("", ncol(out))
      rownames(out) <- rep("", nrow(out))

      print(out, quote = FALSE)

      cat("\nStatistics by Class:\n\n")
      if(mode == "prec_recall")
        x$byClass <- x$byClass[,!grepl("(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)",
                                     colnames(x$byClass))]
      if(mode == "sens_spec")
        x$byClass <- x$byClass[,!grepl("(Precision)|(Recall)|(F1)", colnames(x$byClass))]
      print(t(x$byClass), digits = digits)

    } else {
      if(mode == "prec_recall")
        x$byClass <- x$byClass[!grepl("(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)",
                                       names(x$byClass))]
      if(mode == "sens_spec")
        x$byClass <- x$byClass[!grepl("(Precision)|(Recall)|(F1)", names(x$byClass))]

      overallText <- c(overallText,
                       "",
                       format(x$byClass, digits = digits))
      overallNames <- c(overallNames, "", names(x$byClass))
      overallNames <- ifelse(overallNames == "", "", paste(overallNames, ":"))

      overallNames <- c(overallNames, "", "'Positive' Class :")
      overallText <- c(overallText, "", x$positive)

      out <- cbind(format(overallNames, justify = "right"), overallText)
      colnames(out) <- rep("", ncol(out))
      rownames(out) <- rep("", nrow(out))

      out <- rbind(out, rep("", 2))

      print(out, quote = FALSE)
    }
  }
  invisible(x)
}
