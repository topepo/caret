## Should respect the input class except when there is a conflict or always
## generate a data frame?

ppMethods <- c("BoxCox", "YeoJohnson", "expoTrans", "invHyperbolicSine",
               "center", "scale", "range",
               "knnImpute", "bagImpute", "medianImpute",
               "pca", "ica",
               "spatialSign",
               "ignore", "keep",
               "remove",
               "zv", "nzv", "conditionalX",
               "corr")

invHyperbolicSineFunc <- function(x) log(x+sqrt(x^2+1))

getRangeBounds <- function(pp) {
 if(!is.null(pp$rangeBounds)) {
   list(lower = pp$rangeBounds[1], upper = pp$rangeBounds[2])
 } else {
   list(lower = 0.0, upper = 1.0)
 }
}

#' Pre-Processing of Predictors
#'
#' Pre-processing transformation (centering, scaling etc.) can be estimated
#' from the training data and applied to any data set with the same variables.
#'
#' In all cases, transformations and operations are estimated using the data in
#' \code{x} and these operations are applied to new data using these values;
#' nothing is recomputed when using the \code{predict} function.
#'
#' The Box-Cox (\code{method = "BoxCox"}), Yeo-Johnson (\code{method =
#' "YeoJohnson"}), and exponential transformations (\code{method =
#' "expoTrans"}) have been "repurposed" here: they are being used to transform
#' the predictor variables. The Box-Cox transformation was developed for
#' transforming the response variable while another method, the Box-Tidwell
#' transformation, was created to estimate transformations of predictor data.
#' However, the Box-Cox method is simpler, more computationally efficient and
#' is equally effective for estimating power transformations. The Yeo-Johnson
#' transformation is similar to the Box-Cox model but can accommodate
#' predictors with zero and/or negative values (while the predictors values for
#' the Box-Cox transformation must be strictly positive). The exponential
#' transformation of Manly (1976) can also be used for positive or negative
#' data.
#'
#' \code{method = "center"} subtracts the mean of the predictor's data (again
#' from the data in \code{x}) from the predictor values while \code{method =
#' "scale"} divides by the standard deviation.
#'
#' The "range" transformation scales the data to be within \code{rangeBounds}. If new
#' samples have values larger or smaller than those in the training set, values
#' will be outside of this range.
#'
#' Predictors that are not numeric are ignored in the calculations (including
#' methods "zv`" and "nzv`").
#'
#' \code{method = "zv"} identifies numeric predictor columns with a single
#' value (i.e. having zero variance) and excludes them from further
#' calculations. Similarly, \code{method = "nzv"} does the same by applying
#' \code{\link{nearZeroVar}} exclude "near zero-variance" predictors. The options
#' \code{freqCut} and \code{uniqueCut} can be used to modify the filter.
#'
#' \code{method = "corr"} seeks to filter out highly correlated predictors. See
#' \code{\link{findCorrelation}}.
#'
#' For classification, \code{method = "conditionalX"} examines the distribution
#' of each predictor conditional on the outcome. If there is only one unique
#' value within any class, the predictor is excluded from further calculations
#' (see \code{\link{checkConditionalX}} for an example). When \code{outcome} is
#' not a factor, this calculation is not executed. This operation can be time
#' consuming when used within resampling via \code{\link{train}}.
#'
#' The operations are applied in this order: zero-variance filter, near-zero
#' variance filter, correlation filter, Box-Cox/Yeo-Johnson/exponential transformation, centering,
#' scaling, range, imputation, PCA, ICA then spatial sign. This is a departure
#' from versions of \pkg{caret} prior to version 4.76 (where imputation was
#' done first) and is not backwards compatible if bagging was used for
#' imputation.
#'
#' If PCA is requested but centering and scaling are not, the values will still
#' be centered and scaled. Similarly, when ICA is requested, the data are
#' automatically centered and scaled.
#'
#' k-nearest neighbor imputation is carried out by finding the k closest
#' samples (Euclidian distance) in the training set. Imputation via bagging
#' fits a bagged tree model for each predictor (as a function of all the
#' others). This method is simple, accurate and accepts missing values, but it
#' has much higher computational cost. Imputation via medians takes the median
#' of each predictor in the training set, and uses them to fill missing values.
#' This method is simple, fast, and accepts missing values, but treats each
#' predictor independently, and may be inaccurate.
#'
#' A warning is thrown if both PCA and ICA are requested. ICA, as implemented
#' by the \code{\link[fastICA]{fastICA}} package automatically does a PCA
#' decomposition prior to finding the ICA scores.
#'
#' The function will throw an error of any numeric variables in \code{x} has
#' less than two unique values unless either \code{method = "zv"} or
#' \code{method = "nzv"} are invoked.
#'
#' Non-numeric data will not be pre-processed and their values will be in the
#' data frame produced by the \code{predict} function. Note that when PCA or
#' ICA is used, the non-numeric columns may be in different positions when
#' predicted.
#'
#' @aliases preProcess preProcess.default predict.preProcess
#' @param x a matrix or data frame. Non-numeric predictors are allowed but will
#' be ignored.
#' @param method a character vector specifying the type of processing. Possible
#' values are "BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "range",
#' "knnImpute", "bagImpute", "medianImpute", "pca", "ica", "spatialSign", "corr", "zv",
#' "nzv", and "conditionalX" (see Details below)
#' @param          thresh a cutoff for the cumulative percent of variance to be retained
#' by PCA
#' @param pcaComp the specific number of PCA components to keep. If specified,
#' this over-rides \code{thresh}
#' @param na.remove a logical; should missing values be removed from the
#' calculations?
#' @param object an object of class \code{preProcess}
#' @param newdata a matrix or data frame of new data to be pre-processed
#' @param k the number of nearest neighbors from the training set to use for
#' imputation
#' @param knnSummary function to average the neighbor values per column during
#' imputation
#' @param outcome a numeric or factor vector for the training set outcomes.
#' This can be used to help estimate the Box-Cox transformation of the
#' predictor variables (see Details below)
#' @param fudge a tolerance value: Box-Cox transformation lambda values within
#' +/-fudge will be coerced to 0 and within 1+/-fudge will be coerced to 1.
#' @param numUnique how many unique values should \code{y} have to estimate the
#' Box-Cox transformation?
#' @param verbose a logical: prints a log as the computations proceed
#' @param freqCut the cutoff for the ratio of the most common value to the
#' second most common value. See \code{\link{nearZeroVar}}.
#' @param uniqueCut the cutoff for the percentage of distinct values out of
#' the number of total samples. See \code{\link{nearZeroVar}}.
#' @param cutoff a numeric value for the pair-wise absolute correlation cutoff.
#' See \code{\link{findCorrelation}}.
#' @param rangeBounds a two-element numeric vector specifying closed interval
#' for range transformation
#' @param \dots additional arguments to pass to \code{\link[fastICA]{fastICA}},
#' such as \code{n.comp}
#' @return \code{preProcess} results in a list with elements \item{call}{the
#' function call} \item{method}{a named list of operations and the variables
#' used for each } \item{dim}{the dimensions of \code{x}} \item{bc}{Box-Cox
#' transformation values, see \code{\link{BoxCoxTrans}}} \item{mean}{a vector
#' of means (if centering was requested)} \item{std}{a vector of standard
#' deviations (if scaling or PCA was requested)} \item{rotation}{a matrix of
#' eigenvectors if PCA was requested} \item{method}{the value of \code{method}}
#' \item{thresh}{the value of \code{thresh}} \item{ranges}{a matrix of min and
#' max values for each predictor when \code{method} includes "range" (and
#' \code{NULL} otherwise)} \item{numComp}{the number of principal components
#' required of capture the specified amount of variance} \item{ica}{contains
#' values for the \code{W} and \code{K} matrix of the decomposition}
#' \item{median}{a vector of medians (if median imputation was requested)}
#'
#' \code{predict.preProcess} will produce a data frame.
#' @author Max Kuhn, median imputation by Zachary Mayer
#' @seealso \code{\link{BoxCoxTrans}}, \code{\link{expoTrans}}
#' \code{\link[MASS]{boxcox}}, \code{\link[stats]{prcomp}},
#' \code{\link[fastICA]{fastICA}}, \code{\link{spatialSign}}
#' @references \url{http://topepo.github.io/caret/pre-processing.html}
#'
#' Kuhn and Johnson (2013), Applied Predictive Modeling, Springer, New York
#' (chapter 4)
#'
#' Kuhn (2008), Building predictive models in R using the caret
#' (\doi{10.18637/jss.v028.i05})
#'
#' Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations (with
#' discussion). Journal of the Royal Statistical Society B, 26, 211-252.
#'
#' Box, G. E. P. and Tidwell, P. W. (1962) Transformation of the independent
#' variables. Technometrics 4, 531-550.
#'
#' Manly, B. L. (1976) Exponential data transformations. The Statistician, 25,
#' 37 - 42.
#'
#' Yeo, I-K. and Johnson, R. (2000). A new family of power transformations to
#' improve normality or symmetry. Biometrika, 87, 954-959.
#' @keywords utilities
#' @examples
#'
#' data(BloodBrain)
#' # one variable has one unique value
#' \dontrun{
#' preProc <- preProcess(bbbDescr)
#'
#' preProc  <- preProcess(bbbDescr[1:100,-3])
#' training <- predict(preProc, bbbDescr[1:100,-3])
#' test     <- predict(preProc, bbbDescr[101:208,-3])
#' }
#'
#' @export preProcess
preProcess <- function(x, ...) UseMethod("preProcess")

#' @rdname preProcess
#' @importFrom stats complete.cases median sd prcomp
#' @export
preProcess.default <- function(x, method = c("center", "scale"),
                               thresh = 0.95,
                               pcaComp = NULL,
                               na.remove = TRUE,
                               k = 5,
                               knnSummary = mean,
                               outcome = NULL,
                               fudge = .2,
                               numUnique = 3,
                               verbose = FALSE,
                               freqCut = 95/5,
                               uniqueCut = 10,
                               cutoff = 0.9,
                               rangeBounds = c(0, 1),
                               ...) {
  if(!inherits(x, "matrix") & !inherits(x, "data.frame"))
    stop("Matrices or data frames are required for preprocessing", call. = FALSE)
  column_types <- get_types(x)
  tmp <- pre_process_options(method, column_types)
  method <- tmp$opts
  wildcards <- tmp$wildcards

  if (any(method == "corr") & !any(method == "zv")) {
    method <- unique(c(method, "zv"))
    if (verbose)
      cat("A zero-variance filter was added for the correlation filter\n")
  }

  ## the row.norm option in fastICA states: "logical value indicating whether rows
  ## of the data matrix X should be standardized beforehand." Basically, this means that
  ## we would center *and* scale before the ICA step, so let's adjust the "scale" method too
  if(any(names(method) == "ica")) {
    theDots <- list(...)
    row.norm <- if(is.null(list(...)$row.norm)) FALSE else list(...)$row.norm
  }

  ## check for zero-variance predictors
  if(any(names(method) == "zv")){
    if(is.data.frame(x)) {
      is_zv <- unlist(lapply(x[, !(colnames(x) %in% method$ignore), drop = FALSE], function(x)
        ifelse(na.remove, length(na.omit(unique(x))), length(unique(x))) <= 1))
    } else {
      is_zv <- apply(x[, !(colnames(x) %in% method$ignore), drop = FALSE], 2, function(x)
        ifelse(na.remove, length(na.omit(unique(x))), length(unique(x))) <= 1)
    }
    if(any(is_zv)) {
      removed <- names(is_zv)[is_zv]
      method <- lapply(method, function(x, vars) x[!(x %in% vars)], vars = removed)
      method$remove <- unique(c(method$remove, removed))
      if(verbose) cat(paste(" ", length(removed), "zero variance predictors were removed.\n"))
    }
    method$zv <- NULL
  }
  ## check for near-zero-variance predictors
  if(any(names(method) == "nzv")){
    is_nzv <- nearZeroVar(x[, !(colnames(x) %in% method$ignore), drop = FALSE],
                          freqCut = freqCut, uniqueCut = uniqueCut)
    if(length(is_nzv) > 0) {
      removed <- colnames(x[, !(colnames(x) %in% method$ignore), drop = FALSE])[is_nzv]
      method <- lapply(method, function(x, vars) x[!(x %in% vars)], vars = removed)
      method$remove <- unique(c(method$remove, removed))
      if(verbose) cat(paste(" ", length(removed), "near-zero variance predictors were removed.\n"))
    }
    method$nzv <- NULL
  }
  ##  check the distribution of the columns of x conditioned on the levels of y and
  ## identifies columns of x that are sparse within groups of y
  if(any(names(method) == "conditionalX") & is.factor(outcome)){
    bad_pred <- checkConditionalX(x = x[, !(colnames(x) %in% method$ignore), drop = FALSE],
                                  y = outcome)
    if(length(bad_pred) > 0) {
      removed <- colnames(x[, !(colnames(x) %in% method$ignore), drop = FALSE])[bad_pred]
      method <- lapply(method, function(x, vars) x[!(x %in% vars)], vars = removed)
      method$remove <- unique(c(method$remove, removed))
      if(verbose) cat(paste(" ", length(removed), "conditionally zero variance predictors.\n"))
    }
    method$conditionalX <- NULL
  }

  ## check for highly correlated predictors
  if(any(names(method) == "corr")){
    cmat <- try(cor(x[, !(colnames(x) %in% c(method$ignore, method$remove)), drop = FALSE],
                    use = "pairwise.complete.obs"),
                silent = TRUE)
    if(!inherits(cmat, "try-error")) {
      high_corr <- findCorrelation(cmat, cutoff = cutoff)
      if(length(high_corr) > 0) {
        removed <- colnames(cmat)[high_corr]
        method$remove <- unique(c(method$remove, removed))
        if(verbose) cat(paste(" ", length(removed), "highly correlated predictors were removed.\n"))
      }
    } else warning(paste("correlation matrix could not be computed:\n", cmat))
    method$corr <- NULL
  }

  x <- x[, !(colnames(x) %in% method$remove), drop = FALSE]
  method = sapply(names(method), function(u)
    if(u != 'remove'){
      method[[u]][ which(( method[[u]] %in% colnames(x)))]
    } else {
      method[[u]]
    }, simplify = FALSE
  )

  if(any(names(method) == "invHyperbolicSine")) {
    if(verbose) cat(" applying invHyperbolicSine\n")
    for(i in method$invHyperbolicSine) x[,i] <- invHyperbolicSineFunc(x[,i])
  }

  if(any(names(method) == "BoxCox")) {
    bc <- group_bc(x[, method$BoxCox, drop = FALSE],
                   fudge = fudge,
                   na.remove = na.remove,
                   numUnique = numUnique,
                   verbose = verbose)
    if(verbose) cat(" applying them to training data\n")
    if(length(bc) != length(method$BoxCox)) {
      method$BoxCox <- if(length(bc) == 0) NULL else names(bc)
    }
    for(i in method$BoxCox) x[,i] <- predict(bc[[i]], x[,i])
  } else bc <- NULL


  if(any(names(method) == "YeoJohnson")) {
    yj <- vapply(
      x[, method$YeoJohnson, drop = FALSE],
      recipes::estimate_yj,
      c(lambda = 0),
      limits = c(-3, 3), ## consistent with `car` defaults
      num_unique = numUnique
    )
    yj <- yj[!is.null(yj) & !is.na(yj)]
    if(length(yj) > 0) {
      if(verbose) cat(" applying them to training data\n")
      if(length(yj) != length(method$YeoJohnson)) {
        method$YeoJohnson <- names(yj)
      }
      # now apply to current data
      if(length(yj) > 0) {
        for(i in seq(along = yj)) {
          who <- names(yj)[i]
          x[,who] <- recipes::yj_transform(x[,who], yj[who])
        }
      }
    } else {
      if(verbose) cat(" all of the transformations failed\n")
      yj <- NULL
    }
  } else yj <- NULL

  if(any(names(method) == "expoTrans")) {
    if(verbose)
      cat("Estimating exponential transformations for",
          length(method$expoTrans), "predictors...")
    if(is.data.frame(x)) {
      et <- lapply(x[, method$expoTrans, drop = FALSE],
                   expoTrans.default, numUnique = numUnique)
    } else {
      et <- apply(x[, method$expoTrans, drop = FALSE], 2,
                  expoTrans.default, numUnique = numUnique)
    }
    if(verbose) cat(" applying them to training data\n")
    omit_expo <- NULL
    for(i in names(et)) {
      tmp_trans <- predict(et[[i]], x[,i])
      if(any(is.infinite(tmp_trans))) {
        omit_expo <- c(omit_expo, i)
      } else x[,i] <- tmp_trans
    }
    if(length(omit_expo) > 0) {
      warning(paste("Expo. transform induced infinite values",
                    "in several predictors and is ommitted:",
                    paste(omit_expo, sep = "", collapse = ", ")),
              immediate. = TRUE)
      et <- et[!(names(et) %in% omit_expo)]
    }
  } else et <- NULL
  if(any(names(method)  %in% c("center"))) {
    if(verbose) cat("Calculating", length(method$center), "means for centering\n")
    centerValue <- apply(x[, method$center, drop = FALSE], 2, mean, na.rm = na.remove)
    x[, method$center] <- sweep(x[, method$center, drop = FALSE], 2, centerValue, "-")
  } else centerValue <- NULL

  if(any(names(method) %in% c("scale"))) {
    if(verbose) cat("Calculating", length(method$scale), "standard deviations for scaling\n")
    scaleValue <- apply(x[, method$scale, drop = FALSE], 2, sd, na.rm = na.remove)
    if(any(is.na(scaleValue))){
      wrn <- paste("Std. deviations could not be computed for:",
                   paste(names(scaleValue)[which(is.na(scaleValue))],collapse = ", "))
      warning(wrn, immediate. = TRUE)
      scaleValue[which(is.na(scaleValue))] <- 1
    }

    if(any(scaleValue == 0)){
      wrn <- paste("These variables have zero variances:",
                   paste(names(scaleValue)[which(scaleValue == 0)], collapse = ", "))
      warning(wrn, immediate. = TRUE)
      scaleValue[which(scaleValue == 0)] <- 1
    }
    x[, method$scale] <- sweep(x[, method$scale, drop = FALSE], 2, scaleValue, "/")
  } else scaleValue <- NULL

  if(any(names(method) == "range")) {
    if(verbose) cat("Calculating", length(method$range), "statistcs for scaling to a range\n")
    ## check rangeBounds consistency
    if(!is.numeric(rangeBounds) || length(rangeBounds) != 2)
      stop("'rangeBounds' should be a two-element numeric vector")
    if(rangeBounds[1] >= rangeBounds[2])
      stop("'rangeBounds' interval is empty")

    ranges <- apply(x[, method$range, drop = FALSE],
                    2,
                    function(x) c(min(x, na.rm = na.remove),
                                  max(x, na.rm = na.remove)))
    ## check for zero variance
    is_same <- apply(ranges, 2, function(x) x[1] == x[2])
    if(any(is_same)) {
      wrn <- paste("No variation for for:",paste(names(is_same)[is_same],collapse = ", "))
      warning(wrn,immediate. = TRUE)
      ranges <- ranges[, !is_same, drop = FALSE]
      method$range <- method$range[!(method$range %in% names(is_same)[is_same])]
    }
    x[, method$range] <- sweep(x[, method$range, drop = FALSE], 2, ranges[1,], "-")
    x[, method$range] <- sweep(x[, method$range, drop = FALSE], 2,
                               (ranges[2,] - ranges[1,]) / (rangeBounds[2] - rangeBounds[1]), "/")
    x[, method$range] <- sweep(x[, method$range, drop = FALSE], 2, rangeBounds[1], "+")
  } else ranges <- NULL

  if(any(names(method) == "bagImpute")){
    if(verbose) cat("Computing bagging models for", length(method$bagImpute), "predictors...")
    bagModels <- as.list(method$bagImpute)
    names(bagModels) <- method$bagImpute
    bagModels <- lapply(bagModels, bagImp, x = x)
    if(verbose) cat(" done\n")
  } else bagModels <- NULL

  if (any(names(method) == "medianImpute")) {
    if(verbose)
      cat("Computing medians for",
          length(method$medianImpute), "predictors...")
    medianValue <- apply(x[, method$medianImpute, drop = FALSE],
                         2, median, na.rm=TRUE)

    if (any(is.na(medianValue))) {
      warning(
        paste(
          "These variables are never filled:",
          paste(names(medianValue)[is.na(medianValue)], collapse = ", ")),
        immediate. = TRUE)
      medianValue[is.na(medianValue)] <- 0
    }
    if(verbose) cat(" done\n")
  } else medianValue <- NULL

  ## TODO This should be moved inside of pca or ica code
  ## and done after centering and scaling
  x <- x[complete.cases(x),,drop = FALSE]

  if(any(names(method) == "pca")) {
    if(verbose)
      cat("Computing PCA loadings for",
          length(method$pca),
          "predictors\n")
    tmp <- prcomp(x[, method$pca, drop = FALSE], scale = TRUE, retx = FALSE)
    if(is.null(pcaComp)) {
      cumVar <- cumsum(tmp$sdev^2/sum(tmp$sdev^2))
      aboveThresh <- cumVar >= thresh
      if (!any(aboveThresh)) {
        numComp <- length(aboveThresh)
      } else {
        numComp <- max(2, which.max(aboveThresh))
      }
    } else numComp <- min(pcaComp, ncol(tmp$rotation))
    rot <- tmp$rotation[,1:numComp]
  } else {
    rot <- NULL
    numComp <- NULL
  }

  if(any(names(method) == "ica")) {
    ## TODO What if range is used?
    if(verbose)
      cat("Computing ICA loadings for",
          length(method$ica),
          "predictors\n")
    requireNamespaceQuietStop("fastICA")
    tmp <- fastICA::fastICA(x[, method$ica, drop = FALSE], ...)
    ica <- list(row.norm = row.norm,
                K = tmp$K,
                W = tmp$W)
  } else {
    ica <- NULL
  }

  out <- list(dim = c(nrow(x), length(unique(unlist(method)))),
              bc = bc,
              yj = yj,
              et = et,
              invHyperbolicSine = method$invHyperbolicSine,
              mean = centerValue,
              std = scaleValue,
              ranges = ranges,
              rotation = rot,
              method = method,
              thresh = thresh,
              pcaComp = pcaComp,
              numComp = numComp,
              ica = ica,
              wildcards = wildcards,
              k = k,
              knnSummary = knnSummary,
              bagImp = bagModels,
              median = medianValue,
              data = if(any(names(method) == "knnImpute"))
                x[complete.cases(x),method$knnImpute,drop = FALSE] else NULL,
              rangeBounds = rangeBounds)
  out <- structure(out, class = "preProcess")
  out
}

#' @rdname preProcess
#' @importFrom stats complete.cases
#' @export
predict.preProcess <- function(object, newdata, ...) {
  if(is.vector(object$method) & !is.list(object$method))
    object <- convert_method(object)

  dataNames <- colnames(newdata)
  oldClass <- class(newdata)

  if(!is.null(object$method$remove)) {
    if(length(object$method$remove) > 0)
      newdata <- newdata[, !(colnames(newdata) %in% object$method$remove), drop = FALSE]
    if(ncol(newdata) == 0)
      stop("All predctors were removed as determined by `preProcess`")
  }

  if(!is.null(object$invHyperbolicSine)) {
    for(i in object$invHyperbolicSine) {
      newdata[,i] <- invHyperbolicSineFunc(newdata[,i])
    }
  }

  if(!is.null(object$bc)) {
    lam <- unlist(lapply(object$bc, function(x) x$lambda))
    lamIndex <- which(!is.na(lam))
    if(length(lamIndex) > 0) {
      for(i in names(lamIndex)) {
        tt <- newdata[,i]
        tt <- tt[!is.na(tt)]
        newdata[,i] <- predict(object$bc[[i]], newdata[,i])
      }
    }
  }

  if(!is.null(object$yj)) {
    lam <- get_yj_lambda(object$yj)
    lam <- lam[!is.na(lam)]
    if(length(lam) > 0) {
      for(i in seq(along = lam)) {
        who <- names(lam)[i]
        newdata[,who] <- recipes::yj_transform(newdata[,who], lam[who])
      }
    }
  }

  if(!is.null(object$et)) {
    for(i in seq(along = object$et)) {
      who <-  names(object$et)[i]
      newdata[,who] <- predict(object$et[[who]], newdata[,who])
    }
  }

  if(any(names(object$method) == "range")) {
    rangeBounds <- getRangeBounds(object)
    newdata[, object$method$range] <-
      sweep(newdata[, object$method$range, drop = FALSE], 2,
            object$ranges[1,], "-")
    newdata[, object$method$range] <-
      sweep(newdata[, object$method$range, drop = FALSE], 2,
            (object$ranges[2,] - object$ranges[1,]) /
              (rangeBounds$upper - rangeBounds$lower), "/")
    newdata[, object$method$range] <-
      sweep(newdata[, object$method$range, drop = FALSE], 2,
            rangeBounds$lower, "+")
  }

  if(any(names(object$method) == "center"))
    newdata[, object$method$center] <-
    sweep(newdata[, object$method$center, drop = FALSE], 2, object$mean, "-")
  if(any(names(object$method) %in% c("scale")))
    newdata[, object$method$scale] <-
    sweep(newdata[, object$method$scale, drop = FALSE], 2, object$std, "/")

  cc <- complete.cases(newdata)
  if(any(names(object$method) == "knnImpute") && any(!cc))  {
    hasMiss <- newdata[!cc,object$method$knnImpute,drop = FALSE]
    miss_names <- colnames(hasMiss)
    hasMiss <- apply(hasMiss,
                     1,
                     nnimp,
                     ## todo: should prob us all vars in the next line
                     old = object$data[, object$method$knnImpute, drop = FALSE],
                     k = object$k,
                     foo = object$knnSummary)
    hasMiss <- t(hasMiss)
    colnames(hasMiss) <- miss_names
    if(class(newdata)[1] == class(hasMiss)[1]) {
      newdata[!cc, object$method$knnImpute] <- hasMiss
    } else {
      if(is.data.frame(newdata)) {
        newdata[!cc, object$method$knnImpute] <- as.data.frame(hasMiss, stringsAsFactors = TRUE)
      } else newdata[!cc, object$method$knnImpute] <- as.matrix(hasMiss)
    }
  }

  if(any(names(object$method) == "bagImpute") && any(!cc)) {
    requireNamespaceQuietStop("ipred")
    hasMiss <- newdata[!cc,,drop = FALSE]
    missingVars <- apply(hasMiss,
                         2,
                         function(x) any(is.na(x)))
    missingVars <- names(missingVars)[missingVars]
    ## ipred's bagging procedure only allows for data frames
    if(!is.data.frame(hasMiss)) hasMiss <- as.data.frame(hasMiss, stringsAsFactors = TRUE)
    for(i in seq(along = missingVars)) {
      preds <- predict(object$bagImp[[missingVars[i]]]$model,
                       hasMiss[, !colnames(hasMiss) %in% missingVars[i], drop = FALSE])

      hasMiss[is.na(hasMiss[,missingVars[i]]),
              missingVars[i]] <- preds[is.na(hasMiss[,missingVars[i]])]
    }
    if(class(newdata)[1] == class(hasMiss)[1]) {
      newdata[!cc,] <- hasMiss
    } else {
      if(is.data.frame(newdata)) {
        newdata[!cc,] <- as.data.frame(hasMiss, stringsAsFactors = TRUE)
      } else newdata[!cc,] <- as.matrix(hasMiss)
    }
  }

  if (any(names(object$method) == "medianImpute") && any(!cc)) {
    missingVars <- apply(newdata, 2, function(x) any(is.na(x)))
    missingVars <- if(is.null(names(missingVars))) which(missingVars) else names(missingVars)[missingVars]
    for (v in missingVars) {
      newdata[is.na(newdata[, v]), v] <- object$median[v]
    }
  }

  if(any(names(object$method) == "pca")) {
    pca_cols <- newdata[, object$method$pca, drop = FALSE]
    pca_cols <-if(is.matrix(pca_cols)) pca_cols %*% object$rotation else as.matrix(pca_cols) %*% object$rotation
    if (ncol(pca_cols) == 1) {
      colnames(pca_cols) <- "PC1"
    }
    if(is.data.frame(newdata)) pca_cols <- as.data.frame(pca_cols, stringsAsFactors = TRUE)
    newdata <- cbind(newdata, pca_cols)
    ## normally we get rid of columns that we used to create
    ## the PC's unless we still need them or want them
    discard <- object$method$pca
    if(is.null(object$method$keep)) {
      ## after PCA/ICA comes SS so keep any of those
      discard <- discard[!(discard %in% object$method$spatialSign)]
    } else {
      discard <- discard[!(discard %in% object$method$keep)]
    }
    if(length(discard) > 0) newdata <- newdata[, !(colnames(newdata) %in% discard), drop = FALSE]
  }

  if(any(names(object$method) == "ica")) {
    ica_cols <- newdata[, object$method$ica, drop = FALSE]
    if(!is.matrix(ica_cols)) ica_cols <- as.matrix(ica_cols)
    ##if(object$ica$row.norm) newdata <- apply(newdata, 1, function(u) u/sd(u))
    ica_cols <- ica_cols %*% object$ica$K %*% object$ica$W
    colnames(ica_cols) <- paste("ICA", 1:ncol(object$ica$W), sep = "")
    if(is.data.frame(newdata)) ica_cols <- as.data.frame(ica_cols, stringsAsFactors = TRUE)
    newdata <- cbind(newdata, ica_cols)
    ## Same as PCA above
    discard <- object$method$ica
    if(is.null(object$method$keep)) {
      ## after PCA/ICA comes SS so keep any of those
      discard <- discard[!(discard %in% object$method$spatialSign)]
    } else {
      discard <- discard[!(discard %in% object$method$keep)]
    }
    if(length(discard) > 0) newdata <- newdata[, !(colnames(newdata) %in% discard), drop = FALSE]
  }

  wc <- object$wildcards
  if(any(names(object$method) == "spatialSign") |
     any(wc$PCA == "spatialSign") |
     any(wc$ICA == "spatialSign")){
    ss_col_names <- object$method$spatialSign
    ## adjust for PCA/ICA column wildcards
    if(length(wc$PCA) > 0 && any(wc$PCA == "spatialSign"))
      ss_col_names <- c(ss_col_names, colnames(pca_cols))
    if(length(wc$ICA) > 0 && any(wc$ICA == "spatialSign"))
      ss_col_names <- c(ss_col_names, colnames(ica_cols))

    newdata[, ss_col_names] <- spatialSign(newdata[, ss_col_names, drop = FALSE])
  }

  newdata
}

#' @export
print.preProcess <- function(x, ...) {
  #   printCall(x$call)
  cat("Created from", x$dim[1], "samples and", x$dim[2], "variables\n\n")


  pp_num <- unlist(lapply(x$method, length))
  if(any(unlist(x$wildcards) == "spatialSign")) {
    if(any(x$wildcards$PCA == "spatialSign"))
      pp_num["spatialSign"] <- pp_num["spatialSign"] + x$pcaComp
    if(any(x$wildcards$ICA == "spatialSign"))
      pp_num["spatialSign"] <- pp_num["spatialSign"] + x$numComp
  }
  pp <- paste0("  - ", names(x$method), " (", pp_num, ")\n")
  pp <- pp[order(pp)]
  pp <- gsub("invHyperbolicSine", "Inverve Hyperbolic Sine transformation", pp)
  pp <- gsub("BoxCox", "Box-Cox transformation", pp)
  pp <- gsub("YeoJohnson", "Yeo-Johnson transformation", pp)
  pp <- gsub("expoTrans", "exponential transformation", pp)
  pp <- gsub("scale", "scaled", pp)
  pp <- gsub("center", "centered", pp)
  pp <- gsub("pca", "principal component signal extraction", pp)
  pp <- gsub("ica", "independent component signal extraction", pp)
  pp <- gsub("spatialSign", "spatial sign transformation", pp)
  pp <- gsub("knnImpute", paste(x$k, "nearest neighbor imputation"), pp)
  pp <- gsub("bagImpute", "bagged tree imputation", pp)
  pp <- gsub("medianImpute", "median imputation", pp)

  rangeBounds <- getRangeBounds(x)
  pp <- gsub("range", paste0("re-scaling to [", rangeBounds$lower, ", ", rangeBounds$upper, "]"), pp)

  pp <- gsub("remove", "removed", pp)
  pp <- gsub("ignore", "ignored", pp)

  cat("Pre-processing:\n")
  cat(pp, sep = "")
  cat("\n")

  if(any(names(x$method) == "BoxCox")) {
    cat("Lambda estimates for Box-Cox transformation:\n")
    if(length(x$bc) < 11) {
      lmbda <- unlist(lapply(x$bc, function(x) x$lambda))
      naLmbda <- sum(is.na(lmbda))
      cat(paste(round(lmbda[!is.na(lmbda)], 2), collapse = ", "))
      if(naLmbda > 0) cat(" (#NA: ", naLmbda, ")\n", sep = "")
    } else print(summary(unlist(lapply(x$bc, function(x) x$lambda))))
    cat("\n")
  }

  if(any(names(x$method) == "YeoJohnson")) {
    cat("Lambda estimates for Yeo-Johnson transformation:\n")
    lmbda <- get_yj_lambda(x$yj)
    if(length(lmbda) < 11) {
      naLmbda <- sum(is.na(lmbda))
      cat(paste(round(lmbda[!is.na(lmbda)], 2), collapse = ", "))
      if(naLmbda > 0) cat(" (#NA: ", naLmbda, ")\n", sep = "")
    } else print(summary(lmbda))
    cat("\n")
  }

  if(any(names(x$method) == "pca")) {
    if(is.null(x$pcaComp)) {
      cat("PCA needed", x$numComp, ifelse(x$numComp > 1, "components", "component"),
          "to capture", round(x$thresh*100, 2),
          "percent of the variance")
    } else {
      cat("PCA used", x$pcaComp, ifelse(x$pcaComp > 1, "components", "component"), "as specified")
    }
    if(length(x$wildcards$PCA) > 0)
      cat(" and will be used in the spatial sign transformation")
    cat("\n")
  }

  if(any(names(x$method) == "ica")) {
    cat("ICA used", ncol(x$ica$W), "components")
    if(length(x$wildcards$ICA) > 0)
      cat(" and will be used in the spatial sign transformation")
    cat("\n")
  }
}

nnimp <- function(new, old, k, foo) {
  requireNamespaceQuietStop("RANN")
  if(all(is.na(new)))
    stop("cannot impute when all predictors are missing in the new data point")
  nms <- names(new)
  cols2 <- which(!is.na(new))
  new <- matrix(new, ncol = length(new))
  colnames(new) <- nms
  non_missing_cols <- cols2
  nn <- RANN::nn2(old[, non_missing_cols, drop = FALSE],
                  new[, non_missing_cols, drop = FALSE],
                  k = k)
  tmp <- old[nn$nn.idx, -non_missing_cols, drop = FALSE]
  subs <- apply(tmp, 2, foo, na.rm = TRUE)
  new[, -non_missing_cols] <- subs
  new
}

#' @importFrom stats as.formula
bagImp <- function(var, x, B = 10) {
  requireNamespaceQuietStop("ipred")
  ## The formula interface is much slower than the
  ## (y, X) interface, but the latter would have to
  ## do case-wise deletion of samples from the
  ## training set.
  if(!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
  mod <- ipred::bagging(as.formula(paste(var, "~.")),
                        data = x,
                        nbagg = B,
                        x = FALSE,
                        keepX = FALSE)
  trim_code <- getModelInfo("treebag", FALSE)[[1]]$trim
  list(var = var,
       model = trim_code(mod))
}


## Add checks for zv and nzv and overlap
## allow categorical variables in zv, nzv, and bagImpute

pre_process_options <- function(opts, vars) {
  orig_vars <- vars
  vars <- vars %in% c("integer", "numeric", "double")
  names(vars) <- names(orig_vars)
  ## convert simple vectors to list mode:
  if(is.vector(opts) & !is.list(opts)) {
    op_list <- vector(mode = "list", length = length(opts))
    names(op_list) <- opts
    op_list <- lapply(op_list,
                      function(x, y) {
                        x <- y
                        x
                      }, y = names(vars))
    opts <- op_list
  }

  ## check names of options
  if(!all(names(opts) %in% ppMethods)) {
    others <- names(opts)[!(names(opts) %in% ppMethods)]
    stop((paste("These pre-processing methods are unknown:",
                paste("'", others, "'", sep = "", collapse = ", "))))
  }

  methods <- names(opts)

  ## find and store any PCA/ICA wildcards
  tmp <- check_for_wildcards(opts, verbose = FALSE)
  opts <- tmp$opts
  wildcards <- tmp$wildcards

  ## check that each predictor is in the data
  all_op_vars <- unique(unlist(opts))
  if(!all(all_op_vars %in% names(vars))) {
    others <- all_op_vars[!(all_op_vars %in% names(vars))]
    stop((paste("These fields are not in the data:",
                paste("'", others, "'", sep = "", collapse = ", "))))
  } ## get fancy and look for dummy variables and write sensible note?

  ## check to make sure calcs are on numbers
  num_vars <- names(vars)[vars]
  not_num <- NULL
  for(i in ppMethods) {
    if(i %in% methods) {
      is_num <- opts[[i]] %in% num_vars
      if(any(!is_num)){
        not_num <- c(not_num, opts[[i]][!is_num])
        opts[[i]] <- opts[[i]][is_num]
      }
    }
  }
  not_num <- unique(not_num)
  if(length(not_num) > 0) {
    opts$ignore <- unique(c(opts$ignore, not_num))
  }

  ## check for group trans on single predictors
  if("pca" %in% methods && length(opts[["pca"]]) == 1){
    warning(paste("PCA is a group transformation and only a single predictor",
                  "is listed. This method is eliminated."),
            immediate. = TRUE)
    opts[["pca"]] <- NULL
  }


  if("ica" %in% methods && length(opts[["ica"]]) == 1){
    warning(paste("ICA is a group transformation and only a single predictor",
                  "is listed. This method is eliminated."),
            immediate. = TRUE)
    opts[["ica"]] <- NULL
  }

  if(all(unlist(wildcards) != "spatialSign") &
     "spatialSign" %in% methods &
     length(opts[["spatialSign"]]) == 1 ){
    warning(paste("Spatial sign is a group transformation and only a single predictor",
                  "is listed. This method is eliminated."),
            immediate. = TRUE)
    opts[["spatialSign"]] <- NULL
  }

  methods <- names(opts)
  ## check for inconsistent options for each predictor
  if(all(c("pca", "ica") %in% methods)){
    pca_and_ica <- intersect(opts$pca, opts$ica)
    if(length(pca_and_ica) > 0) {
      warning(paste("fastICA automatically uncorrelates the data using PCA.",
                    "method = 'pca' is not needed for fields:",
                    paste("'", pca_and_ica, "'", sep = "", collapse = ", ")),
              immediate. = TRUE)
      opts[["pca"]] <- opts[["pca"]][!(opts[["pca"]] %in% pca_and_ica)]
    }
  }
  if(sum(c("knnImpute","bagImpute", "medianImpute") %in% methods) > 1) {
    imp_table <- table(unlist(opts[c("knnImpute","bagImpute", "medianImpute")]))
    if(any(imp_table > 1)) {
      dup_imps <- names(imp_table)[imp_table > 1]
      stop((paste("Please pick a single imputation method for:",
                  paste("'", dup_imps, "'", sep = "", collapse = ", "))))
    }
  }

  if(any(methods %in% "range") & any(methods %in% c("center", "scale", "BoxCox")))
    stop("Centering, scaling and/or Box-Cox transformations are inconsistent with scaling to a range")

  ## coerce certain options based on others
  if("pca" %in% methods) {
    if("range" %in% methods) {
      opts[["range"]] <- c(opts[["range"]], opts[["pca"]])
      opts[["range"]] <- c(opts[["range"]], opts[["pca"]])
    } else {
      opts[["center"]] <- c(opts[["center"]], opts[["pca"]])
      opts[["scale"]] <- c(opts[["scale"]], opts[["pca"]])
    }
  }
  if("ica" %in% methods) {
    if("range" %in% methods) {
      opts[["range"]] <- c(opts[["range"]], opts[["ica"]])
      opts[["range"]] <- c(opts[["range"]], opts[["ica"]])
    } else {
      opts[["center"]] <- c(opts[["center"]], opts[["ica"]])
      opts[["scale"]] <- c(opts[["scale"]], opts[["ica"]])
    }
  }
  if("spatialSign" %in% methods) {
    if("range" %in% methods) {
      opts[["range"]] <- c(opts[["range"]], opts[["spatialSign"]])
      opts[["range"]] <- c(opts[["range"]], opts[["spatialSign"]])
    } else {
      opts[["center"]] <- c(opts[["center"]], opts[["spatialSign"]])
      opts[["scale"]] <- c(opts[["scale"]], opts[["spatialSign"]])
    }
  }
  if("knnImpute" %in% methods) {
    if("range" %in% methods) {
      opts[["range"]] <- num_vars
    } else {
      opts[["center"]] <- num_vars
      opts[["scale"]] <- num_vars
    }
  }
  opts <- lapply(opts, unique)

  ## check length of options and remove zero lengths
  opt_len <- unlist(lapply(opts, length))
  if(opt_len["spatialSign"] == 0 & any(unlist(wildcards) == "spatialSign"))
    opt_len["spatialSign"] <- 1
  if(any(opt_len < 1)) {
    warning(paste("The following pre-processing methods were eliminated:",
                  paste("'", names(opts)[opt_len < 1], "'", sep = "", collapse = ", ")),
            immediate. = TRUE)
    opts <- opts[opt_len > 0]
  }

  ## add to 'ignore'
  not_num_vars <- names(vars)[!vars]
  if("ignore" %in% names(opts))
    opts$ignore <- unique(c(not_num_vars, opts$ignore)) else
      opts$ignore <- not_num_vars
  ## TODO make sure that, if a var is in 'ignore' that it is nowhere else (and remove?)

  list(opts = opts, wildcards = wildcards)
}


get_types <- function(x, coarse = TRUE) {
  if(is.null(colnames(x)))
    stop("`x` must have column names")
  if(is.matrix(x)) {
    out <- rep(class(x[1,1]), ncol(x))
  } else {
    if(is.data.frame(x)) {
      out <- unlist(lapply(x, function(x) class(x)[1]))
    }
  }

  if(coarse) {
    num_classes <- c("integer", "numeric", "double")
    str_classes <- c("factor", "character")
    out <- ifelse(out %in% num_classes, "numeric", out)
    out <- ifelse(out %in% str_classes, "string", out)
    out <- ifelse(out %in% c("numeric", "string"), out, "other")
  }
  names(out) <- colnames(x)
  out
}

check_for_wildcards <- function(opts, verbose = TRUE){
  other_methods <- ppMethods[!(ppMethods %in% "spatialSign")]
  pc_wc <- unlist(lapply(opts, function(x) any(x == "_PC_")))
  if(any(pc_wc)) {
    pc_wc <- names(pc_wc)[pc_wc]
    if(verbose) cat("PCA wildcards found for:",
                    paste(pc_wc, sep = "", collapse = ", "))
    if(any(pc_wc %in% other_methods)) {
      bad_ops <- pc_wc[pc_wc %in% other_methods]
      if(verbose) cat(" ...but should not be in methods:",
                      paste(bad_ops, sep = "", collapse = ", "))
      for(i in bad_ops) opts[[i]] <- opts[[i]][opts[[i]] != "_PC_"]
    }
    if(verbose) cat("\n")
  }
  ic_wc <- unlist(lapply(opts, function(x) any(x == "_IC_")))
  if(any(ic_wc)) {
    ic_wc <- names(ic_wc)[ic_wc]
    if(verbose) cat("ICA wildcards found for:",
                    paste(ic_wc, sep = "", collapse = ", "), "\n")
    if(any(ic_wc %in% other_methods)) {
      bad_ops <- ic_wc[ic_wc %in% other_methods]
      if(verbose) cat(" ...but should not be in methods:",
                      paste(bad_ops, sep = "", collapse = ", "))
      for(i in bad_ops) opts[[i]] <- opts[[i]][opts[[i]] != "_IC_"]
    }
    if(verbose) cat("\n")
  }

  pc_wc <- unlist(lapply(opts, function(x) any(x == "_PC_")))
  ic_wc <- unlist(lapply(opts, function(x) any(x == "_IC_")))
  wc_list <- list(PCA = names(pc_wc)[pc_wc],
                  ICA = names(ic_wc)[ic_wc])
  opts <- lapply(opts, function(x) x[!(x %in% c("_PC_", "_IC_"))])
  list(opts = opts, wildcards = wc_list)
}


group_bc <- function(x, outcome = NULL,
                     fudge,
                     na.remove,
                     numUnique,
                     verbose) {
  if(verbose) cat("Estimating Box-Cox transformations for",  ncol(x), "predictors...\n")
  if(is.matrix(x)) {
    bc <- apply(x, 2, BoxCoxTrans,
                fudge = fudge,
                na.rm = na.remove,
                numUnique = numUnique)
  } else {
    bc <- lapply(x, BoxCoxTrans,
                 fudge = fudge,
                 na.rm = na.remove,
                 numUnique = numUnique)
  }
  lambdas <- unlist(lapply(bc, function(x) x$lambda))
  if(any(is.na(lambdas))) {
    bad_lambda <- lambdas[is.na(lambdas)]
    bad_lambda <- names(bad_lambda)
    if(verbose) cat("Box-Cox failed for:",  paste(bad_lambda, sep = "", collapse = ", "))
    bc <- bc[!(names(bc) %in% bad_lambda)]
  }

  bc[!is.null(bc) & !is.na(bc)]
}

convert_method <- function(x) {
  new_method <- list()
  if("center" %in% x$method)       new_method$center       <- names(x$mean)
  if("scale" %in% x$method)        new_method$scale        <- names(x$std)
  if("YeoJohnson" %in% x$method)   new_method$YeoJohnson   <- names(x$yj)
  if("expoTrans" %in% x$method)    new_method$expoTrans    <- names(x$et)
  if("BoxCox" %in% x$method)       new_method$BoxCox       <- names(x$bc)
  if("knnImpute" %in% x$method)    new_method$knnImpute    <- names(x$mean)
  if("bagImpute" %in% x$method)    new_method$bagImpute    <- names(x$bagImp)
  if("medianImpute" %in% x$method) new_method$medianImpute <- names(x$median)
  if("pca" %in% x$method)          new_method$pca          <- names(x$mean)
  if("ica" %in% x$method)          new_method$ica          <- names(x$mean)
  if("spatialSign" %in% x$method)  new_method$spatialSign  <- names(x$mean)
  if("invHyperbolicSine" %in% x$method)  new_method$invHyperbolicSine  <- x$method$invHyperbolicSine
  x$method <- new_method
  x
}

## code for using car method; extract lambdas either way and use
## new code for predictions. Same for predict method
get_yj_lambda <- function(x) {
  if(inherits(x[[1]], "powerTransform")) {
    # backwards compat with old caret objecgts that used `car`
    res <- unlist(lapply(x, function(x) x$lambda))
    names(res) <- gsub("\\.Y1$", "", names(res))
  } else {
    res <- x
  }
  res[!is.na(res)]
}

