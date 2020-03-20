#' Partial Least Squares and Sparse Partial Least Squares Discriminant Analysis
#'
#' \code{plsda} is used to fit standard PLS models for classification while
#' \code{splsda} performs sparse PLS that embeds feature selection and
#' regularization for the same purpose.
#'
#' If a factor is supplied, the appropriate indicator matrix is created.
#'
#' A multivariate PLS model is fit to the indicator matrix using the
#' \code{\link[pls:mvr]{plsr}} or \code{\link[spls]{spls}} function.
#'
#' Two prediction methods can be used.
#'
#' The \bold{softmax function} transforms the model predictions to
#' "probability-like" values (e.g. on [0, 1] and sum to 1). The class with the
#' largest class probability is the predicted class.
#'
#' Also, \bold{Bayes rule} can be applied to the model predictions to form
#' posterior probabilities. Here, the model predictions for the training set
#' are used along with the training set outcomes to create conditional
#' distributions for each class. When new samples are predicted, the raw model
#' predictions are run through these conditional distributions to produce a
#' posterior probability for each class (along with the prior). This process is
#' repeated \code{ncomp} times for every possible PLS model. The
#' \code{\link[klaR]{NaiveBayes}} function is used with \code{usekernel = TRUE}
#' for the posterior probability calculations.
#'
#' @aliases plsda.default predict.plsda plsda splsda.default predict.splsda
#' splsda
#' @param x a matrix or data frame of predictors
#' @param y a factor or indicator matrix for the discrete outcome. If a matrix,
#' the entries must be either 0 or 1 and rows must sum to one
#' @param ncomp the number of components to include in the model. Predictions
#' can be made for models with values less than \code{ncomp}.
#' @param probMethod either "softmax" or "Bayes" (see Details)
#' @param prior a vector or prior probabilities for the classes (only used for
#' \code{probeMethod = "Bayes"})
#' @param \dots arguments to pass to \code{\link[pls:mvr]{plsr}} or
#' \code{\link[spls]{spls}}. For \code{splsda}, this is the method for passing
#' tuning parameters specifications (e.g. \code{K}, \code{eta} or \code{kappa})
#' @param object an object produced by \code{plsda}
#' @param newdata a matrix or data frame of predictors
#' @param type either \code{"class"}, \code{"prob"} or \code{"raw"} to produce
#' the predicted class, class probabilities or the raw model scores,
#' respectively.
#' @return For \code{plsda}, an object of class "plsda" and "mvr". For
#' \code{splsda}, an object of class \code{splsda}.
#'
#' The predict methods produce either a vector, matrix or three-dimensional
#' array, depending on the values of \code{type} of \code{ncomp}. For example,
#' specifying more than one value of \code{ncomp} with \code{type = "class"}
#' with produce a three dimensional array but the default specification would
#' produce a factor vector.
#' @seealso \code{\link[pls:mvr]{plsr}}, \code{\link[spls]{spls}}
#' @keywords models
#' @examples
#'
#' \dontrun{
#' data(mdrr)
#' set.seed(1)
#' inTrain <- sample(seq(along = mdrrClass), 450)
#'
#' nzv <- nearZeroVar(mdrrDescr)
#' filteredDescr <- mdrrDescr[, -nzv]
#'
#' training <- filteredDescr[inTrain,]
#' test <- filteredDescr[-inTrain,]
#' trainMDRR <- mdrrClass[inTrain]
#' testMDRR <- mdrrClass[-inTrain]
#'
#' preProcValues <- preProcess(training)
#'
#' trainDescr <- predict(preProcValues, training)
#' testDescr <- predict(preProcValues, test)
#'
#' useBayes   <- plsda(trainDescr, trainMDRR, ncomp = 5,
#'                     probMethod = "Bayes")
#' useSoftmax <- plsda(trainDescr, trainMDRR, ncomp = 5)
#'
#' confusionMatrix(predict(useBayes, testDescr),
#'                 testMDRR)
#'
#' confusionMatrix(predict(useSoftmax, testDescr),
#'                 testMDRR)
#'
#' histogram(~predict(useBayes, testDescr, type = "prob")[,"Active",]
#'           | testMDRR, xlab = "Active Prob", xlim = c(-.1,1.1))
#' histogram(~predict(useSoftmax, testDescr, type = "prob")[,"Active",]
#'           | testMDRR, xlab = "Active Prob", xlim = c(-.1,1.1))
#'
#'
#' ## different sized objects are returned
#' length(predict(useBayes, testDescr))
#' dim(predict(useBayes, testDescr, ncomp = 1:3))
#' dim(predict(useBayes, testDescr, type = "prob"))
#' dim(predict(useBayes, testDescr, type = "prob", ncomp = 1:3))
#'
#' ## Using spls:
#' ## (As of 11/09, the spls package now has a similar function with
#' ## the same mane. To avoid conflicts, use caret:::splsda to
#' ## get this version)
#'
#' splsFit <- caret:::splsda(trainDescr, trainMDRR,
#'                           K = 5, eta = .9,
#'                           probMethod = "Bayes")
#'
#' confusionMatrix(caret:::predict.splsda(splsFit, testDescr),
#'                 testMDRR)
#' }
#'
#' @export plsda
plsda <- function (x, ...)
  UseMethod("plsda")

#' @rdname plsda
#' @importFrom stats predict
#' @export
predict.plsda <- function(object, newdata = NULL, ncomp = NULL, type = "class", ...){
  requireNamespaceQuietStop('pls')
  if(is.null(ncomp))
    if(!is.null(object$ncomp)) ncomp <- object$ncomp else stop("specify ncomp")

  if(!is.null(newdata)) {
    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
  }

  ## make sure that the prediction function from pls is used
  class(object) <- "mvr"
  tmpPred <- predict(object, newdata = newdata)[,,ncomp,drop = FALSE]

  if(type == "raw") return(tmpPred)

  if(is.null(object$probModel)) {
    ## use softmax
    switch(type,

           class = {
             if(length(dim(tmpPred)) < 3) {
               ##only requested one component
               out <- object$obsLevels[apply(tmpPred, 1, which.max)]
               out <- factor(out, levels = object$obsLevels)
             } else {
               ## more than one component
               tmpOut <- matrix("", nrow = dim(tmpPred)[1], ncol = dim(tmpPred)[3])
               for(i in 1:dim(tmpPred)[3]) {
                 tmpOut[,i] <- object$obsLevels[apply(tmpPred[,,i,drop=FALSE], 1, which.max)]
               }
               out <- as.data.frame(tmpOut, stringsAsFactors = TRUE)
               out <- as.data.frame(
                 lapply(out, function(x, y) factor(x, levels = y),
                        y = object$obsLevels),
                 stringsAsFactors = TRUE
               )
               names(out) <- paste("ncomp", ncomp, sep = "")
               rownames(out) <- rownames(newdata)
               if(length(ncomp) == 1) out <- out[,1]
             }
           },
           prob = {
             ## fix prob names
             if(length(dim(tmpPred)) < 3) {
               out <- t(apply(tmpPred, 1, function(data) exp(data)/sum(exp(data))))
             } else {
               ## more than one component
               out <- tmpPred * NA
               for(i in 1:dim(tmpPred)[3]) {
                 out[,,i] <- t(apply(tmpPred[,,i,drop=FALSE], 1, function(data) exp(data)/sum(exp(data))))
               }
             }
           })
  } else {
    ## Bayes rule

    requireNamespaceQuietStop("klaR")
    tmp <- vector(mode = "list", length = length(ncomp))
    for(i in seq(along = ncomp)) {
      tmp[[i]] <- predict(object$probModel[[ ncomp[i] ]],
                          as.data.frame(tmpPred[,-length(object$obsLevels),i]), stringsAsFactors = TRUE)
    }

    if(type == "class") {
      out <- t(do.call("rbind",
                       lapply(tmp, function(x) as.character(x$class))))
      rownames(out) <- names(tmp[[1]]$class)
      colnames(out) <- paste("ncomp", ncomp, sep = "")
      out <- as.data.frame(out, stringsAsFactors = TRUE)
      out <- as.data.frame(
        lapply(out, function(x, y) factor(x, levels = y),
               y = object$obsLevels),
        stringsAsFactors = TRUE
      )
      if(length(ncomp) == 1) out <- out[,1]
    } else {
      out <- array(dim = c(dim(tmp[[1]]$posterior), length(ncomp)),
                   dimnames = list(
                     rownames(tmp[[1]]$posterior),
                     colnames(tmp[[1]]$posterior),
                     paste("ncomp", ncomp, sep = "")))
      for(i in seq(along = ncomp)) out[,,i] <- tmp[[i]]$posterior
    }
  }
  out
}

#' @rdname plsda
#' @importFrom stats predict
#' @export
plsda.default <- function(x, y, ncomp = 2, probMethod = "softmax", prior = NULL, ...) {
  requireNamespaceQuietStop('pls')

  funcCall <- match.call(expand.dots = TRUE)

  if(!is.matrix(x)) x <- as.matrix(x)
  if(length(ncomp) > 1) {
    ncomp <- max(ncomp)
    warning(paste(
      "A value single ncomp must be specified.",
      "max(ncomp) was used.",
      "Predictions can be obtained for values <= ncomp"))
  }

  if(probMethod == "softmax") {
    if(!is.null(prior)) warning("Priors are ignored unless probMethod = \"Bayes\"")
  }


  if(is.factor(y)) {
    obsLevels <- levels(y)
    oldY <- y
    y <- class2ind(y)
  } else {
    if(is.matrix(y)) {
      test <- apply(y, 1, sum)
      if(any(test != 1)) stop("the rows of y must be 0/1 and sum to 1")
      obsLevels <- colnames(y)
      if(is.null(obsLevels)) stop("the y matrix must have column names")
      oldY <- obsLevels[apply(y, 1, which.max)]
    } else stop("y must be a matrix or a factor")
  }

  if(!is.matrix(x)) x <- as.matrix(x)

  tmpData <- data.frame(n = paste("row", 1:nrow(y), sep = ""))
  tmpData$y <- y
  tmpData$x <- x

  out <- pls::plsr(y ~ x, data = tmpData, ncomp = ncomp, ...)

  out$obsLevels <- obsLevels
  out$probMethod <- probMethod
  if(probMethod == "Bayes") {
    requireNamespaceQuietStop('klaR')
    makeModels <- function(x, y, pri) {
      probModel <- klaR::NaiveBayes(x, y, prior = pri, usekernel = TRUE)
      probModel$train <- predict(probModel)$posterior
      probModel$x <- NULL
      probModel
    }
    cls <- class(out)
    class(out) <- "mvr"
    train <- predict(out, as.matrix(tmpData$x), ncomp = 1:ncomp)
    ## Get the raw model predictions, but leave one behind since the
    ## final class probs sum to one
    train <- train[, -length(obsLevels),, drop = FALSE]

    out$probModel <- apply(train, 3, makeModels, y = oldY, pri = prior)
  } else out$probModel <- NULL

  ##out$call <- funcCall
  class(out) <- c("plsda", class(out))
  out
}

#' @export
print.plsda <- function (x, ...) {
  ## minor change to print.mvr
  switch(x$method,
         kernelpls = {
           regr = "Partial least squares"
           alg = "kernel"
         }, simpls = {
           regr = "Partial least squares"
           alg = "simpls"
         }, oscorespls = {
           regr = "Partial least squares"
           alg = "orthogonal scores"
         }, svdpc = {
           regr = "Principal component"
           alg = "singular value decomposition"
         }, stop("Unknown fit method."))
  cat(regr, "classification, fitted with the", alg, "algorithm.")
  if (!is.null(x$validation))
    cat("\nCross-validated using", length(x$validation$segments),
        attr(x$validation$segments, "type"), "segments.")

  switch(x$probMethod,
         softmax = cat("\nThe softmax function was used to compute class probabilities.\n"),
         Bayes = cat("\nBayes rule was used to compute class probabilities.\n"))
  invisible(x)
}
