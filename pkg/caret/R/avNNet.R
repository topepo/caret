#' @title Neural Networks Using Model Averaging
#' @name avNNet
#' @description Aggregate several neural network models
#'
#'
#' @param formula A formula of the form \code{class ~ x1 + x2 + \dots}
#' @param x matrix or data frame of \code{x} values for examples.
#' @param y matrix or data frame of target values for examples.
#' @param weights (case) weights for each example - if missing defaults to 1.
#' @param repeats the number of neural networks with different random number seeds
#' @param bag a logical for bagging for each repeat
#' @param seeds random number seeds that can be set prior to bagging (if done) and network creation. This helps maintain reproducibility when models are run in parallel.
#' @param allowParallel if a parallel backend is loaded and available, should the function use it?
#' @param data Data frame from which variables specified in  \code{formula} are preferentially to be taken.
#' @param subset An index vector specifying the cases to be used in the training sample.  (NOTE: If given, this argument must be named.)
#' @param na.action A function to specify the action to be taken if \code{NA}s are found.
#' The default action is for the procedure to fail.  An alternative is
#' \code{na.omit}, which leads to rejection of cases with missing values on
#' any required variable.  (NOTE: If given, this argument must be named.)
#' @param contrasts a list of contrasts to be used for some or all of
#' the  factors  appearing as variables in the model formula.
#' @param object an object of class \code{avNNet} as  returned by \code{avNNet}.
#' @param newdata matrix or data frame of test examples. A vector is considered to be
#' a row vector comprising a single case.
#' @param type Type of output, either: \code{raw} for the raw outputs, \code{code} for the predicted class or \code{prob} for the class probabilities.
#' @param \dots arguments passed to \code{\link[nnet]{nnet}}
#'
#' @details Following Ripley (1996), the same neural network model is fit using different random number seeds. All the resulting models are used for prediction. For regression, the output from each network are averaged. For classification, the model scores are first averaged, then translated to predicted classes. Bagging can also be used to create the models.
#'
#' If a parallel backend is registered, the \pkg{foreach} package is used to train the networks in parallel.
#'
#' @return
#'   For \code{avNNet}, an object of  \code{"avNNet"} or \code{"avNNet.formula"}. Items of interest in #' the output are:
#'   \item{model }{a list of the models generated from  \code{\link[nnet]{nnet}}}
#'   \item{repeats }{an echo of the model input}
#'   \item{names }{if any predictors had only one distinct value, this is a character string of the #' remaining columns. Otherwise a value of \code{NULL}}
#' @references   Ripley, B. D. (1996)
#' \emph{Pattern Recognition and Neural Networks.} Cambridge.
#'
#' @author These are heavily based on the \code{nnet} code from Brian Ripley.
#' @seealso \code{\link[nnet]{nnet}},  \code{\link{preProcess}}
#' @examples
#' data(BloodBrain)
#' \dontrun{
#' modelFit <- avNNet(bbbDescr, logBBB, size = 5, linout = TRUE, trace = FALSE)
#' modelFit
#'
#' predict(modelFit, bbbDescr)
#' }
#' @keywords neural
#' @aliases avNNet.default predict.avNNet avNNet.formula avNNet
#' @export
avNNet <- function (x, ...)
  UseMethod("avNNet")


## this is a near copy of nnet.formula
#' @importFrom stats .getXlevels contrasts model.matrix model.response model.weights
#' @rdname avNNet
#' @method avNNet formula
#' @export
avNNet.formula <- function (formula, data, weights, ...,
                            repeats = 5,
                            bag= FALSE,
                            allowParallel = TRUE,
                            seeds = sample.int(1e5, repeats),
                            subset, na.action, contrasts = NULL)
{
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval.parent(m$data)))
    m$data <- as.data.frame(data, stringsAsFactors = FALSE)
##  bag <- m$bag
##  repeats <- m$repeats
  m$... <- m$contrasts <- m$bag <- m$repeats <- m$allowParallel <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  xint <- match("(Intercept)", colnames(x), nomatch = 0)
  if (xint > 0)
    x <- x[, -xint, drop = FALSE]
  w <- model.weights(m)
  if (length(w) == 0)
    w <- rep(1, nrow(x))
  y <- model.response(m)

  res <- avNNet.default(x, y,
                        weights = w,
                        repeats = repeats,
                        bag = bag,
                        allowParallel = allowParallel,
                        seeds = seeds,
                        ...)
  res$terms <- Terms
  res$coefnames <- colnames(x)
  res$na.action <- attr(m, "na.action")
  res$contrasts <- cons
  res$xlevels <- .getXlevels(Terms, m)
  class(res) <- c("avNNet.formula", "avNNet")
  res
}

#' @import foreach
#' @rdname avNNet
#' @method avNNet default
#' @export
avNNet.default <- function(x, y, repeats = 5,
                           bag = FALSE, allowParallel = TRUE,
                           seeds = sample.int(1e5, repeats), ...)
  {
    requireNamespaceQuietStop("nnet")
    ## check for factors
    ## this is from nnet.formula

    ind <- seq(along = y)
    if(is.factor(y))
      {
        classLev <- levels(y)
        y <- class2ind(y)
      } else classLev <- NULL

    if(is.matrix(y)) classLev <- colnames(y)

    theDots <- list(...)

    ## to avoid a "no visible binding for global variable 'i'" warning:
    i <- NULL
    `%op%` <-  if(allowParallel)  `%dopar%` else  `%do%`
     mods <- foreach(i = 1:repeats,
                     .verbose = FALSE,
                     .packages = "caret",
                     .errorhandling = "stop") %op%
    {
      if(any(names(theDots) == "trace"))
        {
          if(theDots$trace) cat("\nFitting Repeat", i, "\n\n")
        } else cat("Fitting Repeat", i, "\n\n")
      set.seed(as.integer(seeds[i]))
      if(bag)  ind <- sample(1:nrow(x), replace = TRUE)
      thisMod <- if(is.null(classLev)) nnet::nnet(x[ind,,drop = FALSE], y[ind], ...) else nnet::nnet(x[ind,,drop = FALSE], y[ind,], ...)
      thisMod$lev <- classLev
      thisMod
    }

    ## return results
    out <- list(model = mods,
                repeats = repeats,
                bag = bag,
                seeds = seeds,
                names = colnames(x))
    class(out) <- "avNNet"
    out
  }

#' @rdname avNNet
#' @method print avNNet
#' @export
print.avNNet <- function (x, ...)
{
  cat("Model Averaged Neural Network with", x$repeats, "Repeats", ifelse(x$bag, "and Bagging", ""), "\n\n")
  print(x$model[[1]])
  cat("\n")
  invisible(x)
}

#' @importFrom stats .checkMFClasses delete.response fitted.values model.frame model.matrix predict na.omit
#' @rdname avNNet
#' @method predict avNNet
#' @export
predict.avNNet <- function(object, newdata, type = c("raw", "class", "prob"), ...)
  {
    loadNamespace("nnet")
    if (!inherits(object, "avNNet"))
      stop("object not of class \"avNNet\"")
    if (missing(newdata))
      {
        if(is.null(object$model[[1]]$lev))
          {
            out <- lapply(object$model, fitted.values)
            out <- do.call("cbind", out)
            return(apply(out, 1, mean))
          } else {
            for(i in 1:object$repeats)
              {
                rawTmp <- fitted.values(object$model[[i]])
                rawTmp <- t(apply(rawTmp, 1, function(x) exp(x)/sum(exp(x))))
                scores <- if(i == 1) rawTmp else scores + rawTmp
              }
            scores <- scores/object$repeats
            classes <- colnames(scores)[apply(scores, 1, which.max)]
            classes <- factor(as.character(classes), levels = object$model[[1]]$lev)
            if(type[1]== "raw") out <- scores
            if(type[1]== "class")  out <- (classes)
            if(type[1]== "prob")  out <- t(apply(scores, 1, function(x) x/sum(x)))
          }
      }  else {
        if (inherits(object, "avNNet.formula")) {
          newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
          rn <- row.names(newdata)
          Terms <- delete.response(object$terms)
          m <- model.frame(Terms, newdata, na.action = na.omit,
                           xlev = object$xlevels)
          if (!is.null(cl <- attr(Terms, "dataClasses")))
            .checkMFClasses(cl, m)
          keep <- match(row.names(m), rn)
          x <- model.matrix(Terms, m, contrasts = object$contrasts)
          xint <- match("(Intercept)", colnames(x), nomatch = 0)
          if (xint > 0)
            x <- x[, -xint, drop = FALSE]
        }
        else {
          if (is.null(dim(newdata)))
            dim(newdata) <- c(1, length(newdata))
          x <- as.matrix(newdata)
          if (any(is.na(x)))
            stop("missing values in 'x'")
          keep <- 1:nrow(x)
          rn <- rownames(x)
        }
        if(!is.null(object$names))  x <- x[, object$names, drop = FALSE]
        if(is.null(object$model[[1]]$lev))
          {
            out <- lapply(object$model, predict, newdata = x)
            out <- do.call("cbind", out)
            return(apply(out, 1, mean))
          } else {
            for(i in 1:object$repeats)
              {
                scores <- if(i == 1) predict(object$model[[i]], newdata = x) else scores + predict(object$model[[i]], newdata = x)

              }
            scores <- scores/object$repeats
            classes <- colnames(scores)[apply(scores, 1, which.max)]
            classes <- factor(as.character(classes), levels = object$model[[1]]$lev)
            if(type[1]== "raw") out <- scores
            if(type[1]== "class")  out <- (classes)
            if(type[1]== "prob")  out <- t(apply(scores, 1, function(x) x/sum(x)))
          }

      }
    out

  }

