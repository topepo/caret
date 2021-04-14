# add model averaging?
# check predict method with formula interface
# how to handle variable imp?


#' Neural Networks with a Principal Component Step
#'
#' Run PCA on a dataset, then use it in a neural network model
#'
#' The function first will run principal component analysis on the data. The
#' cumulative percentage of variance is computed for each principal component.
#' The function uses the \code{thresh} argument to determine how many
#' components must be retained to capture this amount of variance in the
#' predictors.
#'
#' The principal components are then used in a neural network model.
#'
#' When predicting samples, the new data are similarly transformed using the
#' information from the PCA analysis on the training data and then predicted.
#'
#' Because the variance of each predictor is used in the PCA analysis, the code
#' does a quick check to make sure that each predictor has at least two
#' distinct values. If a predictor has one unique value, it is removed prior to
#' the analysis.
#'
#' @aliases pcaNNet pcaNNet.default predict.pcaNNet pcaNNet.formula
#' @param formula A formula of the form \code{class ~ x1 + x2 + \dots{}}
#' @param x matrix or data frame of \code{x} values for examples.
#' @param y matrix or data frame of target values for examples.
#' @param weights (case) weights for each example - if missing defaults to 1.
#' @param thresh a threshold for the cumulative proportion of variance to
#' capture from the PCA analysis. For example, to retain enough PCA components
#' to capture 95 percent of variation, set \code{thresh = .95}
#' @param data Data frame from which variables specified in \code{formula} are
#' preferentially to be taken.
#' @param subset An index vector specifying the cases to be used in the
#' training sample.  (NOTE: If given, this argument must be named.)
#' @param na.action A function to specify the action to be taken if \code{NA}s
#' are found. The default action is for the procedure to fail.  An alternative
#' is na.omit, which leads to rejection of cases with missing values on any
#' required variable.  (NOTE: If given, this argument must be named.)
#' @param contrasts a list of contrasts to be used for some or all of the
#' factors appearing as variables in the model formula.
#' @param object an object of class \code{pcaNNet} as returned by
#' \code{pcaNNet}.
#' @param newdata matrix or data frame of test examples. A vector is considered
#' to be a row vector comprising a single case.
#' @param type Type of output
#' @param \dots arguments passed to \code{\link[nnet]{nnet}}, such as
#' \code{size}, \code{decay}, etc.
#' @return For \code{pcaNNet}, an object of \code{"pcaNNet"} or
#' \code{"pcaNNet.formula"}. Items of interest in the output are: \item{pc
#' }{the output from \code{\link{preProcess}}} \item{model }{the model
#' generated from \code{\link[nnet]{nnet}}} \item{names }{if any predictors had
#' only one distinct value, this is a character string of the remaining
#' columns. Otherwise a value of \code{NULL}}
#' @author These are heavily based on the \code{nnet} code from Brian Ripley.
#' @seealso \code{\link[nnet]{nnet}}, \code{\link{preProcess}}
#' @references Ripley, B. D. (1996) \emph{Pattern Recognition and Neural
#' Networks.} Cambridge.
#' @keywords neural
#' @examples
#'
#' data(BloodBrain)
#' modelFit <- pcaNNet(bbbDescr[, 1:10], logBBB, size = 5, linout = TRUE, trace = FALSE)
#' modelFit
#'
#' predict(modelFit, bbbDescr[, 1:10])
#'
#' @export pcaNNet
pcaNNet <- function (x, ...)
   UseMethod("pcaNNet")


# this is a near copy of nnet.formula
#' @rdname pcaNNet
#' @method pcaNNet formula
#' @importFrom stats .getXlevels contrasts model.matrix model.response model.weights
#' @export
pcaNNet.formula <- function (formula, data, weights, ...,
                             thresh = .99,
                             subset, na.action, contrasts = NULL)
{

    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval.parent(m$data)))
        m$data <- as.data.frame(data, stringsAsFactors = TRUE)
    m$... <- m$contrasts <- NULL
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

    res <- pcaNNet.default(x, y, weights = w, thresh = thresh, ...)
    res$terms <- Terms
    res$coefnames <- colnames(x)
    res$na.action <- attr(m, "na.action")
    res$contrasts <- cons
    res$xlevels <- .getXlevels(Terms, m)
    class(res) <- c("pcaNNet.formula", "pcaNNet")
    res
}

#' @rdname pcaNNet
#' @method pcaNNet default
#' @export
pcaNNet.default <- function(x, y, thresh = .99, ...)
  {
    requireNamespaceQuietStop("nnet")

    # check for no variance data
    isZV <- apply(x, 2,
                  function(u) length(unique(u)) < 2)
    if(any(isZV))
      {
        x <- x[,!isZV, drop = FALSE]
        xNames <- colnames(x)
      } else xNames <- NULL

    # get pca
    pp <- preProcess(x, "pca", thresh = thresh)
    x <- predict(pp, x)

    # check for factors

    if(is.factor(y))
      {
        classLev <- levels(y)
        y <- class2ind(y)
      } else classLev <- NULL


    # fit nnet
    modelFit <- nnet::nnet(x, y, ...)
    modelFit$lev <- classLev

    # return results
    out <- list(model = modelFit,
                pc = pp,
                names = xNames)
    class(out) <- "pcaNNet"
    out
  }

#' @rdname pcaNNet
#' @method print pcaNNet
#' @export
print.pcaNNet <- function (x, ...)
{
  cat("Neural Network Model with PCA Pre-Processing\n\n")

  cat("Created from", x$pc$dim[1], "samples and", x$pc$dim[2], "variables\n")
  cat("PCA needed", x$pc$numComp, "components to capture",
      round(x$pc$thresh * 100, 2), "percent of the variance\n\n")

  print(x$model)
  cat("\n")
  invisible(x)
}

#' @rdname pcaNNet
#' @method predict pcaNNet
#' @importFrom stats .checkMFClasses delete.response model.frame model.matrix predict na.omit fitted
#' @export
predict.pcaNNet <- function(object, newdata, type = c("raw", "class", "prob"), ...)
  {
    requireNamespaceQuietStop("nnet")
    if (!inherits(object, "pcaNNet"))
      stop("object not of class \"pcaNNet\"")
    if (missing(newdata))
      {
        if(is.null(object$model$lev))
           {
             return(fitted(object$model))
           } else {
             scores <- fitted(object$model)
             classes <- colnames(scores)[apply(scores, 1, which.max)]
             classes <- factor(as.character(classes), levels = object$model$lev)
             if(type[1]== "raw") return(scores) else return(classes)
           }
      }  else {
        if (inherits(object, "pcaNNet.formula")) {
          newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
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
      }

    if(!is.null(object$names)) x <- x[, object$names, drop = FALSE]

    x <- predict(object$pc, x)
    if(type[1] %in% c("raw", "class")) {
      out <- predict(object$model, x, type = type[1], ...)
    } else {
      out <- predict(object$model, x, type = "raw", ...)
      out <- t(apply(out, 1, function(x) x/sum(x)))
    }
    out
  }

