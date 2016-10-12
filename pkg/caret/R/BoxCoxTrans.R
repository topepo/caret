#' Box-Cox and Exponential Transformations
#'
#' @name BoxCoxTrans
#' @aliases BoxCoxTrans.default BoxCoxTrans predict.BoxCoxTrans expoTrans.default expoTrans predict.expoTrans
#'
#' @description These classes can be used to estimate transformations and apply them to existing and future data
#'
#' @param y a numeric vector of data to be transformed. For \code{BoxCoxTrans}, the data must be strictly positive.
#' @param x an optional dependent variable to be used in a linear model.
#' @param fudge a tolerance value: lambda values within +/-fudge will be coerced to 0 and within 1+/-fudge will be coerced to 1.
#' @param numUnique how many unique values should \code{y} have to estimate the transformation?
#' @param na.rm a logical value indicating whether \code{NA} values should be stripped from \code{y} and \code{x} before the computation proceeds.
#' @param newdata a numeric vector of values to transform.
#' @param digits minimal number of \emph{significant digits}.
#' @param object an object of class \code{BoxCoxTrans} or \code{expoTrans}.
#' @param \dots for \code{BoxCoxTrans}: options to pass to \code{\link[MASS]{boxcox}}. \code{plotit} should not be passed through. For \code{predict.BoxCoxTrans}, additional arguments are ignored.
#'
#' @details
#' \code{BoxCoxTrans} function is basically a wrapper for the \code{\link[MASS]{boxcox}} function in the MASS library. It can be used to estimate the transformation and apply it to new data.
#'
#' \code{expoTrans} estimates the exponential transformation of Manly (1976) but assumes a common mean for
#' the data. The transformation parameter is estimated by directly maximizing the likelihood.
#'
#' If \code{any(y <= 0)} or  if \code{length(unique(y)) < numUnique}, lambda is not estimated and no
#' transformation is applied.
#'
#' @return
#' Both functions returns a list of class of either \code{BoxCoxTrans} or \code{expoTrans} with
#' elements
#' \item{lambda }{estimated transformation value}
#' \item{fudge }{value of \code{fudge}}
#' \item{n }{number of data points used to estimate lambda}
#' \item{summary }{the results of \code{summary(y)}}
#' \item{ratio }{\code{max(y)/min(y)}}
#' \item{skewness }{sample skewness statistic}
#' \code{BoxCoxTrans} also returns: \item{fudge }{value of \code{fudge}}
#'
#' The \code{predict} functions returns numeric vectors of transformed values
#'
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations (with discussion). Journal of the Royal Statistical Society B, 26, 211-252.
#' Manly, B. L. (1976) Exponential data transformations. The Statistician, 25, 37 - 42.
#'
#' @author Max Author
#'
#' @seealso \code{\link[MASS]{boxcox}}, \code{\link{preProcess}}, \code{\link{optim}}
#'
#' @examples
#' data(BloodBrain)
#'
#' ratio <- exp(logBBB)
#' bc <- BoxCoxTrans(ratio)
#' bc
#'
#' predict(bc, ratio[1:5])
#'
#' ratio[5] <- NA
#' bc2 <- BoxCoxTrans(ratio, bbbDescr$tpsa, na.rm = TRUE)
#' bc2
#'
#' manly <- expoTrans(ratio)
#' manly
#'
#' @keywords utilities
#'
#' @export
BoxCoxTrans <- function(y, ...) UseMethod("BoxCoxTrans")


##TODO add an epsilon?
## TODO add exclusion list to preProc?
#' @rdname BoxCoxTrans
#' @export
BoxCoxTrans.default <- function(y, x = rep(1, length(y)), fudge = .2, numUnique = 3,  na.rm = FALSE, ...) {
  requireNamespaceQuietStop("MASS")
  requireNamespaceQuietStop("e1071")
  if(na.rm && (any(is.na(y)) | any(is.na(x)))) {
    rmv <- is.na(y) | is.na(x)
    y <- y[!rmv]
    x <- x[!rmv]
  }
  if(!is.numeric(y) | is.factor(y) | is.character(y)) stop("y must be numeric")

  if(any(y <= 0) | length(unique(y)) < numUnique) {
    out <- list(lambda = NA,
                summary = summary(y),
                ratio = NA,
                n = length(y))
  } else {
    bc <- MASS::boxcox(y~x, plotit = FALSE, ...)
    out <- list(lambda = bc$x[which.max(bc$y)])
  }
  out$fudge <- fudge
  out$n <- length(y)
  out$summary <- summary(y)
  out$ratio <- max(y)/min(y)
  out$skewness <- e1071::skewness(y)
  class(out) <- "BoxCoxTrans"
  out
}


#' @rdname BoxCoxTrans
#' @method print BoxCoxTrans
#' @export
print.BoxCoxTrans <- function(x, newdata, digits = 3, ...){
  cat("Box-Cox Transformation\n\n")

  cat(x$n, "data points used to estimate Lambda\n\n")
  cat("Input data summary:\n")
  print(x$summary)
  if(!is.na(x$lambda)) {
    cat("\nLargest/Smallest:", signif(x$ratio, digits), "\n")
    cat("Sample Skewness:", signif(x$skewness, digits), "\n")
    cat("\nEstimated Lambda:", signif(x$lambda, digits), "\n")
    if(x$lambda < x$fudge & x$lambda > -x$fudge)
      cat("With fudge factor, Lambda = 0 will be used for transformations\n")
    if(x$lambda < 1+x$fudge & x$lambda > 1-x$fudge)
      cat("With fudge factor, no transformation is applied\n")
  } else cat("\nLambda could not be estimated; no transformation is applied\n")
  cat("\n")
  invisible(x)
}


#' @rdname BoxCoxTrans
#' @method predict BoxCoxTrans
#' @export
predict.BoxCoxTrans <- function(object, newdata, ...) {
  if(!is.vector(newdata) || !is.numeric(newdata)) stop("newdata should be a numeric vector")
  if(is.na(object$lambda))  {
    out <- newdata
  } else {
    if(object$lambda < object$fudge & object$lambda > -object$fudge)  {
      if(any(newdata[!is.na(newdata)] <= 0)) warning("newdata should have values > 0")
      out <- log(newdata)
    } else {
      if(object$lambda < 1+object$fudge & object$lambda > 1-object$fudge) {
        out <- newdata
      } else out <- (newdata^object$lambda - 1)/object$lambda
    }
  }
  out
}
