#' Compute the multivariate spatial sign
#' 
#' Compute the spatial sign (a projection of a data vector to a
#'  unit length circle). The spatial sign of a vector \code{w} is
#'  \code{w /norm(w)}.
#' 
#' @aliases spatialSign spatialSign.default spatialSign.matrix
#' spatialSign.data.frame
#' @param x an object full of numeric data (which should probably
#'  be scaled). Factors are not allowed. This could be a vector,
#'  matrix or data frame.
#' @param na.rm A logical; should missing data be removed when
#'  computing the norm of the vector?
#' @param ... Not currently used. 
#' @return A vector, matrix or data frame with the same dim names
#'  of the original data.
#' @author Max Kuhn
#' @references Serneels et al. Spatial sign preprocessing: a
#'  simple way to impart moderate robustness to multivariate
#'  estimators. J. Chem. Inf. Model (2006) vol. 46 (3) pp. 1402-1409
#' @keywords manip
#' @examples
#' 
#' spatialSign(rnorm(5))
#' 
#' spatialSign(matrix(rnorm(12), ncol = 3))
#' 
#' # should fail since the fifth column is a factor
#' try(spatialSign(iris), silent = TRUE)
#' 
#' spatialSign(iris[,-5])
#' 
#' trellis.par.set(caretTheme())
#' featurePlot(iris[,-5], iris[,5], "pairs")
#' featurePlot(spatialSign(scale(iris[,-5])), iris[,5], "pairs")
#' 
#' @export spatialSign
"spatialSign" <- function(x, ...) 
  UseMethod("spatialSign")

#' @export
#' @rdname spatialSign
"spatialSign.default" <- function(x, na.rm = TRUE, ...) {
  if (is.character(x) | is.factor(x))
    stop("spatial sign is not defined for character or factor data",
         call. = FALSE)
  denom <- sum(x ^ 2, na.rm = na.rm)
  out <-
    if (sqrt(denom) > .Machine$double.eps)
      x / sqrt(denom)
  else
    x * 0
  out
}

#' @export
#' @rdname spatialSign
"spatialSign.matrix" <- function(x, na.rm = TRUE, ...) {
  # check for character data
  if (is.character(x))
    stop("spatial sign is not defined for character data",
         call. = FALSE)
  xNames <- dimnames(x)
  p <- ncol(x)
  tmp <- t(apply(x, 1, spatialSign.default, na.rm = na.rm))
  if (p == 1 & nrow(tmp) == 1)
    tmp <- t(tmp)
  dimnames(tmp) <- xNames
  tmp
}

#' @export
#' @rdname spatialSign
"spatialSign.data.frame" <- function(x, na.rm = TRUE, ...) {
  if (any(apply(x, 2, function(data)
    is.character(data) | is.factor(data))))
    stop("spatial sign is not defined for character or factor data",
         call. = FALSE)
  xNames <- dimnames(x)
  x <- as.matrix(x)
  if (!is.numeric(x))
    stop("a character matrix was the result of as.matrix",
         call. = FALSE)
  tmp <- spatialSign(x, na.rm = na.rm)
  dimnames(tmp) <- xNames
  tmp
}

