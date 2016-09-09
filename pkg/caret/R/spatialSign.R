#' Compute the multivariate spatial sign
#' 
#' Compute the spatial sign (a projection of a data vector to a unit length
#' circle). The spatial sign of a vector \code{w} is \code{w /norm(w)}.
#' 
#' 
#' @aliases spatialSign spatialSign.default spatialSign.matrix
#' spatialSign.data.frame
#' @param x an object full of numeric data (which should probably be scaled).
#' Factors are not allowed. This could be a vector, matrix or data frame.
#' @return A vector, matrix or data frame with the same dim names of the
#' original data.
#' @author Max Kuhn
#' @references Serneels et al. Spatial sign preprocessing: a simple way to
#' impart moderate robustness to multivariate estimators. J. Chem. Inf. Model
#' (2006) vol. 46 (3) pp. 1402-1409
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
"spatialSign" <- function(x) UseMethod("spatialSign")

#' @export
"spatialSign.default" <- function(x)
{
   if(is.character(x) | is.factor(x)) stop("spatial sign is not defined for character or factor data")
   denom <- sum(x^2)
   out <- if(sqrt(denom) > .Machine$double.eps)  x / sqrt(denom) else x * 0
   out
}

#' @export
"spatialSign.matrix" <- function(x)
{
   # check for character data
   if(is.character(x)) stop("spatial sign is not defined for character data")
   xNames <- dimnames(x)
   p <- ncol(x)
   tmp <- t(apply(x, 1, spatialSign.default))
   if(p == 1 & nrow(tmp) == 1) tmp <- t(tmp)
   dimnames(tmp) <- xNames
   tmp
}

#' @export
"spatialSign.data.frame" <- function(x)
{
   if(any(apply(x, 2, function(data) is.character(data) | is.factor(data))))
      stop("spatial sign is not defined for character or factor data")
   xNames <- dimnames(x)
   x <- as.matrix(x)
   if(!is.numeric(x)) stop("a character matrix was the result of as.matrix")
   tmp <- spatialSign(x)
   dimnames(tmp) <- xNames
   tmp
}

