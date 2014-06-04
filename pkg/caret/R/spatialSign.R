
"spatialSign" <- function(x) UseMethod("spatialSign")

"spatialSign.default" <- function(x)
{
   if(is.character(x) | is.factor(x)) stop("spatial sign is not defined for character or factor data")
   denom <- sum(x^2)
   out <- if(sqrt(denom) > .Machine$double.eps)  x / sqrt(denom) else x * 0
   out
}

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

