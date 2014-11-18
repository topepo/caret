PLS <- function (x, ...) 
   UseMethod("PLS")

PLS.default <- function (x, y, ncomp = min(dim(x)[2], 5),  ...) 
{
    # much of this function is from the nnet package
    class.ind <- function(cl) 
    {

        n <- length(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1:n) + n * (as.vector(unclass(cl)) - 1)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }    
    if(!is.matrix(x)) x <- as.matrix(x)
    if (any(is.na(x))) 
        stop("missing values in 'x'")
    if (any(is.na(y))) 
        stop("missing values in 'y'")
    if (dim(x)[1] != length(y)) 
        stop("nrows of 'x' and 'y' must match")        
        
   if (is.factor(y)) 
   {
      isRegression <- FALSE
      lev <- levels(y)
      counts <- table(y)
      if (any(counts == 0)) 
      {
         empty <- lev[counts == 0]
         warning(sprintf(ngettext(length(empty), "group %s is empty", 
            "groups %s are empty"), paste(empty, collapse = " ")), 
            domain = NA)
         y <- factor(y, levels = lev[counts > 0])
      }
      yLevels <- lev[counts > 0]
      y <- class.ind(y)
   } else {
      isRegression <- TRUE
      yLevels <- NULL
   }
   
   library(pls)   
   out <- simpls.fit(x, y, ncomp)     
   out$ncomp <- ncomp
   out$isRegression <- isRegression
   out$x <- x
   out$y <- y
   out$yLevels <- yLevels

   structure(out, class = "PLS")
}
