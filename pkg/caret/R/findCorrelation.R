findCorrelation <- function(x, cutoff = 0.90, verbose = FALSE)
{
  varnum <- dim(x)[1]
  
  if (!isTRUE(all.equal(x, t(x)))) stop("correlation matrix is not symmetric")
  if (varnum == 1) stop("only one variable given")
  
  x <- abs(x)
  
  # re-ordered columns based on max absolute correlation
  originalOrder <- 1:varnum
  
  averageCorr <- function(x) mean(x, na.rm = TRUE)
  tmp <- x
  diag(tmp) <- NA
  
  maxAbsCorOrder <- order(apply(tmp, 2, averageCorr), decreasing = TRUE)
  x <- x[maxAbsCorOrder, maxAbsCorOrder]
  newOrder <- originalOrder[maxAbsCorOrder]
  
  deletecol <- rep(F, varnum)
  
  for (i in 1:(varnum - 1)) {
    if (deletecol[i]) next
    for (j in (i + 1):varnum) {
      if (!deletecol[i] & !deletecol[j]) {
        if(verbose)
          cat("Considering row\t", newOrder[i], 
              "column\t", newOrder[j], 
              "value\t", round(x[i,j], 3), "\n")  
        if (x[i, j] > cutoff) {
          if (mean(x[i, -i]) > mean(x[-j, j])) {
            deletecol[i] <- T
            if (verbose) cat("  Flagging column\t", newOrder[i], "\n")
          }
          else {
            deletecol[j] <- T
            if (verbose) cat("  Flagging column\t", newOrder[j], "\n")
          }
        }
      }
    }
  }
  newOrder[which(deletecol)]
}
