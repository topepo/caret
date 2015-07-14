
findCorrelation_fast <- function(x, cutoff = .90, verbose = FALSE){
  averageCorr <- colMeans(abs(x))
  averageCorr <- as.numeric(as.factor(averageCorr))
  x[lower.tri(x, diag = TRUE)] <- NA
  combsAboveCutoff <- which(abs(x) > cutoff)
  
  colsToCheck <- ceiling(combsAboveCutoff / nrow(x))
  rowsToCheck <- combsAboveCutoff %% nrow(x)
  
  colsToDiscard <- averageCorr[colsToCheck] > averageCorr[rowsToCheck]
  rowsToDiscard <- !colsToDiscard
  
  if(verbose){
    colsFlagged <- pmin(ifelse(colsToDiscard, colsToCheck, NA),
                        ifelse(rowsToDiscard, rowsToCheck, NA), na.rm = TRUE)
    values <- round(x[combsAboveCutoff], 3)
    cat('\n',paste('Combination row', rowsToCheck, 'and column', colsToCheck,
                   'is above the cut-off, value =', values,
                   '\n \t Flagging column', colsFlagged, '\n'
    ))
  }
  
  deletecol <- c(colsToCheck[colsToDiscard], rowsToCheck[rowsToDiscard])
  deletecol <- unique(deletecol)
  deletecol
}

findCorrelation_exact <- function(x, cutoff = 0.90, verbose = FALSE)
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
  rm(tmp)
  
  deletecol <- rep(FALSE, varnum)
  
  x2 <- x
  diag(x2) <- NA
  
  for (i in 1:(varnum - 1)) {
    if(!any(x2[!is.na(x2)] > cutoff)){
      if (verbose) cat("All correlations <=", cutoff, "\n")
      break()
    }
    if (deletecol[i]) next
    for (j in (i + 1):varnum) {
      if (!deletecol[i] & !deletecol[j]) {
        
        if (x[i, j] > cutoff) {
          mn1 <- mean(x2[i,], na.rm = TRUE)
          mn2 <- mean(x2[-j,], na.rm = TRUE)
          if(verbose) cat("Compare row", newOrder[i], 
                          " and column ", newOrder[j], 
                          "with corr ", round(x[i,j], 3), "\n")  
          if (verbose) cat("  Means: ", round(mn1, 3), "vs", round(mn2, 3))
          if (mn1 > mn2) {
            deletecol[i] <- TRUE
            x2[i, ] <- NA
            x2[, i] <- NA
            if (verbose) cat(" so flagging column", newOrder[i], "\n")
          }
          else {
            deletecol[j] <- TRUE
            x2[j, ] <- NA
            x2[, j] <- NA
            if (verbose) cat(" so flagging column", newOrder[j], "\n")
          }
        }
      }
    }
  }
  newOrder[which(deletecol)]
}


findCorrelation <- function(x, cutoff = 0.90, verbose = FALSE, names = FALSE, exact = ncol(x) < 100) {
  if(names & is.null(colnames(x)))
    stop("'x' must have column names when `names = TRUE`")
  out <- if(exact) 
    findCorrelation_exact(x = x, cutoff = cutoff, verbose = verbose) else 
      findCorrelation_fast(x = x, cutoff = cutoff, verbose = verbose)
  out
  if(names) out <- colnames(x)[out]
  out
}



