#' @importFrom stats complete.cases
findCorrelation_fast <- function(x, cutoff = .90, verbose = FALSE){
  if(any(!complete.cases(x)))
    stop("The correlation matrix has some missing values.")
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



#' Determine highly correlated variables
#' 
#' This function searches through a correlation matrix and returns a vector of
#' integers corresponding to columns to remove to reduce pair-wise
#' correlations.
#' 
#' The absolute values of pair-wise correlations are considered. If two
#' variables have a high correlation, the function looks at the mean absolute
#' correlation of each variable and removes the variable with the largest mean
#' absolute correlation.
#' 
#' Using \code{exact = TRUE} will cause the function to re-evaluate the average
#' correlations at each step while \code{exact = FALSE} uses all the
#' correlations regardless of whether they have been eliminated or not. The
#' exact calculations will remove a smaller number of predictors but can be
#' much slower when the problem dimensions are "big".
#' 
#' There are several function in the \pkg{subselect} package
#' (\code{\link[subselect:eleaps]{leaps}},
#' \code{\link[subselect:genetic]{genetic}},
#' \code{\link[subselect:anneal]{anneal}}) that can also be used to accomplish
#' the same goal but tend to retain more predictors.
#' 
#' @param x A correlation matrix
#' @param cutoff A numeric value for the pair-wise absolute correlation cutoff
#' @param verbose A boolean for printing the details
#' @param names a logical; should the column names be returned (\code{TRUE}) or
#' the column index (\code{FALSE})?
#' @param exact a logical; should the average correlations be recomputed at
#' each step? See Details below.
#' @return A vector of indices denoting the columns to remove (when \code{names
#' = TRUE}) otherwise a vector of column names. If no correlations meet the
#' criteria, \code{integer(0)} is returned.
#' @author Original R code by Dong Li, modified by Max Kuhn
#' @seealso \code{\link[subselect:eleaps]{leaps}},
#' \code{\link[subselect:genetic]{genetic}},
#' \code{\link[subselect:anneal]{anneal}}, \code{\link{findLinearCombos}}
#' @keywords manip
#' @examples
#' 
#' R1 <- structure(c(1, 0.86, 0.56, 0.32, 0.85, 0.86, 1, 0.01, 0.74, 0.32, 
#'                   0.56, 0.01, 1, 0.65, 0.91, 0.32, 0.74, 0.65, 1, 0.36,
#'                   0.85, 0.32, 0.91, 0.36, 1), 
#'                 .Dim = c(5L, 5L))
#' colnames(R1) <- rownames(R1) <- paste0("x", 1:ncol(R1))
#' R1
#' 
#' findCorrelation(R1, cutoff = .6, exact = FALSE)
#' findCorrelation(R1, cutoff = .6, exact = TRUE)
#' findCorrelation(R1, cutoff = .6, exact = TRUE, names = FALSE)
#' 
#' 
#' R2 <- diag(rep(1, 5))
#' R2[2, 3] <- R2[3, 2] <- .7
#' R2[5, 3] <- R2[3, 5] <- -.7
#' R2[4, 1] <- R2[1, 4] <- -.67
#' 
#' corrDF <- expand.grid(row = 1:5, col = 1:5)
#' corrDF$correlation <- as.vector(R2)
#' levelplot(correlation ~ row + col, corrDF)
#' 
#' findCorrelation(R2, cutoff = .65, verbose = TRUE)
#' 
#' findCorrelation(R2, cutoff = .99, verbose = TRUE)
#' 
#' @export findCorrelation
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



