# enumerate linear combinations
enumLC <- function(object, ...) UseMethod("enumLC")

#' @export
enumLC.default <- function(object, ...)
{
   # there doesn't seem to be a reasonable default, so
   # we'll throw an error
   stop(paste('enumLC does not support ', class(object), 'objects'))
}

enumLC.matrix <- function(object, ...)
{
   # factor the matrix using QR decomposition and then process it
   internalEnumLC(qr(object))
}

enumLC.lm <- function(object, ...)
{
   # extract the QR decomposition and the process it
   internalEnumLC(object$qr)
}

#' @importFrom stats lm
enumLC.formula <- function(object, ...)
{
   # create an lm fit object from the formula, and then call
   # appropriate enumLC method
   enumLC(lm(object))
}

# this function does the actual work for all of the enumLC methods
internalEnumLC <- function(qrObj, ...)
{
   R <- qr.R(qrObj)                     # extract R matrix
   numColumns <- dim(R)[2]              # number of columns in R
   rank <- qrObj$rank                   # number of independent columns
   pivot <- qrObj$pivot                 # get the pivot vector

   if (is.null(numColumns) || rank == numColumns)
   {
      list()                            # there are no linear combinations
   } else {
      p1 <- 1:rank
      X <- R[p1, p1]                    # extract the independent columns
      Y <- R[p1, -p1, drop = FALSE]     # extract the dependent columns
      b <- qr(X)                        # factor the independent columns
      b <- qr.coef(b, Y)                # get regression coefficients of
                                        # the dependent columns
      b[abs(b) < 1e-6] <- 0             # zap small values

      # generate a list with one element for each dependent column
      lapply(1:dim(Y)[2],
         function(i) c(pivot[rank + i], pivot[which(b[,i] != 0)]))
   }
}



#' Determine linear combinations in a matrix
#' 
#' Enumerate and resolve the linear combinations in a numeric matrix
#' 
#' The QR decomposition is used to determine if the matrix is full rank and
#' then identify the sets of columns that are involved in the dependencies.
#' 
#' To "resolve" them, columns are iteratively removed and the matrix rank is
#' rechecked.
#' 
#' The \code{\link[subselect:trim.matrix]{trim.matrix}} function in the
#' \pkg{subselect} package can also be used to accomplish the same goal.
#' 
#' @param x a numeric matrix
#' @return a list with elements: \item{linearCombos }{If there are linear
#' combinations, this will be a list with elements for each dependency that
#' contains vectors of column numbers. } \item{remove }{a list of column
#' numbers that can be removed to counter the linear combinations}
#' @author Kirk Mettler and Jed Wing (\code{enumLC}) and Max Kuhn
#' (\code{findLinearCombos})
#' @seealso \code{\link[subselect:trim.matrix]{trim.matrix}}
#' @keywords manip
#' @examples
#' 
#' testData1 <- matrix(0, nrow=20, ncol=8)
#' testData1[,1] <- 1
#' testData1[,2] <- round(rnorm(20), 1)
#' testData1[,3] <- round(rnorm(20), 1)
#' testData1[,4] <- round(rnorm(20), 1)
#' testData1[,5] <- 0.5 * testData1[,2] - 0.25 * testData1[,3] - 0.25 * testData1[,4]
#' testData1[1:4,6] <- 1
#' testData1[5:10,7] <- 1
#' testData1[11:20,8] <- 1
#' 
#' findLinearCombos(testData1)
#' 
#' testData2 <- matrix(0, nrow=6, ncol=6)
#' testData2[,1] <- c(1, 1, 1, 1, 1, 1)
#' testData2[,2] <- c(1, 1, 1, 0, 0, 0)
#' testData2[,3] <- c(0, 0, 0, 1, 1, 1)
#' testData2[,4] <- c(1, 0, 0, 1, 0, 0)
#' testData2[,5] <- c(0, 1, 0, 0, 1, 0)
#' testData2[,6] <- c(0, 0, 1, 0, 0, 1)
#' 
#' findLinearCombos(testData2)
#' 
#' @export findLinearCombos
findLinearCombos <- function(x)
{
   if(!is.matrix(x)) x <- as.matrix(x)
   lcList <- enumLC(x)
   initialList <- lcList
   badList <- NULL
   if(length(lcList) > 0)
   {
      continue <- TRUE
      while(continue)
      {
         # keep removing linear dependencies until it resolves
         tmp <- unlist(lapply(lcList, function(x) x[1]))   
         tmp <- unique(tmp[!is.na(tmp)])
         badList <- unique(c(tmp, badList))
         lcList <- enumLC(x[,-badList, drop = FALSE])
         continue <- (length(lcList) > 0)
      }
   } else badList <- NULL
   list(linearCombos = initialList, remove = badList)
}
