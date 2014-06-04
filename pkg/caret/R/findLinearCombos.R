# enumerate linear combinations
enumLC <- function(object, ...) UseMethod("enumLC")

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
         lcList <- enumLC(x[,-badList])
         continue <- (length(lcList) > 0)
      }
   } else badList <- NULL
   list(linearCombos = initialList, remove = badList)
}
