findCorrelation <- function(x, cutoff = .90, verbose = FALSE)
{
   varnum<-dim(x)[1]
   
   if(!isTRUE(all.equal(x, t(x)))) stop("correlation matrix is not symmetric")
   if(varnum ==1) stop("only one variable given")
    
   x <- abs(x)
    
   # re-ordered columns based on max absolute correlation
   originalOrder <- 1:varnum   
   
   averageCorr <- function(x) mean(x, na.rm = TRUE)
   tmp <- x
   diag(tmp) <- NA
   
   maxAbsCorOrder <- order(apply(tmp, 2, averageCorr), decreasing = TRUE)
   x <- x[maxAbsCorOrder, maxAbsCorOrder]
   newOrder <- originalOrder[maxAbsCorOrder]
   
   deletecol <- 0

    for(i in 1:(varnum-1))
   {  
       for(j in (i+1):varnum)
      {  
         if(!any(i == deletecol)  & !any(j == deletecol))
         {
            if(verbose)
               cat("Considering row\t", newOrder[i], 
                  "column\t", newOrder[j], 
                  "value\t", round(x[i,j], 3), "\n")        
            if(abs(x[i,j]) > cutoff)
            {
               if(mean(x[i, -i]) > mean(x[-j, j]))
               {
                  deletecol <- unique(c(deletecol, i))    
                  if(verbose) cat("  Flagging column\t", newOrder[i], "\n")                         
               } else {
                  deletecol <- unique(c(deletecol, j))      
                  if(verbose) cat("  Flagging column\t", newOrder[j], "\n")                                       
               }
            }
         }
      }
   }
   deletecol <- deletecol[deletecol != 0]
   newOrder[deletecol]
}   
