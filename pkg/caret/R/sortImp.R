#' @rdname caret-internal
#' @importFrom stats runif
#' @export
sortImp <- function(object, top)
{

   if(object$calledFrom == "varImp")
   {
      best <- switch(
         object$model,
         pam = "maxabs",
         "max")   
   } else {
      best <- "max"
   }
     
   featureRank <- switch(best,
      max = rank(-apply(object$importance, 1, max, na.rm = TRUE)),
      min = rank(apply(object$importance, 1, min, na.rm = TRUE)),         
      maxabs = rank(-apply(abs(object$importance), 1, max, na.rm = TRUE)))     

   tiedRanks <- as.numeric(names(table(featureRank)[table(featureRank) > 1]))

   if(length(tiedRanks) > 0)
   {
      for(i in seq(along = tiedRanks))
      {
         tmp <- featureRank[featureRank == tiedRanks[i]] 
         featureRank[featureRank == tiedRanks[i]] <- tmp + runif(length(tmp), min = 0.001, max = 0.999)
      }
   }

   featureOrder <- order(featureRank)   

   out <- object$importance[featureOrder,, drop = FALSE]
   out <- out[1:top,, drop = FALSE]
   out
}

