#' @rdname caret-internal
#' @export
resampleWrapper <- function(x, ind) 
{
   out <- rep(NA, dim(x$data)[1])
   trainData <- x$data
   x$data <- x$data[ind,]
   tmpModelFit <- do.call(createModel, x)
   outBagData <- trainData[-ind, ]        
   outBagData$.outcome <- NULL
   
   out[-ind] <- if(is.factor(x$data$.outcome))
   {
      as.character(predictionFunction(x$method, tmpModelFit, outBagData))
   } else {
      predictionFunction(x$method, tmpModelFit, outBagData)
   }   
   out
}

