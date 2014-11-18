
predict.PLS <- function(object, newdata, 
   ncomp = NULL,
   type = ifelse(object$isRegression, "response", "class"), ...)
{

   if(is.null(ncomp) & !is.null(object$bestIter$.ncomp)) ncomp <- object$bestIter$.ncomp

   if(is.null(ncomp)) stop("the number of components must be given")
   
   # adapted from the pls package
   if(object$isRegression & c(type %in% c("class", "prob")))
      stop("cannot get a class estimate if the original y was not a factor")

   if(!(type %in% c("response", "class", "prob")))
      stop("type must be either response, class or prob")

   if(missing(newdata)) newdata <- object$x
   if(!is.matrix(newdata)) newdata <- as.matrix(newdata)

   # from coef.mvr in pls package
   B <- object$coefficients[, , 1:ncomp, drop = FALSE]
   dB <- dim(B)
   dB[1] <- dB[1] + 1
   dnB <- dimnames(B)
   dnB[[1]] <- c("(Intercept)", dnB[[1]])
   BInt <- array(dim = dB, dimnames = dnB)
   BInt[-1, , ] <- B
   for (i in seq(along = 1:ncomp)) BInt[1, , i] <- object$Ymeans - object$Xmeans %*% B[, , i]
   B <- BInt
   # stop
   
   # from predict.mvr in pls package   
   dPred <- dim(B)
   dPred[1] <- dim(newdata)[1]
   dnPred <- dimnames(B)
   dnPred[1] <- dimnames(newdata)[1]
   pred <- array(dim = dPred, dimnames = dnPred)
   predY <- sweep(newdata %*% B[-1, , ncomp], 2, B[1, , ncomp], "+")   
   # stop
 
   out <- switch(
      type,
      response = predY,
      class = 
         {
            classNum <- apply(predY, 1, which.max)
            factor(object$yLevels[classNum], levels = object$yLevels)   
         },
      # use softmax technique here         
      prob =  t(apply(predY, 1, function(data) exp(data)/sum(exp(data)))))

   out
}
