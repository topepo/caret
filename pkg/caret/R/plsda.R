plsda <- function (x, ...)
  UseMethod("plsda")



predict.plsda <- function(object, newdata = NULL, ncomp = NULL, type = "class", ...){
  requireNamespaceQuietStop('pls')
  if(is.null(ncomp)) 
    if(!is.null(object$ncomp)) ncomp <- object$ncomp else stop("specify ncomp")
  
  if(!is.null(newdata)) {
    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
  }
  
  ## make sure that the prediction function from pls is used
  class(object) <- "mvr"
  tmpPred <- predict(object, newdata = newdata)[,,ncomp,drop = FALSE]
  
  if(type == "raw") return(tmpPred)
  
  if(is.null(object$probModel)) {
    ## use softmax
    switch(type,
           
           class = {
             if(length(dim(tmpPred)) < 3) {
               ##only requested one component
               out <- object$obsLevels[apply(tmpPred, 1, which.max)]
               out <- factor(out, levels = object$obsLevels)
             } else {
               ## more than one component
               tmpOut <- matrix("", nrow = dim(tmpPred)[1], ncol = dim(tmpPred)[3])
               for(i in 1:dim(tmpPred)[3]) {
                 tmpOut[,i] <- object$obsLevels[apply(tmpPred[,,i,drop=FALSE], 1, which.max)]
               }
               out <- as.data.frame(tmpOut)
               out <- as.data.frame(lapply(out, function(x, y) factor(x, levels = y), y = object$obsLevels))
               names(out) <- paste("ncomp", ncomp, sep = "")
               rownames(out) <- rownames(newdata)
               if(length(ncomp) == 1) out <- out[,1]
             }
           },
           prob = {
             ## fix prob names
             if(length(dim(tmpPred)) < 3) {
               out <- t(apply(tmpPred, 1, function(data) exp(data)/sum(exp(data))))
             } else {
               ## more than one component
               out <- tmpPred * NA
               for(i in 1:dim(tmpPred)[3]) {
                 out[,,i] <- t(apply(tmpPred[,,i,drop=FALSE], 1, function(data) exp(data)/sum(exp(data))))
               }
             }
           })
  } else {
    ## Bayes rule
    
    library(klaR)
    tmp <- vector(mode = "list", length = length(ncomp))
    for(i in seq(along = ncomp)) {
      tmp[[i]] <- predict(object$probModel[[ ncomp[i] ]],
                          as.data.frame(tmpPred[,-length(object$obsLevels),i]))
    }
    
    if(type == "class") {
      out <- t(do.call("rbind",
                       lapply(tmp, function(x) as.character(x$class))))
      rownames(out) <- names(tmp[[1]]$class)
      colnames(out) <- paste("ncomp", ncomp, sep = "")
      out <- as.data.frame(out)
      out <- as.data.frame(lapply(out, function(x, y) factor(x, levels = y), y = object$obsLevels))
      if(length(ncomp) == 1) out <- out[,1]
    } else {
      out <- array(dim = c(dim(tmp[[1]]$posterior), length(ncomp)),
                   dimnames = list(
                     rownames(tmp[[1]]$posterior),
                     colnames(tmp[[1]]$posterior),
                     paste("ncomp", ncomp, sep = "")))
      for(i in seq(along = ncomp)) out[,,i] <- tmp[[i]]$posterior
    }
  }
  out
}


plsda.default <- function(x, y, ncomp = 2, probMethod = "softmax", prior = NULL, ...) {
  requireNamespaceQuietStop('pls')
  
  funcCall <- match.call(expand.dots = TRUE)
  
  if(!is.matrix(x)) x <- as.matrix(x)
  if(length(ncomp) > 1) {
    ncomp <- max(ncomp)
    warning(paste(
      "A value single ncomp must be specified.",
      "max(ncomp) was used.", 
      "Predictions can be obtained for values <= ncomp"))
  }
  
  if(probMethod == "softmax") {
    if(!is.null(prior)) warning("Priors are ignored unless probMethod = \"Bayes\"")
  }
  
  
  if(is.factor(y)) {
    obsLevels <- levels(y)
    oldY <- y
    y <- class2ind(y)
  } else {
    if(is.matrix(y)) {
      test <- apply(y, 1, sum)
      if(any(test != 1)) stop("the rows of y must be 0/1 and sum to 1")
      obsLevels <- colnames(y)
      if(is.null(obsLevels)) stop("the y matrix must have column names")
      oldY <- obsLevels[apply(y, 1, which.max)]
    } else stop("y must be a matrix or a factor")
  }
  
  if(!is.matrix(x)) x <- as.matrix(x)
  
  tmpData <- data.frame(n = paste("row", 1:nrow(y), sep = ""))
  tmpData$y <- y
  tmpData$x <- x
  
  out <- pls::plsr(y ~ x, data = tmpData, ncomp = ncomp, ...)
  
  out$obsLevels <- obsLevels
  out$probMethod <- probMethod
  if(probMethod == "Bayes") {
    requireNamespaceQuietStop('klaR')
    makeModels <- function(x, y, pri) {
      probModel <- klaR::NaiveBayes(x, y, prior = pri, usekernel = TRUE)
      probModel$train <- predict(probModel)$posterior
      probModel$x <- NULL
      probModel
    }
    cls <- class(out)
    class(out) <- "mvr"
    train <- predict(out, as.matrix(tmpData$x), ncomp = 1:ncomp)
    ## Get the raw model predictions, but leave one behind since the
    ## final class probs sum to one
    train <- train[, -length(obsLevels),, drop = FALSE]
    
    out$probModel <- apply(train, 3, makeModels, y = oldY, pri = prior)
  } else out$probModel <- NULL 
  
  ##out$call <- funcCall
  class(out) <- c("plsda", class(out))
  out
}

print.plsda <- function (x, ...) {
  ## minor change to print.mvr
  switch(x$method,
         kernelpls = {
           regr = "Partial least squares"
           alg = "kernel"
         }, simpls = {
           regr = "Partial least squares"
           alg = "simpls"
         }, oscorespls = {
           regr = "Partial least squares"
           alg = "orthogonal scores"
         }, svdpc = {
           regr = "Principal component"
           alg = "singular value decomposition"
         }, stop("Unknown fit method."))
  cat(regr, "classification, fitted with the", alg, "algorithm.")
  if (!is.null(x$validation))
    cat("\nCross-validated using", length(x$validation$segments),
        attr(x$validation$segments, "type"), "segments.")
  
  switch(x$probMethod,
         softmax = cat("\nThe softmax function was used to compute class probabilities.\n"),
         Bayes = cat("\nBayes rule was used to compute class probabilities.\n"))
  printCall(x$call)
  invisible(x)
}
