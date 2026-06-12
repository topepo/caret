#' @export
splsda <- function (x, ...)
  UseMethod("splsda")

#' @importFrom stats predict
#' @export
predict.splsda <- function(object, newdata = NULL, type = "class", ...)
{
  requireNamespaceQuietStop("spls")
  tmpPred <- spls::predict.spls(object, newx = newdata)

  if(type == "raw") return(tmpPred)

  if(is.null(object$probModel))
    {
      ## use softmax
      out <- switch(type,

             class =
             {
               classIndex <- object$obsLevels[apply(tmpPred, 1, which.max)]
               factor(classIndex, levels = object$obsLevels)
             },
             prob = t(apply(tmpPred, 1, function(data) exp(data)/sum(exp(data)))))
    } else {
      requireNamespaceQuietStop("klaR")
      ## Bayes rule
      tmpPred <-  as.data.frame(tmpPred[,-length(object$obsLevels)], stringsAsFactors = TRUE)
      pred <- predict(object$probModel, tmpPred)
      out <- switch(type, class = pred$class, prob = pred$posterior)

    }
  out
}

#' @importFrom stats predict
#' @export
splsda.default <- function(x, y, probMethod = "softmax", prior = NULL, ...)
{
  requireNamespaceQuietStop("spls")
  funcCall <- match.call(expand.dots = TRUE)

  if(probMethod == "softmax")
    {
      if(!is.null(prior)) warning("Priors are ignored unless probMethod = \"Bayes\"")
    }

  if(is.factor(y))
    {
      obsLevels <- levels(y)
      oldY <- y
      y <- class2ind(y)
    } else {
      if(is.matrix(y))
        {
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

  out <- spls::spls(x, y, ...)

  out$obsLevels <- obsLevels
  out$probMethod <- probMethod
  if(probMethod == "Bayes")
    {

      requireNamespaceQuietStop("klaR")
      makeModels <- function(x, y, pri)
        {
          probModel <- klaR::NaiveBayes(x, y, prior = pri, usekernel = TRUE)
          probModel$train <- predict(probModel)$posterior
          probModel$x <- NULL
          probModel
        }
      train <- predict(out, as.matrix(tmpData$x))
      ## Get the raw model predictions, but leave one behind since the
      ## final class probs sum to one
      train <- train[, -length(obsLevels), drop = FALSE]

      out$probModel <- makeModels(train, oldY, pri = prior)
    } else out$probModel <- NULL

  ##out$call <- funcCall
  class(out) <- "splsda"
  out
}

#' @export
print.splsda <- function (x, ...)
{
    xmat <- x$x
    p <- ncol(xmat)
    A <- x$A
    xAnames <- colnames(xmat)[A]
    q <- ncol(x$y)
    eta <- x$eta
    K <- x$K
    kappa <- x$kappa
    select <- x$select
    fit <- x$fit
    if (q == 1) {
        cat("\nSparse Partial Least Squares for discriminant analysis\n")
        cat("----\n")
        cat(paste("Parameters: eta = ", eta, ", K = ", K, "\n",
            sep = ""))
    }
    if (q > 1) {
        cat("\nSparse Partial Least Squares for discriminant analysis\n")
        cat("----\n")
        cat(paste("Parameters: eta = ", eta, ", K = ", K, ", kappa = ",
            kappa, "\n", sep = ""))
    }
    cat(paste("PLS algorithm:\n", select, " for variable selection, ",
        fit, " for model fitting\n", sep = ""))

    switch(x$probMethod,
           softmax = cat("The softmax function was used to compute class probabilities.\n"),
           Bayes = cat("Bayes rule was used to compute class probabilities.\n"))

    cat(paste("\nSPLS chose ", length(A), " variables among ",
        p, " variables\n\n", sep = ""))
    cat("Selected variables: \n")
    if (!is.null(xAnames)) {
        for (i in 1:length(A)) {
            cat(paste(xAnames[i], "\t", sep = ""))
            if (i%%5 == 0) {
                cat("\n")
            }
        }
    }
    else {
        for (i in 1:length(A)) {
            cat(paste(A[i], "\t", sep = ""))
            if (i%%5 == 0) {
                cat("\n")
            }
        }
    }
    cat("\n")
}
