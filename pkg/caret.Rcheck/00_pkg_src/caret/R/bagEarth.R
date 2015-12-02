"bagEarth" <-
  function(x, ...)
  UseMethod("bagEarth")


"bagEarth.default" <-
  function(x, y, weights = NULL, B = 50, summary = mean, keepX = TRUE, ...)
{
  requireNamespaceQuietStop("earth")
  funcCall <- match.call(expand.dots = TRUE)
  if(!is.matrix(x)) x <- as.matrix(x)
  if(!is.factor(y))
    {
      if(!is.vector(y)) y <- as.vector(y)   
      if(!is.vector(y)) y <- y[,1]
    }
  if(is.null(weights)) weights <- rep(1, dim(x)[1])

  if(is.factor(y))
    {
      lev <- levels(y)
      theDots <- list(...)
      if(all(names(theDots) != "glm")) stop("must declare a binomal glm using the glm argument to earth")
      #if(theDots$glm$family$family != "binomial") stop("must use binomial glm for factors")
    } else {
      lev <- NA
    }

  foo <- function(index, x, y, w, ...)
    {
      subX <- x[index,, drop = FALSE]
      subY <- y[index]
      subW <- weights[index]      
      fit <- earth::earth(subX, subY, subW, ...)
      fit$index <- index
      fit
    }
  
  oobFoo <- function(fit, x, y, lev)
    {
      index <- fit$index
      subX <- x[-index,, drop = FALSE]
      subY <- y[-index]
      ## todo
      predY <- if(is.null(fit$levels)) predict(fit, subX) else predict(fit, subX, type = "class")
      postResample(predY, subY)   
    }
  
  btSamples <- createResample(y, times = B)
  btFits <- lapply(btSamples, foo, x = x, y = y, w = weights, ...)
  oobList <- lapply(btFits, oobFoo, x = x, y = y, lev = lev)
  oob <- matrix(unlist(oobList), ncol = length(oobList[[1]]), byrow = TRUE)
  colnames(oob) <- names(oobList[[1]])
  if(keepX) x <- x else x <- NULL
  structure(
            list(fit = btFits,
                 B = B,
                 oob = oob,
                 summary = summary,
                 call = funcCall,
                 levels = lev,
                 x = x),
            class = "bagEarth")
}

"bagEarth.formula" <-
  function (formula, data = NULL, B = 50, summary = mean, keepX = TRUE, ..., subset, weights, na.action = na.omit) 
{
  funcCall <- match.call(expand.dots = TRUE)
  
  if (!inherits(formula, "formula")) 
    stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)  
  mIndex <- match(c("formula", "data", "subset", "weights", "na.action"), names(m), 0)
  m <- m[c(1, mIndex)]
  m$... <- NULL
  m$na.action <- na.action
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  y <- model.response(m)
  w <- model.weights(m)
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  xint <- match("(Intercept)", colnames(x), nomatch = 0)
  if (xint > 0)  x <- x[, -xint, drop = FALSE]
  
  out <- bagEarth.default(x, y, w, B = B, summary = summary, keepX = keepX, ...)
  out$call <- funcCall
  out
}

"predict.bagEarth" <-
  function(object, newdata = NULL, type = "response", ...)
{
  if(!any(type %in% c("response", "class", "prob")))
    stop("type must be either response, class or prob")
  requireNamespaceQuietStop("earth")
  ## get oob predictions
  getTrainPred <- function(x)
    {
      oobIndex <- seq(along = x$fitted.values)
      oobIndex <- oobIndex[!(oobIndex %in% unique(x$index))]
      data.frame(pred = x$fitted.values[oobIndex],
                 sample = oobIndex)
    }

  if(is.null(newdata) & !is.null(object$x)) newdata <- object$x

  if(is.null(newdata))
    {
      pred <- lapply(object$fit, getTrainPred)
      pred <- rbind.fill(pred)
      out <- ddply(pred, .(sample), function(x) object$summary(x$pred))$V1
    } else {

      pred <- lapply(object$fit,
                     function(x, y)
                     {
                       if(is.null(x$glm.list)) predict(x, newdata = y) else predict(x, newdata = y, type = "response")
                     },
                     y = newdata
                     )
      out <- matrix(unlist(pred), ncol = object$B)
      out <- apply(out, 1, object$summary, na.rm = TRUE)
    }


  if(!all(is.na(object$levels)))
    {
      if(type == "prob")
        {
          out <- cbind(1 - out, out)
          colnames(out) <- object$levels
        } else {
          if(type == "class")
            {
              out <- factor(ifelse(out > .5,
                                   object$levels[2],
                                   object$levels[1]),
                            levels = object$levels)
            }
        }
    }
  out

}


print.bagEarth <- function (x, ...) 
{
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  if(!is.null(x$x))cat("Data:\n   # variables:\t", dim(x$x)[2], "\n   # samples:\t", dim(x$x)[1], "\n")
  cat("\nB:", x$B,"\n")
  invisible(x)
}

"summary.bagEarth" <-
  function(object, ...)
{
  requireNamespaceQuietStop("earth")
  oobStat <- apply(object$oob, 2, function(x) quantile(x, probs = c(0, 0.025, .25, .5, .75, .975, 1)))

  numTerms <- unlist(lapply(object$fit, function(x) length(x$selected.terms)))
  numVar <- unlist(lapply(
                          object$fit, 
                          function(x) {
                          imp <- rownames(earth::evimp(x, trim = FALSE))
                          imp <- imp[!grepl("-unused", imp)]
                          imp  
                          }))
  modelInfo <- cbind(numTerms, numVar)
  colnames(modelInfo) <- c("Num Terms", "Num Variables")
  out <- list(modelInfo = modelInfo, oobStat = oobStat, bagEarthCall = object$call)
  class(out) <- "summary.bagEarth"
  out
}

"print.summary.bagEarth" <-
  function(x, digits = max(3, getOption("digits") - 3), ...)
{
  cat("\nCall:\n", deparse(x$bagEarthCall), "\n\n", sep = "")

  oobStat <- apply(x$oob, 2, function(x) quantile(x, probs = c(0, 0.025, .5, .975, 1)))
  cat("Out of bag statistics:\n\n")
  print(x$oobStat, digits = digits)
  cat("\nModel Selection Statistics:\n\n")
  print(summary(x$modelInfo))
  cat("\n")
}

