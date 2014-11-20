
avNNet <- function (x, ...)
  UseMethod("avNNet")


## this is a near copy of nnet.formula
avNNet.formula <- function (formula, data, weights, ...,
                            repeats = 5,
                            bag= FALSE,
                            allowParallel = TRUE,
                            subset, na.action, contrasts = NULL) 
{
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval.parent(m$data))) 
    m$data <- as.data.frame(data)
##  bag <- m$bag
##  repeats <- m$repeats
  m$... <- m$contrasts <- m$bag <- m$repeats <- m$allowParallel <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  xint <- match("(Intercept)", colnames(x), nomatch = 0)
  if (xint > 0) 
    x <- x[, -xint, drop = FALSE]
  w <- model.weights(m)
  if (length(w) == 0) 
    w <- rep(1, nrow(x))
  y <- model.response(m)
  
  res <- avNNet.default(x, y,
                        weights = w,
                        repeats = repeats,
                        bag = bag,
                        allowParallel = allowParallel,
                        ...)
  res$terms <- Terms
  res$coefnames <- colnames(x)
  res$call <- match.call()
  res$na.action <- attr(m, "na.action")
  res$contrasts <- cons
  res$xlevels <- .getXlevels(Terms, m)
  class(res) <- c("avNNet.formula", "avNNet")
  res
}

avNNet.default <- function(x, y, repeats = 5, bag = FALSE, allowParallel = TRUE, ...)
  {
    requireNamespaceQuietStop("nnet")
    ## check for factors
    ## this is from nnet.formula

    ind <- seq(along = y)
    if(is.factor(y))
      {
        classLev <- levels(y)
        y <- class2ind(y)
      } else classLev <- NULL

    if(is.matrix(y)) classLev <- colnames(y)
    
    theDots <- list(...)

    ## to avoid a "no visible binding for global variable ‘i’" warning:
    i <- NULL
    `%op%` <-  if(allowParallel)  `%dopar%` else  `%do%`
     mods <- foreach(i = 1:repeats,
                     .verbose = FALSE,
                     .packages = "caret",
                     .errorhandling = "stop") %op%  
    {
      if(any(names(theDots) == "trace"))
        {
          if(theDots$trace) cat("\nFitting Repeat", i, "\n\n")
        } else cat("Fitting Repeat", i, "\n\n")
      if(bag)  ind <- sample(1:nrow(x))
      thisMod <- if(is.null(classLev)) nnet::nnet(x[ind,,drop = FALSE], y[ind], ...) else nnet::nnet(x[ind,,drop = FALSE], y[ind,], ...)
      thisMod$lev <- classLev
      thisMod
    }
    
    ## return results
    out <- list(model = mods,
                repeats = repeats,
                bag = bag,
                names = colnames(x))
    class(out) <- "avNNet"
    out
  }

print.avNNet <- function (x, ...) 
{
  cat("Model Averaged Neural Network with", x$repeats, "Repeats", ifelse(x$bag, "and Bagging", ""), "\n\n")
  print(x$model[[1]])
  cat("\n")
  invisible(x)
}

predict.avNNet <- function(object, newdata, type = c("raw", "class", "prob"), ...)
  {
    library(nnet)
    if (!inherits(object, "avNNet")) 
      stop("object not of class \"avNNet\"")
    if (missing(newdata))
      {
        if(is.null(object$model[[1]]$lev))
          {
            out <- lapply(object$model, fitted.values)
            out <- do.call("cbind", out)
            return(apply(out, 1, mean))
          } else {
            for(i in 1:object$repeats)
              {
                rawTmp <- fitted.values(object$model[[i]])
                rawTmp <- t(apply(rawTmp, 1, function(x) exp(x)/sum(exp(x))))
                scores <- if(i == 1) rawTmp else scores + rawTmp
              }
            scores <- scores/object$repeats
            classes <- colnames(scores)[apply(scores, 1, which.max)]
            classes <- factor(as.character(classes), levels = object$model[[1]]$lev)
            if(type[1]== "raw") out <- scores
            if(type[1]== "class")  out <- (classes)
            if(type[1]== "prob")  out <- t(apply(scores, 1, function(x) x/sum(x)))
          }
      }  else {
        if (inherits(object, "avNNet.formula")) {
          newdata <- as.data.frame(newdata)
          rn <- row.names(newdata)
          Terms <- delete.response(object$terms)
          m <- model.frame(Terms, newdata, na.action = na.omit, 
                           xlev = object$xlevels)
          if (!is.null(cl <- attr(Terms, "dataClasses"))) 
            .checkMFClasses(cl, m)
          keep <- match(row.names(m), rn)
          x <- model.matrix(Terms, m, contrasts = object$contrasts)
          xint <- match("(Intercept)", colnames(x), nomatch = 0)
          if (xint > 0) 
            x <- x[, -xint, drop = FALSE]
        }
        else {
          if (is.null(dim(newdata))) 
            dim(newdata) <- c(1, length(newdata))
          x <- as.matrix(newdata)
          if (any(is.na(x))) 
            stop("missing values in 'x'")
          keep <- 1:nrow(x)
          rn <- rownames(x)
        }
        if(!is.null(object$names))  x <- x[, object$names, drop = FALSE]
        if(is.null(object$model[[1]]$lev))
          {
            out <- lapply(object$model, predict, newdata = x)
            out <- do.call("cbind", out)
            return(apply(out, 1, mean))
          } else {
            for(i in 1:object$repeats)
              {
                scores <- if(i == 1) predict(object$model[[i]], newdata = x) else scores + predict(object$model[[i]], newdata = x)

              }
            scores <- scores/object$repeats
            classes <- colnames(scores)[apply(scores, 1, which.max)]
            classes <- factor(as.character(classes), levels = object$model[[1]]$lev)
            if(type[1]== "raw") out <- scores
            if(type[1]== "class")  out <- (classes)
            if(type[1]== "prob")  out <- t(apply(scores, 1, function(x) x/sum(x)))
          }
        
      }
    out

  }

