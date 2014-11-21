# add model averaging?
# check predict method with formula interface
# how to handle variable imp?

pcaNNet <- function (x, ...)
   UseMethod("pcaNNet")


# this is a near copy of nnet.formula
pcaNNet.formula <- function (formula, data, weights, ...,
                             thresh = .99,
                             subset, na.action, contrasts = NULL) 
{

    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval.parent(m$data))) 
        m$data <- as.data.frame(data)
    m$... <- m$contrasts <- NULL
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

    res <- pcaNNet.default(x, y, weights = w, thresh = thresh, ...)
    res$terms <- Terms
    res$coefnames <- colnames(x)
    res$call <- match.call()
    res$na.action <- attr(m, "na.action")
    res$contrasts <- cons
    res$xlevels <- .getXlevels(Terms, m)
    class(res) <- c("pcaNNet.formula", "pcaNNet")
    res
}

pcaNNet.default <- function(x, y, thresh = .99, ...)
  {
    requireNamespaceQuietStop("nnet")

    # check for no variance data
    isZV <- apply(x, 2,
                  function(u) length(unique(u)) < 2)
    if(any(isZV))
      {
        x <- x[,!isZV, drop = FALSE]
        xNames <- colnames(x)
      } else xNames <- NULL
    
    # get pca
    pp <- preProcess(x, "pca", thresh = thresh)
    x <- predict(pp, x)

    # check for factors

    if(is.factor(y))
      {
        classLev <- levels(y)
        y <- class2ind(y)
      } else classLev <- NULL

        
    # fit nnet
    modelFit <- nnet::nnet(x, y, ...)
    modelFit$lev <- classLev  
    
    # return results
    out <- list(model = modelFit,
                pc = pp,
                names = xNames)
    class(out) <- "pcaNNet"
    out
  }


#predict.pcaNNet <- function(object, newdata, ...)
#  {
#    library(nnet)
#    
#    if(is.null(newdata)) stop("provide newdata")
#
#    if(!is.null(object$names))
#      {
#        newdata <- newdata[, object$names, drop = FALSE]
#      }
#    x <- predict(object$pc, newdata)
#    predict(object$model, x, ...)
#  }

print.pcaNNet <- function (x, ...) 
{
  cat("Neural Network Model with PCA Pre-Processing\n\n")
  
  cat("Created from", x$pc$dim[1], "samples and", x$pc$dim[2], "variables\n")
  cat("PCA needed", x$pc$numComp, "components to capture", 
      round(x$pc$thresh * 100, 2), "percent of the variance\n\n")

  print(x$model)
  cat("\n")
  invisible(x)
}

predict.pcaNNet <- function(object, newdata, type = c("raw", "class"), ...)
  {
    requireNamespaceQuietStop("nnet")
    if (!inherits(object, "pcaNNet")) 
      stop("object not of class \"pcaNNet\"")
    if (missing(newdata))
      {
        if(is.null(object$model$lev))
           {
             return(fitted(object$model))
           } else {
             scores <- fitted(object$model)
             classes <- colnames(scores)[apply(scores, 1, which.max)]
             classes <- factor(as.character(classes), levels = object$model$lev)
             if(type[1]== "raw") return(scores) else return(classes)
           }
      }  else {
        if (inherits(object, "pcaNNet.formula")) {
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
      }

    if(!is.null(object$names))
      {
        x <- x[, object$names, drop = FALSE]
      }
    x <- predict(object$pc, x)
    predict(object$model, x, type = type, ...)
  }

