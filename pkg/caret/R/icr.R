
icr <- function (x, ...) UseMethod("icr")
                                    
icr.formula <- function (formula, data, weights, ...,              
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

    res <- icr.default(x, y, weights = w, thresh = thresh, ...)
    res$terms <- Terms
    res$coefnames <- colnames(x)
    res$na.action <- attr(m, "na.action")
    res$contrasts <- cons
    res$xlevels <- .getXlevels(Terms, m)
    class(res) <- c("icr.formula", "icr")
    res
}

icr.default <- function(x, y, ...)
  {
    xNames <- colnames(x)
    pp <- preProcess(x, "ica", ...)
    x <- predict(pp, x)

    if(is.factor(y)) stop("y must be numeric")

    data <- if(is.data.frame(x)) x else as.data.frame(x)
    data$y <- y
    
    modelFit <- lm(y ~ ., data = data)

    out <- list(model = modelFit,
                ica = pp,
                dim = dim(x),
                n.comp = list(...)$n.comp,
                names = xNames)
    class(out) <- "icr"
    out
  }



print.icr <- function (x, digits = max(3, getOption("digits") - 3), ...) 
{
  cat("Independent Component Regression\n\n")
  
  cat("Created from", x$dim[1], "samples and", x$dim[2], "variables\n\n")

  if (length(coef(x$model))) {
        cat("Coefficients:\n")
        print.default(
                      format(
                             coef(x$model),
                             digits = digits),
                      print.gap = 2, 
            quote = FALSE)
    }
    else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}

predict.icr <- function(object, newdata, ...)
  {
  loadNamespace("fastICA")
    if (!inherits(object, "icr")) stop("object not of class \"icr\"")
    if (missing(newdata))
      {
        return(fitted(object$model))
      }  else {
        if (inherits(object, "icr.formula")) {
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
    if(!is.data.frame(x)) x <- as.data.frame(x)
    x <- predict(object$ica, x)
    predict(object$model, x, ...)
  }

