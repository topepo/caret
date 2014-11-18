"knn3" <- function(x, ...)   UseMethod("knn3")

knn3.default <- function(x, ...)
{
   if(!any(class(x) %in% "formula"))  stop("knn3 only implemented for formula objects")
}

knn3.formula <- function (formula, data, subset, na.action, k = 5, ...) 
{
    cl <- match.call()
    if (missing(formula) ||
        (length(formula) != 3) ||
        (length(attr(terms(formula[-2], data = data), "term.labels")) < 1) ||
        (length(attr(terms(formula[-3], data = data), "term.labels")) != 1)) 
        stop("formula missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    m$... <- NULL
    m$k <- NULL
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    y <- model.extract(m, "response")
    x <- model.matrix(Terms, m)
    xvars <- as.character(attr(Terms, "variables"))[-1]
    if ((yvar <- attr(Terms, "response")) > 0) 
        xvars <- xvars[-yvar]
    xlev <- if (length(xvars) > 0) {
        xlev <- lapply(m[xvars], levels)
        xlev[!sapply(xlev, is.null)]
    }
    xint <- match("(Intercept)", colnames(x), nomatch = 0)
    if (xint > 0) 
        x <- x[, -xint, drop = FALSE]
    RET <- list(learn = list(y = y, X = x))
    RET$k <- k
    RET$terms <- Terms
    RET$call <- match.call(expand.dots = TRUE)
    RET$contrasts <- attr(x, "contrasts")
    RET$xlevels <- xlev
    RET$theDots <- list(...)
    attr(RET, "na.message") <- attr(m, "na.message")
    if (!is.null(attr(m, "na.action"))) 
        RET$na.action <- attr(m, "na.action")
    class(RET) <- "knn3"
    RET
}

knn3.data.frame <- function(x, y, k = 5, ...)
{
  x <- as.matrix(x)
  out <- knn3(x, y = y, k = k, ...)
  call <- match.call(expand.dots = TRUE)
  out$call <- call
  out
}

knn3.matrix <- function(x, y, k = 5, ...)
{
    if(!is.matrix(x)) x <- as.matrix(x)
    if(!is.factor(y)) stop("y must be a factor")
    RET <- list(learn = list(y = y, X = x))
    RET$k <- k
    RET$terms <- NULL
    RET$call <- match.call(expand.dots = TRUE)
    RET$contrasts <- NULL
    RET$xlevels <- NULL
    RET$theDots <- list(...)    
     class(RET) <- "knn3"
    RET
}

print.knn3 <- function (x, ...) 
{
   cat(x$k, "-nearest neighbor classification model\n", sep = "")
   printCall(x$call)
   cat("Training set class distribution:\n")
   print(table(x$learn$y))

   cat("\n")
   invisible(x)
}

predict.knn3 <- function (object, newdata, type = c("prob", "class"), ...) 
{
    type <- match.arg(type)
    if (!inherits(object, "knn3")) 
        stop("object not of class knn3")
    if (!is.null(Terms <- object$terms)) {
        if (missing(newdata)) 
            newdata <- model.frame(object)
        else {
            newdata <- model.frame(as.formula(delete.response(Terms)), 
                newdata, na.action = function(x) x, xlev = object$xlevels)
        }
        x <- model.matrix(delete.response(Terms), newdata, contrasts = object$contrasts)
        xint <- match("(Intercept)", colnames(x), nomatch = 0)
        if (xint > 0) 
            x <- x[, -xint, drop = FALSE]
    }
    else {
        x <- as.matrix(newdata)
    }
    
    argList <- list(
      train = object$learn$X,
      test = x,
      cl = object$learn$y,
      k = object$k)

    if(length(object$theDots) == 0) object$theDots <- list(prob = TRUE)
    if(any(names(object$theDots) == "prob")) object$theDots$prob <- TRUE

    argList <- c(argList, object$theDots)  
    
    RET <- do.call(
      "knn3Train", 
      argList)
      
    if (type == "prob")
    {
       return(attr(RET, "prob"))
    }  else {
      RET <- factor(RET, levels = levels(object$learn$y))
      return(RET)
    }
}

