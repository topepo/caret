"knnreg" <- function(x, ...)   UseMethod("knnreg")

knnreg.default <- function(x, ...)
{
  if(!any(class(x) %in% "formula"))  stop("knnreg only implemented for formula objects")
}

knnreg.formula <- function (formula, data, subset, na.action, k = 5, ...) 
{
  if (missing(formula) ||
      (length(formula) != 3) ||
      (length(attr(terms(formula[-2],  data = data), "term.labels")) < 1) ||
      (length(attr(terms(formula[-3],  data = data), "term.labels")) != 1)) 
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
  RET$contrasts <- attr(x, "contrasts")
  RET$xlevels <- xlev
  RET$theDots <- list(...)
  attr(RET, "na.message") <- attr(m, "na.message")
  if (!is.null(attr(m, "na.action"))) 
    RET$na.action <- attr(m, "na.action")
  class(RET) <- "knn3"
  RET
}

knnreg.matrix <- function(x, y, k = 5, ...)
{
  if(!is.matrix(x)) x <- as.matrix(x)
  if(!is.numeric(y)) stop("y must be numeric")
  RET <- list(learn = list(y = y, X = x))
  RET$k <- k
  RET$terms <- NULL
  RET$contrasts <- NULL
  RET$theDots <- list(...)    
  class(RET) <- "knnreg"
  RET
}


knnreg.data.frame <- function(x, y, k = 5, ...)
{
  x <- as.data.frame(x)
  if(!is.numeric(y)) stop("y must be numeric")
  RET <- list(learn = list(y = y, X = x))
  RET$k <- k
  RET$terms <- NULL
  RET$contrasts <- NULL
  RET$theDots <- list(...)    
  class(RET) <- "knnreg"
  RET
}

print.knnreg <- function (x, ...) 
{
  cat(x$k, "-nearest neighbor regression model\n", sep = "")
  invisible(x)
}

predict.knnreg <- function (object, newdata, ...) 
{
  if (!inherits(object, "knnreg")) 
    stop("object not of class knnreg")
  if (!is.null(Terms <- object$terms)) {
    if (missing(newdata)) 
      newdata <- model.frame(object)
    else {
      newdata <- model.frame(as.formula(delete.response(Terms)), 
                             newdata)
    }
    x <- model.matrix(delete.response(Terms), newdata, contrasts = object$contrasts)
    xint <- match("(Intercept)", colnames(x), nomatch = 0)
    if (xint > 0) 
      x <- x[, -xint, drop = FALSE]
  }
  else {
    x <- as.matrix(newdata)
  }
  
  argList <- list(train = object$learn$X,
                  test = x,
                  y = object$learn$y,
                  k = object$k)

  
  RET <- do.call("knnregTrain", argList)
  
  RET
}


knnregTrain <- function(train, test, y, k = 5, use.all=TRUE)
{
  train <- as.matrix(train)
  if(is.null(dim(test))) dim(test) <- c(1, length(test))
  test <- as.matrix(test)
  if(any(is.na(train)) || any(is.na(test)) || any(is.na(y)))
    stop("no missing values are allowed")
  p <- ncol(train)
  ntr <- nrow(train)
  if(length(y) != ntr) stop("'train' and 'class' have different lengths")
  if(ntr < k) {
    warning(gettextf("k = %d exceeds number %d of patterns", k, ntr),
            domain = NA)
    k <- ntr
  }
  if (k < 1)
    stop(gettextf("k = %d must be at least 1", k), domain = NA)
  nte <- nrow(test)
  if(ncol(test) != p) stop("dims of 'test' and 'train differ")

  Z <- .C("knn3reg",
          as.integer(k),
          as.integer(ntr),
          as.integer(nte),
          as.integer(p),
          as.double(train),
          as.double(y),
          as.double(test),
          double(nte),
          as.integer(FALSE),
          as.integer(use.all))

  Z[[8]]
}
