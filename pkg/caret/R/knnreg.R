#' k-Nearest Neighbour Regression
#'
#' $k$-nearest neighbour regression that can return the average value for the
#' neighbours.
#'
#' \code{knnreg} is similar to \code{\link[ipred]{ipredknn}} and
#' \code{knnregTrain} is a modification of \code{\link[class]{knn}}. The
#' underlying C code from the \code{class} package has been modified to return
#' average outcome.
#'
#' @aliases knnreg knnregTrain knnreg.formula knnreg.default knnreg.matrix
#' knnreg.data.frame knnreg
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} is
#' the response variable and \code{rhs} a set of predictors.
#' @param data optional data frame containing the variables in the model
#' formula.
#' @param subset optional vector specifying a subset of observations to be
#' used.
#' @param na.action function which indicates what should happen when the data
#' contain \code{NA}s.
#' @param k number of neighbours considered.
#' @param x a matrix or data frame of training set predictors.
#' @param y a numeric vector of outcomes.
#' @param ... additional parameters to pass to \code{knnregTrain}.
#' @param train matrix or data frame of training set cases.
#' @param test matrix or data frame of test set cases. A vector will be
#' interpreted as a row vector for a single case.
#' @param use.all controls handling of ties. If true, all distances equal to
#' the \code{k}th largest are included. If false, a random selection of
#' distances equal to the \code{k}th is chosen to use exactly \code{k}
#' neighbours.
#' @return An object of class \code{knnreg}. See \code{\link{predict.knnreg}}.
#' @author \code{\link[class]{knn}} by W. N. Venables and B. D. Ripley and
#' \code{\link[ipred]{ipredknn}} by Torsten.Hothorn
#' <Torsten.Hothorn@@rzmail.uni-erlangen.de>, modifications by Max Kuhn and
#' Chris Keefer
#' @keywords multivariate
#' @examples
#'
#' data(BloodBrain)
#'
#' inTrain <- createDataPartition(logBBB, p = .8)[[1]]
#'
#' trainX <- bbbDescr[inTrain,]
#' trainY <- logBBB[inTrain]
#'
#' testX <- bbbDescr[-inTrain,]
#' testY <- logBBB[-inTrain]
#'
#' fit <- knnreg(trainX, trainY, k = 3)
#'
#' plot(testY, predict(fit, testX))
#'
#' @export knnreg
"knnreg" <- function(x, ...)   UseMethod("knnreg")

#' @rdname knnreg
#' @method knnreg default
#' @export
knnreg.default <- function(x, ...)
{
  if(!any(class(x) %in% "formula"))  stop("knnreg only implemented for formula objects")
}

#' @rdname knnreg
#' @method knnreg formula
#' @importFrom stats model.matrix terms model.extract
#' @export
knnreg.formula <- function (formula, data, subset, na.action, k = 5, ...)
{
  if (missing(formula) ||
      (length(formula) != 3) ||
      (length(attr(terms(formula[-2],  data = data), "term.labels")) < 1) ||
      (length(attr(terms(formula[-3],  data = data), "term.labels")) != 1))
    stop("formula missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data, stringsAsFactors = FALSE)
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
  class(RET) <- "knnreg"
  RET
}

#' @rdname knnreg
#' @method knnreg matrix
#' @export
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

#' @rdname knnreg
#' @method knnreg data.frame
#' @export
knnreg.data.frame <- function(x, y, k = 5, ...)
{
  x <- as.data.frame(x, stringsAsFactors = TRUE)
  if(!is.numeric(y)) stop("y must be numeric")
  RET <- list(learn = list(y = y, X = x))
  RET$k <- k
  RET$terms <- NULL
  RET$contrasts <- NULL
  RET$theDots <- list(...)
  class(RET) <- "knnreg"
  RET
}

#' @rdname knnreg
#' @method print knnreg
#' @export
print.knnreg <- function (x, ...)
{
  cat(x$k, "-nearest neighbor regression model\n", sep = "")
  invisible(x)
}



#' Predictions from k-Nearest Neighbors Regression Model
#'
#' Predict the outcome of a new observation based on k-NN.
#'
#' This function is a method for the generic function \code{\link{predict}} for
#' class \code{knnreg}. For the details see \code{\link{knnreg}}. This is
#' essentially a copy of \code{\link[ipred]{predict.ipredknn}}.
#'
#' @aliases predict.knnreg
#' @param object object of class \code{knnreg}.
#' @param newdata a data frame or matrix of new observations.
#' @param ... additional arguments.
#' @return a numeric vector
#' @author Max Kuhn, Chris Keefer, adapted from \code{\link[class]{knn}} and
#' \code{\link[ipred]{predict.ipredknn}}
#' @keywords multivariate
#' @method predict knnreg
#' @export
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

#' @rdname knnreg
#' @export
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
