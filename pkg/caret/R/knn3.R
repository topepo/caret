#' k-Nearest Neighbour Classification
#'
#' $k$-nearest neighbour classification that can return class votes for all
#' classes.
#'
#' \code{knn3} is essentially the same code as \code{\link[ipred]{ipredknn}}
#' and \code{knn3Train} is a copy of \code{\link[class]{knn}}. The underlying C
#' code from the \code{class} package has been modified to return the vote
#' percentages for each class (previously the percentage for the winning class
#' was returned).
#'
#' @aliases knn3 knn3.formula knn3.matrix knn3.data.frame knn3Train
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} is
#' the response variable and \code{rhs} a set of predictors.
#' @param data optional data frame containing the variables in the model
#' formula.
#' @param subset optional vector specifying a subset of observations to be
#' used.
#' @param na.action function which indicates what should happen when the data
#' contain \code{NA}s.
#' @param k number of neighbours considered.
#' @param x a matrix of training set predictors
#' @param y a factor vector of training set classes
#' @param ... additional parameters to pass to \code{knn3Train}. However,
#' passing \code{prob = FALSE} will be over-ridden.
#' @param train matrix or data frame of training set cases.
#' @param test matrix or data frame of test set cases. A vector will be
#' interpreted as a row vector for a single case.
#' @param cl factor of true classifications of training set
#' @param l minimum vote for definite decision, otherwise \code{doubt}. (More
#' precisely, less than \code{k-l} dissenting votes are allowed, even if
#' \code{k} is increased by ties.)
#' @param prob If this is true, the proportion of the votes for each class are
#' returned as attribute \code{prob}.
#' @param use.all controls handling of ties. If true, all distances equal to
#' the \code{k}th largest are included. If false, a random selection of
#' distances equal to the \code{k}th is chosen to use exactly \code{k}
#' neighbours.
#' @return An object of class \code{knn3}. See \code{\link{predict.knn3}}.
#' @author \code{\link[class]{knn}} by W. N. Venables and B. D. Ripley and
#' \code{\link[ipred]{ipredknn}} by Torsten.Hothorn
#' <Torsten.Hothorn@@rzmail.uni-erlangen.de>, modifications by Max Kuhn and
#' Andre Williams
#' @keywords multivariate
#' @examples
#'
#' irisFit1 <- knn3(Species ~ ., iris)
#'
#' irisFit2 <- knn3(as.matrix(iris[, -5]), iris[,5])
#'
#' data(iris3)
#' train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
#' test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
#' cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
#' knn3Train(train, test, cl, k = 5, prob = TRUE)
#'
#' @export knn3
"knn3" <- function(x, ...)   UseMethod("knn3")

knn3.default <- function(x, ...)
{
   if(!inherits(x, "formula"))  stop("knn3 only implemented for formula objects")
}

#' @rdname knn3
#' @method knn3 formula
#' @importFrom stats model.matrix terms model.extract
#' @export
knn3.formula <- function (formula, data, subset, na.action, k = 5, ...)
{

    if (missing(formula) ||
        (length(formula) != 3) ||
        (length(attr(terms(formula[-2], data = data), "term.labels")) < 1) ||
        (length(attr(terms(formula[-3], data = data), "term.labels")) != 1))
        stop("formula missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data, stringsAsFactors = TRUE)
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

#' @rdname knn3
#' @method knn3 data.frame
#' @export
knn3.data.frame <- function(x, y, k = 5, ...)
{
  x <- as.matrix(x)
  out <- knn3(x, y = y, k = k, ...)
  out
}

#' @rdname knn3
#' @method knn3 matrix
#' @export
knn3.matrix <- function(x, y, k = 5, ...)
{
    if(!is.matrix(x)) x <- as.matrix(x)
    if(!is.factor(y)) stop("y must be a factor")
    RET <- list(learn = list(y = y, X = x))
    RET$k <- k
    RET$terms <- NULL
    RET$contrasts <- NULL
    RET$xlevels <- NULL
    RET$theDots <- list(...)
     class(RET) <- "knn3"
    RET
}

#' @rdname knn3
#' @method print knn3
#' @export
print.knn3 <- function (x, ...)
{
   cat(x$k, "-nearest neighbor model\n", sep = "")
   cat("Training set outcome distribution:\n")
   if(is.factor(x$learn$y)) {
     print(table(x$learn$y))
   } else print(summary(x$learn$y))

   cat("\n")
   invisible(x)
}



#' Predictions from k-Nearest Neighbors
#'
#' Predict the class of a new observation based on k-NN.
#'
#' This function is a method for the generic function \code{\link{predict}} for
#' class \code{knn3}. For the details see \code{\link{knn3}}. This is
#' essentially a copy of \code{\link[ipred]{predict.ipredknn}}.
#'
#' @param object object of class \code{knn3}.
#' @param newdata a data frame of new observations.
#' @param type return either the predicted class or the proportion of the votes
#' for the winning class.
#' @param ... additional arguments.
#' @return Either the predicted class or the proportion of the votes for each
#' class.
#' @author \code{\link[ipred]{predict.ipredknn}} by Torsten.Hothorn
#' <Torsten.Hothorn@@rzmail.uni-erlangen.de>
#' @keywords multivariate
#' @method predict knn3
#' @export
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

#' @rdname knn3
#' @export
knn3Train <- function(train, test, cl, k=1, l=0, prob = TRUE, use.all=TRUE)
{
  train <- as.matrix(train)
  if(is.null(dim(test))) dim(test) <- c(1, length(test))
  test <- as.matrix(test)
        if(any(is.na(train)) || any(is.na(test)) || any(is.na(cl)))
            stop("no missing values are allowed")
  p <- ncol(train)
  ntr <- nrow(train)
  if(length(cl) != ntr) stop("'train' and 'class' have different lengths")
  if(ntr < k) {
            warning(gettextf("k = %d exceeds number %d of patterns", k, ntr),
                    domain = NA)
     k <- ntr
  }
  if (k < 1)
            stop(gettextf("k = %d must be at least 1", k), domain = NA)
  nte <- nrow(test)
  if(ncol(test) != p) stop("dims of 'test' and 'train differ")
  clf <- as.factor(cl)
  nc <- max(unclass(clf))
  Z <- .C("knn3",
    as.integer(k),
    as.integer(l),
    as.integer(ntr),
    as.integer(nte),
    as.integer(p),
    as.double(train),
    as.integer(unclass(clf)),
    as.double(test),
    integer(nc+1),
    as.integer(nc),
    as.integer(FALSE),
    as.integer(use.all),
    all_vote=double(as.integer(nte*nc))

    )

  classProbs <- matrix(Z$all_vote,nrow=nte,ncol=nc,byrow=TRUE)
  colnames(classProbs)<-sort(unique(clf))

   bestClass <- function(x)
   {
      out <- which(x == max(x))
      if(length(out) > 1) out <- sample(out, 1)
      out
   }

   res <- colnames(classProbs)[apply(classProbs, 1, bestClass)]

   votes <- apply(classProbs * k, 1, max)
   inDoubt <- (votes < l)
   if(any(inDoubt)) res[inDoubt] <- NA

   if (prob) attr(res, "prob") <- classProbs
   res
}
