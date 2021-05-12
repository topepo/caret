#' Bagged Earth
#'
#' @aliases bagEarth print.bagEarth bagEarth.default bagEarth.formula
#' @description A bagging wrapper for multivariate adaptive regression
#' splines (MARS) via the \code{earth} function
#'
#'
#' @param formula A formula of the form \code{y ~ x1 + x2 + ...}
#' @param x matrix or data frame of 'x' values for examples.
#' @param y matrix or data frame of numeric values outcomes.
#' @param weights (case) weights for each example - if missing defaults to 1.
#' @param data Data frame from which variables specified in  'formula' are
#'         preferentially to be taken.
#' @param subset An index vector specifying the cases to be used in the
#'         training sample.  (NOTE: If given, this argument must be
#'         named.)
#' @param na.action A function to specify the action to be taken if 'NA's are
#'         found. The default action is for the procedure to fail.  An
#'         alternative is na.omit, which leads to rejection of cases
#'         with missing values on any required variable.  (NOTE: If
#'         given, this argument must be named.)
#'
#' @param B the number of bootstrap samples
#' @param summary a function with a single argument specifying how the bagged predictions should be summarized
#' @param keepX a logical: should the original training data be kept?
#' @param \dots arguments passed to the \code{earth} function
#'
#' @details The function computes a Earth model for each bootstap sample.
#'
#' @return
#' A list with elements
#' \item{fit }{a list of \code{B} Earth fits}
#' \item{B }{the number of bootstrap samples}
#' \item{call }{the function call}
#' \item{x }{either \code{NULL} or the value of \code{x}, depending on the
#'   value of \code{keepX}}
#' \item{oob}{a matrix of performance estimates for each bootstrap sample}
#'
#' @references J. Friedman, ``Multivariate Adaptive Regression Splines'' (with
#' discussion) (1991).  Annals of Statistics, 19/1, 1-141.
#'
#' @author Max Kuhn (\code{bagEarth.formula} is based on Ripley's \code{nnet.formula})
#'
#' @seealso \code{\link[earth]{earth}}, \code{\link{predict.bagEarth}}
#'
#' @examples \dontrun{
#' library(mda)
#' library(earth)
#' data(trees)
#' fit1 <- earth(x = trees[,-3], y = trees[,3])
#' set.seed(2189)
#' fit2 <- bagEarth(x = trees[,-3], y = trees[,3], B = 10)
#' }
#'
#' @keywords regression
#'
#' @export
"bagEarth" <-
  function(x, ...)
  UseMethod("bagEarth")

#' @rdname bagEarth
#' @method bagEarth default
#' @importFrom stats predict
#' @export
"bagEarth.default" <-
  function(x,
           y,
           weights = NULL,
           B = 50,
           summary = mean,
           keepX = TRUE,
           ...) {
    requireNamespaceQuietStop("earth")
    if (!isNamespaceLoaded("earth"))
      attachNamespace("earth")
    funcCall <- match.call(expand.dots = TRUE)
    if (!is.matrix(x))
      x <- as.matrix(x)
    if (!is.factor(y)) {
      if (!is.vector(y))
        y <- as.vector(y)
      if (!is.vector(y))
        y <- y[, 1]
    }
    if (is.null(weights))
      weights <- rep(1, dim(x)[1])

    if (is.factor(y)) {
      lev <- levels(y)
      theDots <- list(...)
      if (all(names(theDots) != "glm"))
        stop("must declare a binomal glm using the glm argument to earth")
    } else {
      lev <- NA
    }

    foo <- function(index, x, y, w, ...) {
      subX <- x[index, , drop = FALSE]
      subY <- y[index]

      if (is.null(w)) {
        fit <- earth::earth(x = subX, y = subY, ...)
      } else {
        subW <- weights[index]
        fit <- earth::earth(x = subX,
                            y = subY,
                            weights = subW,
                            ...)
      }
      fit$index <- index
      fit
    }

    oobFoo <- function(fit, x, y, lev) {
      index <- fit$index
      subX <- x[-index, , drop = FALSE]
      subY <- y[-index]
      predY <-
        if (is.null(fit$levels))
          predict(fit, subX)
      else
        predict(fit, subX, type = "class")
      postResample(predY, subY)
    }

    btSamples <- createResample(y, times = B)
    btFits <- lapply(btSamples,
                     foo,
                     x = x,
                     y = y,
                     w = weights,
                     ...)
    oobList <- lapply(btFits,
                      oobFoo,
                      x = x,
                      y = y,
                      lev = lev)
    oob <-
      matrix(unlist(oobList),
             ncol = length(oobList[[1]]),
             byrow = TRUE)
    colnames(oob) <- names(oobList[[1]])
    if (keepX)
      x <- x
    else
      x <- NULL
    structure(
      list(
        fit = btFits,
        B = B,
        oob = oob,
        summary = summary,
        call = funcCall,
        levels = lev,
        x = x,
        weights = !is.null(weights)
      ),
      class = "bagEarth"
    )
  }

#' @rdname bagEarth
#' @method bagEarth formula
#' @importFrom stats contrasts model.matrix model.response model.weights na.omit
#' @export
"bagEarth.formula" <-
  function (formula, data = NULL, B = 50, summary = mean, keepX = TRUE, ..., subset, weights = NULL, na.action = na.omit)
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
  x <- model.matrix(Terms, m)
  cons <- attr(x, "contrast")
  xint <- match("(Intercept)", colnames(x), nomatch = 0)
  if (xint > 0)  x <- x[, -xint, drop = FALSE]

  out <- bagEarth.default(x, y, w, B = B, summary = summary, keepX = keepX, ...)
  out$call <- funcCall
  out
}



#' Predicted values based on bagged Earth and FDA models
#'
#'
#' @aliases predict.bagEarth
#' @param object Object of class inheriting from \code{bagEarth}
#' @param newdata An optional data frame or matrix in which to look for
#' variables with which to predict.  If omitted, the fitted values are used
#' (see note below).
#' @param type The type of prediction. For bagged \code{\link[earth]{earth}}
#' regression model, \code{type = "response"} will produce a numeric vector of
#' the usual model predictions. \code{\link[earth]{earth}} also allows the user
#' to fit generalized linear models. In this case, \code{type = "response"}
#' produces the inverse link results as a vector. In the case of a binomial
#' generalized linear model, \code{type = "response"} produces a vector of
#' probabilities, \code{type = "class"} generates a factor vector and
#' \code{type = "prob"} produces a two-column matrix with probabilities for
#' both classes (averaged across the individual models). Similarly, for bagged
#' \code{\link[mda]{fda}} models, \code{type = "class"} generates a factor
#' vector and \code{type = "probs"} outputs a matrix of class probabilities.
#' @param \dots not used
#' @return A vector of predictions (for regression or \code{type = "class"})
#'  or a data frame of class probabilities. By default, when the model
#'  predicts a number, a vector of numeric predictions is returned. When
#'  a classification model is used, the default prediction is a factor vector
#'  of classes.
#' @note If the predictions for the original training set are needed, there are
#' two ways to calculate them. First, the original data set can be predicted by
#' each bagged earth model. Secondly, the predictions from each bootstrap
#' sample could be used (but are more likely to overfit). If the original call
#' to \code{bagEarth} or \code{bagFDA} had \code{keepX = TRUE}, the first
#' method is used, otherwise the values are calculated via the second method.
#' @author Max Kuhn
#' @seealso \code{\link{bagEarth}}
#' @keywords regression
#' @method predict bagEarth
#' @export
#' @examples
#'
#' \dontrun{
#' data(trees)
#' ## out of bag predictions vs just re-predicting the training set
#' set.seed(655)
#' fit1 <- bagEarth(Volume ~ ., data = trees, keepX = TRUE)
#' set.seed(655)
#' fit2 <- bagEarth(Volume ~ ., data = trees, keepX = FALSE)
#' hist(predict(fit1) - predict(fit2))
#' }
#'
#' @export predict.bagEarth
"predict.bagEarth" <-
  function(object,
           newdata = NULL,
           type = NULL,
           ...) {
    if(is.null(type)) {
      type <- if (all(is.na(object$levels)))
        "response"
      else
        "class"
    }
    if (!any(type %in% c("response", "class", "prob")))
      stop("type must be either response, class or prob",
           call. = FALSE)
    requireNamespaceQuietStop("earth")
    ## get oob predictions
    getTrainPred <- function(x) {
      oobIndex <- seq(along = x$fitted.values)
      oobIndex <- oobIndex[!(oobIndex %in% unique(x$index))]
      data.frame(pred = x$fitted.values[oobIndex],
                 sample = oobIndex)
    }

    if (is.null(newdata) & !is.null(object$x))
      newdata <- object$x

    if (is.null(newdata)) {
      pred <- lapply(object$fit, getTrainPred)
      pred <- rbind.fill(pred)
      out <-
        ddply(pred, .(sample), function(x)
          object$summary(x$pred))$V1
    } else {
      pred <- lapply(object$fit,
                     function(x, y) {
                       if (is.null(x$glm.list))
                         predict(x, newdata = y)
                       else
                         predict(x, newdata = y, type = "response")
                     },
                     y = newdata)
      out <- aggregate_pred(pred, object$levels, object$summary)
    }

    if (type == "class") {
      out <- object$levels[apply(out, 1, which.max)]
      out <- factor(out, levels = object$levels)
    }
    out
  }

#' @rdname bagEarth
#' @export
print.bagEarth <- function (x, ...) {
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  if (!is.null(x$x))
    cat("Data:\n   # variables:\t",
        dim(x$x)[2],
        "\n   # samples:\t",
        dim(x$x)[1],
        "\n")
  if (x$weights)
    cat("case weights used\n")
  cat("\nB:", x$B, "\n")
  invisible(x)
}



#' Summarize a bagged earth or FDA fit
#'
#' The function shows a summary of the results from a bagged earth model
#'
#' The out-of-bag statistics are summarized, as well as the distribution of the
#' number of model terms and number of variables used across all the bootstrap
#' samples.
#'
#' @aliases summary.bagEarth summary.bagFDA
#' @param object an object of class "bagEarth" or "bagFDA"
#' @param \dots optional arguments (not used)
#' @return a list with elements \item{modelInfo}{a matrix with the number of
#' model terms and variables used} \item{oobStat }{a summary of the out-of-bag
#' statistics} \item{bmarsCall }{the original call to \code{bagEarth}}
#' @author Max Kuhn
#' @keywords manip
#' @method summary bagEarth
#' @export
#' @examples
#'
#' \dontrun{
#' data(trees)
#' set.seed(9655)
#' fit <- bagEarth(trees[,-3], trees[3])
#' summary(fit)
#' }
#'
#' @export summary.bagEarth
"summary.bagEarth" <-
  function(object, ...) {
  requireNamespaceQuietStop("earth")
  oobStat <- apply(object$oob, 2, function(x) quantile(x, probs = c(0, 0.025, .25, .5, .75, .975, 1)))

  numTerms <- unlist(lapply(object$fit, function(x) length(x$selected.terms)))
  numVar <- unlist(lapply(
                          object$fit,
                          function(x) {
                          imp <- rownames(earth::evimp(x, trim = FALSE))
                          imp <- imp[!grepl("-unused", imp)]
                          length(imp)
                          }))
  modelInfo <- cbind(numTerms, numVar)
  colnames(modelInfo) <- c("Num Terms", "Num Variables")
  out <- list(modelInfo = modelInfo, oobStat = oobStat, bagEarthCall = object$call)
  class(out) <- "summary.bagEarth"
  out
}

#' @importFrom stats quantile
#' @export
"print.summary.bagEarth" <-
  function(x, digits = max(3, getOption("digits") - 3), ...) {
    cat("\nCall:\n", deparse(x$bagEarthCall), "\n\n", sep = "")

    oobStat <-
      apply(x$oob, 2, function(x)
        quantile(x, probs = c(0, 0.025, .5, .975, 1)))
    cat("Out of bag statistics:\n\n")
    print(x$oobStat, digits = digits)
    cat("\nModel Selection Statistics:\n\n")
    print(summary(x$modelInfo))
    cat("\n")
  }

# Go from a list of predictions (of various forms) to a summarized
# vector or matrix depending on prediction type
aggregate_pred <- function(x, lvl, summ) {
  # classification first
  if(!all(is.na(lvl))) {
    if(length(lvl) == 2) {
      x <- lapply(x, function(x) cbind(x, 1 - x))
    }
    x <- simplify2array(x)
    colnames(x) <- lvl
    out <- apply(x, c(1,2), summ)
    out <- t(apply(out, 1, function(x) x/sum(x)))
    out <- as.data.frame(out, stringsAsFactors = TRUE)
  } else {
    # regression
    out <- matrix(unlist(x), ncol = length(x))
    out <- apply(out, 1, summ, na.rm = TRUE)
    out <- as.vector(out)
  }
  out
}
