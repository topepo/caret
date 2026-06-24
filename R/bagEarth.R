#' Bagged Earth
#'
#' @aliases bagEarth print.bagEarth bagEarth.default bagEarth.formula
#' @description A bagging wrapper for multivariate adaptive regression splines
#'   (MARS) via the `earth` function
#'
#' @template param-formula-data
#' @param x matrix or data frame of 'x' values for examples.
#' @param y matrix or data frame of numeric values outcomes.
#'
#' @param B the number of bootstrap samples
#' @param summary a function with a single argument specifying how the bagged
#'   predictions should be summarized
#' @param keepX a logical: should the original training data be kept?
#' @param \dots arguments passed to the `earth` function
#'
#' @details The function computes a Earth model for each bootstap sample.
#'
#' @return A list with elements:
#'
#' * `fit`: a list of `B` Earth fits
#' * `B`: the number of bootstrap samples
#' * `call`: the function call
#' * `x`: either `NULL` or the value of `x`, depending on the value of `keepX`
#' * `oob`: a matrix of performance estimates for each bootstrap sample
#' @references J. Friedman, ``Multivariate Adaptive Regression Splines'' (with
#'   discussion) (1991).  Annals of Statistics, 19/1, 1-141.
#'
#' @author Max Kuhn (`bagEarth.formula` is based on Ripley's `nnet.formula`)
#'
#' @seealso [earth::earth()], [predict.bagEarth()]
#'
#' @examplesIf !caret:::is_cran_check()
#' library(mda)
#' library(earth)
#' data(trees)
#' fit1 <- earth(x = trees[,-3], y = trees[,3])
#' set.seed(2189)
#' fit2 <- bagEarth(x = trees[,-3], y = trees[,3], B = 10)
#'
#' @keywords regression
#'
#' @export
"bagEarth" <-
  function(x, ...) {
    UseMethod("bagEarth")
  }

#' @rdname bagEarth
#' @importFrom stats predict
#' @export
"bagEarth.default" <-
  function(x, y, weights = NULL, B = 50, summary = mean, keepX = TRUE, ...) {
    requireNamespaceQuietStop("earth")
    if (!isNamespaceLoaded("earth")) {
      attachNamespace("earth")
    }
    funcCall <- match.call(expand.dots = TRUE)
    if (!is.matrix(x)) {
      x <- as.matrix(x)
    }
    if (!is.factor(y)) {
      if (!is.vector(y)) {
        y <- as.vector(y)
      }
      if (!is.vector(y)) {
        y <- y[, 1]
      }
    }
    if (is.null(weights)) {
      weights <- rep(1, dim(x)[1])
    }

    if (is.factor(y)) {
      lev <- levels(y)
      theDots <- list(...)
      if (all(names(theDots) != "glm")) {
        stop("must declare a binomal glm using the glm argument to earth")
      }
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
        fit <- earth::earth(x = subX, y = subY, weights = subW, ...)
      }
      fit$index <- index
      fit
    }

    oobFoo <- function(fit, x, y, lev) {
      index <- fit$index
      subX <- x[-index, , drop = FALSE]
      subY <- y[-index]
      predY <-
        if (is.null(fit$levels)) {
          predict(fit, subX)
        } else {
          predict(fit, subX, type = "class")
        }
      postResample(predY, subY)
    }

    btSamples <- createResample(y, times = B)
    btFits <- lapply(btSamples, foo, x = x, y = y, w = weights, ...)
    oobList <- lapply(btFits, oobFoo, x = x, y = y, lev = lev)
    oob <-
      matrix(unlist(oobList), ncol = length(oobList[[1]]), byrow = TRUE)
    colnames(oob) <- names(oobList[[1]])
    if (keepX) {
      x <- x
    } else {
      x <- NULL
    }
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
#' @importFrom stats contrasts model.matrix model.response model.weights na.omit
#' @export
"bagEarth.formula" <-
  function(
    formula,
    data = NULL,
    B = 50,
    summary = mean,
    keepX = TRUE,
    ...,
    subset,
    weights = NULL,
    na.action = na.omit
  ) {
    funcCall <- match.call(expand.dots = TRUE)

    if (!inherits(formula, "formula")) {
      stop("method is only for formula objects")
    }
    m <- match.call(expand.dots = FALSE)
    mIndex <- match(
      c("formula", "data", "subset", "weights", "na.action"),
      names(m),
      0
    )
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
    if (xint > 0) {
      x <- x[, -xint, drop = FALSE]
    }

    out <- bagEarth.default(
      x,
      y,
      w,
      B = B,
      summary = summary,
      keepX = keepX,
      ...
    )
    out$call <- funcCall
    out
  }


#' Predicted values based on bagged Earth and FDA models
#'
#' @aliases predict.bagEarth
#' @param object Object of class inheriting from `bagEarth`
#' @param newdata An optional data frame or matrix in which to look for
#'   variables with which to predict.  If omitted, the fitted values are used
#'   (see note below).
#' @param type The type of prediction. For bagged [earth::earth()] regression
#'   model, `type = "response"` will produce a numeric vector of the usual
#'   model predictions. [earth::earth()] also allows the user to fit
#'   generalized linear models. In this case, `type = "response"` produces the
#'   inverse link results as a vector. In the case of a binomial generalized
#'   linear model, `type = "response"` produces a vector of probabilities,
#'   `type = "class"` generates a factor vector and `type = "prob"` produces a
#'   two-column matrix with probabilities for both classes (averaged across the
#'   individual models). Similarly, for bagged [mda::fda()] models, `type =
#'   "class"` generates a factor vector and `type = "probs"` outputs a matrix
#'   of class probabilities.
#' @param \dots not used
#' @return A vector of predictions (for regression or `type = "class"`) or a
#'   data frame of class probabilities. By default, when the model predicts a
#'   number, a vector of numeric predictions is returned. When a classification
#'   model is used, the default prediction is a factor vector of classes.
#' @note If the predictions for the original training set are needed, there are
#'   two ways to calculate them. First, the original data set can be predicted
#'   by each bagged earth model. Secondly, the predictions from each bootstrap
#'   sample could be used (but are more likely to overfit). If the original
#'   call to `bagEarth` or `bagFDA` had `keepX = TRUE`, the first method is
#'   used, otherwise the values are calculated via the second method.
#' @author Max Kuhn
#' @seealso [bagEarth()]
#' @keywords regression
#' @export
#' @examplesIf !caret:::is_cran_check()
#'
#' data(trees)
#' ## out of bag predictions vs just re-predicting the training set
#' set.seed(655)
#' fit1 <- bagEarth(Volume ~ ., data = trees, keepX = TRUE)
#' set.seed(655)
#' fit2 <- bagEarth(Volume ~ ., data = trees, keepX = FALSE)
#' hist(predict(fit1) - predict(fit2))
#'
#' @export predict.bagEarth
"predict.bagEarth" <-
  function(object, newdata = NULL, type = NULL, ...) {
    if (is.null(type)) {
      type <- if (all(is.na(object$levels))) {
        "response"
      } else {
        "class"
      }
    }
    if (!any(type %in% c("response", "class", "prob"))) {
      stop("type must be either response, class or prob", call. = FALSE)
    }
    requireNamespaceQuietStop("earth")
    ## get oob predictions
    getTrainPred <- function(x) {
      oobIndex <- seq(along.with = x$fitted.values)
      oobIndex <- oobIndex[!(oobIndex %in% unique(x$index))]
      data.frame(pred = x$fitted.values[oobIndex], sample = oobIndex)
    }

    if (is.null(newdata) & !is.null(object$x)) {
      newdata <- object$x
    }

    if (is.null(newdata)) {
      pred <- lapply(object$fit, getTrainPred)
      pred <- rbind.fill(pred)
      out <-
        ddply(pred, .(sample), function(x) {
          object$summary(x$pred)
        })$V1
    } else {
      pred <- lapply(
        object$fit,
        function(x, y) {
          if (is.null(x$glm.list)) {
            predict(x, newdata = y)
          } else {
            predict(x, newdata = y, type = "response")
          }
        },
        y = newdata
      )
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
print.bagEarth <- function(x, ...) {
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  if (!is.null(x$x)) {
    cat(
      "Data:\n   # variables:\t",
      dim(x$x)[2],
      "\n   # samples:\t",
      dim(x$x)[1],
      "\n"
    )
  }
  if (x$weights) {
    cat("case weights used\n")
  }
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
#' @return a list with elements:
#'
#' * `modelInfo`: a matrix with the number of model terms and variables used
#' * `oobStat`: a summary of the out-of-bag statistics
#' * `bmarsCall`: the original call to `bagEarth`
#' @author Max Kuhn
#' @keywords manip
#' @export
#' @examplesIf !caret:::is_cran_check()
#'
#' data(trees)
#' set.seed(9655)
#' fit <- bagEarth(trees[, -3], trees[, 3])
#' summary(fit)
#'
#' @export summary.bagEarth
"summary.bagEarth" <-
  function(object, ...) {
    requireNamespaceQuietStop("earth")
    oobStat <- apply(object$oob, 2, function(x) {
      quantile(x, probs = c(0, 0.025, .25, .5, .75, .975, 1))
    })

    numTerms <- unlist(lapply(object$fit, function(x) length(x$selected.terms)))
    numVar <- unlist(lapply(
      object$fit,
      function(x) {
        imp <- rownames(earth::evimp(x, trim = FALSE))
        imp <- imp[!grepl("-unused", imp)]
        length(imp)
      }
    ))
    modelInfo <- cbind(numTerms, numVar)
    colnames(modelInfo) <- c("Num Terms", "Num Variables")
    out <- list(
      modelInfo = modelInfo,
      oobStat = oobStat,
      bagEarthCall = object$call
    )
    class(out) <- "summary.bagEarth"
    out
  }

#' @importFrom stats quantile
#' @export
"print.summary.bagEarth" <-
  function(x, digits = max(3, getOption("digits") - 3), ...) {
    cat("\nCall:\n", deparse(x$bagEarthCall), "\n\n", sep = "")

    oobStat <-
      apply(x$oob, 2, function(x) {
        quantile(x, probs = c(0, 0.025, .5, .975, 1))
      })
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
  if (!all(is.na(lvl))) {
    if (length(lvl) == 2) {
      x <- lapply(x, function(x) cbind(x, 1 - x))
    }
    x <- simplify2array(x)
    colnames(x) <- lvl
    out <- apply(x, c(1, 2), summ)
    out <- t(apply(out, 1, function(x) x / sum(x)))
    out <- as.data.frame(out, stringsAsFactors = TRUE)
  } else {
    # regression
    out <- matrix(unlist(x), ncol = length(x))
    out <- apply(out, 1, summ, na.rm = TRUE)
    out <- as.vector(out)
  }
  out
}
