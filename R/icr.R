#' @export
icr <- function (x, ...) UseMethod("icr")



#' Independent Component Regression
#'
#' Fit a linear regression model using independent components
#'
#' This produces a model analogous to Principal Components Regression (PCR) but
#' uses Independent Component Analysis (ICA) to produce the scores. The user
#' must specify a value of `n.comp` to pass to [fastICA::fastICA()].
#'
#' The function [preProcess()] to produce the ICA scores for the original data
#' and for `newdata`.
#'
#' @aliases icr.formula icr.default icr predict.icr
#' @template param-formula-data
#' @param contrasts a list of contrasts to be used for some or all of the
#'   factors appearing as variables in the model formula.
#' @param \dots arguments passed to [fastICA::fastICA()]
#' @param x matrix or data frame of `x` values for examples.
#' @param y matrix or data frame of target values for examples.
#' @param object an object of class `icr` as returned by `icr`.
#' @param newdata matrix or data frame of test examples.
#' @return
#'
#' For `icr`, a list with elements:
#'
#' * `model`: the results of [stats::lm()] after the ICA transformation
#' * `ica`: pre-processing information
#' * `n.comp`: number of ICA components
#' * `names`: column names of the original data
#' @author Max Kuhn
#' @seealso [fastICA::fastICA()], [preProcess()], [stats::lm()]
#' @keywords multivariate
#' @examples
#' 
#' data(BloodBrain)
#' 
#' icrFit <- icr(bbbDescr, logBBB, n.comp = 5)
#' 
#' icrFit
#' 
#' predict(icrFit, bbbDescr[1:5, ])
#' @export
icr.formula <- function (formula, data, weights, ...,
                         subset, na.action, contrasts = NULL)
{
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval.parent(m$data)))
        m$data <- as.data.frame(data, stringsAsFactors = TRUE)
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

#' @rdname icr.formula
#' @importFrom stats predict lm
#' @export
icr.default <- function(x, y, ...)
  {
    xNames <- colnames(x)
    pp <- preProcess(x, "ica", ...)
    x <- predict(pp, x)

    if(is.factor(y)) stop("y must be numeric")

    data <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
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


#' @importFrom stats4 coef
#' @export
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

#' @rdname icr.formula
#' @importFrom stats .checkMFClasses delete.response model.frame model.matrix predict na.omit fitted
#' @export
predict.icr <- function(object, newdata, ...)
  {
  loadNamespace("fastICA")
    if (!inherits(object, "icr")) stop("object not of class \"icr\"")
    if (missing(newdata))
      {
        return(fitted(object$model))
      }  else {
        if (inherits(object, "icr.formula")) {
          newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
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
          keep <- seq_len(nrow(x))
          rn <- rownames(x)
        }
      }

    if(!is.null(object$names))
      {
        x <- x[, object$names, drop = FALSE]
      }
    if(!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
    x <- predict(object$ica, x)
    predict(object$model, x, ...)
  }
