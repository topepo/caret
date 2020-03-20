#' Bagged FDA
#' @aliases bagFDA print.bagFDA bagFDA.default bagFDA.formula
#'
#' @description A bagging wrapper for flexible discriminant analysis (FDA) using multivariate adaptive regression splines (MARS) basis functions
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
#'
#' @param keepX a logical: should the original training data be kept?
#'
#' @param \dots arguments passed to the \code{mars} function
#'
#' @details The function computes a FDA model for each bootstap sample.
#'
#' @return
#' A list with elements
#' \item{fit }{a list of \code{B} FDA fits}
#' \item{B }{the number of bootstrap samples}
#' \item{call }{the function call}
#' \item{x }{either \code{NULL} or the value of \code{x}, depending on the
#'   value of \code{keepX}}
#' \item{oob}{a matrix of performance estimates for each bootstrap sample}
#'
#' @references J. Friedman, ``Multivariate Adaptive Regression Splines'' (with discussion) (1991).  Annals of Statistics, 19/1, 1-141.
#'
#' @author Max Kuhn (\code{bagFDA.formula} is based on Ripley's \code{nnet.formula})
#'
#' @seealso \code{\link[mda]{fda}}, \code{\link{predict.bagFDA}}
#'
#' @examples
#' library(mlbench)
#' library(earth)
#' data(Glass)
#'
#' set.seed(36)
#' inTrain <- sample(1:dim(Glass)[1], 150)
#'
#' trainData <- Glass[ inTrain, ]
#' testData  <- Glass[-inTrain, ]
#'
#'
#' set.seed(3577)
#' baggedFit <- bagFDA(Type ~ ., trainData)
#' confusionMatrix(data = predict(baggedFit, testData[, -10]),
#'                 reference = testData[, 10])
#'
#' @keywords regression
#'
#' @export
"bagFDA" <-
function(x, ...)
   UseMethod("bagFDA")

#' @rdname bagFDA
#' @method bagFDA default
#' @importFrom stats predict
#' @export
"bagFDA.default" <-
function(x, y, weights = NULL, B = 50, keepX = TRUE, ...)
{
  requireNamespaceQuietStop("mda")
  requireNamespaceQuietStop("earth")
   if(!is.matrix(x)) x <- as.matrix(x)
   if(!is.vector(y) & !is.factor(y)) y <- as.vector(y)
   if(!is.vector(y) & !is.factor(y)) y <- factor(y[,1])
   if(is.null(weights)) weights <- rep(1, dim(x)[1])
   foo <- function(index, x, y, w, ...)
   {
      subX <- x[index,, drop = FALSE]
      subY <- y[index]
      tmp <- as.data.frame(subX, stringsAsFactors = FALSE)
      tmp$.outcome <- subY
      if(!is.null(w)) subW <- w[index]
      fit <- if(is.null(w))
        mda::fda(.outcome ~., data = tmp, method = earth::earth, ...) else
          mda::fda(.outcome ~., data = tmp, method = earth::earth, weights = subW, ...)
      fit$index <- index
      fit
   }

   oobFoo <- function(fit, x, y)
   {
      index <- fit$index
      subX <- x[-index,, drop = FALSE]
      subY <- y[-index]
      predY <- predict(fit, subX)
      postResample(predY, subY)
   }

   btSamples <- createResample(y, times = B)
   btFits <- lapply(btSamples, foo, x = x, y = y, w = weights, ...)
   oobList <- lapply(btFits, oobFoo, x = x, y = y)
   oob <- matrix(unlist(oobList), ncol = length(oobList[[1]]), byrow = TRUE)
   colnames(oob) <- names(oobList[[1]])
   if(keepX) x <- x else x <- NULL
   structure(list(fit = btFits, B = B, oob = oob, x = x, levels = levels(y),
                  weights = !is.null(weights), dots = list(...)), class = "bagFDA")
}

#' @rdname bagFDA
#' @method bagFDA formula
#' @importFrom stats contrasts model.matrix model.response model.weights na.omit
#' @export
"bagFDA.formula" <-
function (formula, data = NULL, B = 50, keepX = TRUE, ..., subset, weights = NULL, na.action = na.omit)
{

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

   out <- bagFDA.default(x = x, y = y, weights = weights, B = B, keepX = keepX, ...)
   out
}

#' @rdname bagFDA
#' @method print bagFDA
#' @export
"print.bagFDA" <-
function (x, ...)
{
    if(!is.null(x$x))cat("Data:\n   # variables:\t", dim(x$x)[2], "\n   # samples:\t", dim(x$x)[1], "\n")
    cat(
      "\nModel:",
      "\n   B:        \t", x$B,
      "\n   dimension:\t", x$fit[[1]]$dimension,
      "\n")
    if(x$weights) cat("case weights used\n")

    cat("\n")
    invisible(x)
}




#' @rdname predict.bagEarth
#' @importFrom stats predict
#' @export
"predict.bagFDA" <-
function(object, newdata = NULL, type = "class", ...)
{
  requireNamespaceQuietStop("mda")
  requireNamespaceQuietStop("earth")
   getTrainPred <- function(x)
     {
       oobIndex <- 1:nrow(x$fit$fitted.values)
       oobIndex <- oobIndex[!(oobIndex %in% unique(x$index))]
       tmp <- predict(x, type = "posterior")[oobIndex,,drop = FALSE]
       rownames(tmp) <- 1:nrow(tmp)
       out <- data.frame(pred = tmp,
                         sample = oobIndex,
                         check.rows = FALSE)
       colnames(out)[1:ncol(tmp)] <- names(x$prior)
       out
     }

   if(is.null(newdata) & !is.null(object$x)) newdata <- object$x

   if(is.null(newdata))
     {
       pred <- lapply(object$fit, getTrainPred)
    } else {
       pred <- lapply(object$fit,
                      function(x, y)
                      {
                        tmp <- predict(x, newdata = y, type = "posterior")
                        nms <- colnames(tmp)
                        tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)
                        names(tmp) <- nms
                        tmp$sample <- 1:nrow(tmp)
                        tmp
                      },
                      y = newdata)
     }
   pred <- rbind.fill(pred)
   out <- ddply(pred, .(sample),
                function(x) colMeans(x[,seq(along = object$levels)], na.rm = TRUE))
   out <- out[,-1,drop = FALSE]
   rownames(out) <- rownames(newdata)
   predClass <- object$levels[apply(out, 1, which.max)]
   predClass <- factor(predClass, levels = object$levels)
   switch(type, class = predClass, probs = out, posterior = out)
}

#' @rdname summary.bagEarth
#' @method summary bagFDA
#' @importFrom stats quantile
#' @export
"summary.bagFDA" <-
function(object, ...)
{

   oobStat <- apply(object$oob, 2, function(x) quantile(x, probs = c(0, 0.025, .5, .975, 1)))

   numTerms <- unlist(lapply(object$fit, function(x) length(x$fit$selected.terms)))
   numVar <- unlist(lapply(
      object$fit,
      function(x)
      {
         sum(
            apply(
               x$fit$dirs,
               2,
               function(u) any(u != 0)))
      }))
   modelInfo <- cbind(numTerms, numVar)
   colnames(modelInfo) <- c("Num Terms", "Num Variables")
   out <- list(modelInfo = modelInfo, oobStat = oobStat)
   class(out) <- "summary.bagFDA"
   out
}

#' @export
"print.summary.bagFDA" <-
function(x, digits = max(3, getOption("digits") - 3), ...)
{
   oobStat <- apply(x$oob, 2, function(x) quantile(x, probs = c(0, 0.025, .25, .5, .75, .975, 1)))
   cat("Out of bag statistics:\n\n")
   print(x$oobStat, digits = digits)
   cat("\nModel Selection Statistics:\n\n")
   print(summary(x$modelInfo))
   cat("\n")
}

