#' A General Framework For Bagging
#' @aliases bag.default bag bagControl predict.bag ldaBag plsBag nbBag ctreeBag svmBag nnetBag
#'
#' @description \code{bag} provides a framework for bagging classification or regression models. The user can provide their own functions for model building, prediction and aggregation of predictions (see Details below).
#'
#'
#' @param x a matrix or data frame of predictors
#' @param y a vector of outcomes
#' @param B the number of bootstrap samples to train over.
#' @param bagControl a list of options.
#' @param \dots arguments to pass to the model function
#' @param fit a function that has arguments \code{x}, \code{y} and \code{...} and produces a model object #' that can later be used for prediction. Example functions are found in \code{ldaBag}, \code{plsBag}, #' \code{nbBag}, \code{svmBag} and \code{nnetBag}.
#' @param predict a function that generates predictions for each sub-model. The function should have #' arguments \code{object} and \code{x}. The output of the function can be any type of object (see the #' example below where posterior probabilities are generated. Example functions are found in \code{ldaBag}#' , \code{plsBag}, \code{nbBag}, \code{svmBag} and \code{nnetBag}.)
#' @param aggregate a function with arguments \code{x} and \code{type}. The function that takes the output #' of the \code{predict} function and reduces the bagged predictions to a single prediction per sample. #' the \code{type} argument can be used to switch between predicting classes or class probabilities for #' classification models. Example functions are found in \code{ldaBag}, \code{plsBag}, \code{nbBag}, #' \code{svmBag} and \code{nnetBag}.
#' @param downSample logical: for classification, should the data set be randomly sampled so that each #' class has the same number of samples as the smallest class?
#' @param oob logical: should out-of-bag statistics be computed and the predictions retained?
#' @param allowParallel a parallel backend is loaded and available, should the function use it?
#' @param vars an integer. If this argument is not \code{NULL}, a random sample of size \code{vars} is taken of the predictors in each bagging iteration. If \code{NULL}, all predictors are used.
#' @param object an object of class \code{bag}.
#' @param newdata a matrix or data frame of samples for prediction. Note that this argument must have a non-null value
#' @param digits minimal number of \emph{significant digits}.
#'
#' @details The function is basically a framework where users can plug in any model in to assess
#' the effect of bagging. Examples functions can be found in \code{ldaBag}, \code{plsBag}
#' , \code{nbBag}, \code{svmBag} and \code{nnetBag}.
#' Each has elements \code{fit}, \code{pred} and \code{aggregate}.
#'
#' One note: when \code{vars} is not \code{NULL}, the sub-setting occurs prior to the \code{fit} and #' \code{predict} functions are called. In this way, the user probably does not need to account for the #' change in predictors in their functions.
#'
#' When using \code{bag} with \code{\link{train}}, classification models should use \code{type = "prob"} #' inside of the \code{predict} function so that \code{predict.train(object, newdata, type = "prob")} will #' work.
#'
#' If a parallel backend is registered, the \pkg{foreach} package is used to train the models in parallel.
#'
#' @return
#'   \code{bag} produces an object of class \code{bag} with elements
#'   \item{fits }{a list with two sub-objects: the \code{fit} object has the actual model fit for that #' bagged samples and the \code{vars} object is either \code{NULL} or a vector of integers corresponding to which predictors were sampled for that model}
#'   \item{control }{a mirror of the arguments passed into \code{bagControl}}
#'   \item{call }{the call}
#'   \item{B }{the number of bagging iterations}
#'   \item{dims }{the dimensions of the training set}
#'
#' @author Max Kuhn
#'
#' @examples
#' ## A simple example of bagging conditional inference regression trees:
#' data(BloodBrain)
#'
#' ## treebag <- bag(bbbDescr, logBBB, B = 10,
#' ##                bagControl = bagControl(fit = ctreeBag$fit,
#' ##                                        predict = ctreeBag$pred,
#' ##                                        aggregate = ctreeBag$aggregate))
#'
#'
#'
#'
#' ## An example of pooling posterior probabilities to generate class predictions
#' data(mdrr)
#'
#' ## remove some zero variance predictors and linear dependencies
#' mdrrDescr <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
#' mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .95)]
#'
#' ## basicLDA <- train(mdrrDescr, mdrrClass, "lda")
#'
#' ## bagLDA2 <- train(mdrrDescr, mdrrClass,
#' ##                  "bag",
#' ##                  B = 10,
#' ##                  bagControl = bagControl(fit = ldaBag$fit,
#' ##                                          predict = ldaBag$pred,
#' ##                                          aggregate = ldaBag$aggregate),
#' ##                  tuneGrid = data.frame(vars = c((1:10)*10 , ncol(mdrrDescr))))
#'
#' @keywords models
#'
#' @export
"bag" <-
  function(x, ...)
  UseMethod("bag")


#' @rdname bag
#' @export
bagControl <- function(
  fit = NULL, predict = NULL, aggregate = NULL, downSample = FALSE,
                       oob = TRUE, allowParallel = TRUE)
  {

    list(fit = fit,
         predict = predict,
         aggregate = aggregate,
         downSample = downSample,
         oob = oob,
         allowParallel = allowParallel)
  }


#' @rdname bag
#' @method bag default
#' @export
"bag.default" <-
  function(x, y, B = 10, vars = ncol(x), bagControl = NULL,  ...)
{
  funcCall <- match.call(expand.dots = TRUE)

  if(is.null(bagControl)) stop("Please specify 'bagControl' with the appropriate functions")

   if(!is.null(vars) && vars < 1) stop("vars must be an integer > 0")

  if(bagControl$downSample & is.numeric(y)) {
      warning("down-sampling with regression... downSample changed to FALSE")
      bagControl$downSample <- FALSE
    }

  if(is.null(bagControl$fit) | is.null(bagControl$predict) |
       is.null(bagControl$aggregate)) {
    stop("The control arguments 'fit', 'predict' and 'aggregate' should have non-NULL values")
  }

  fitter <- function(index, x, y, ctrl, v, ...)
    {

      subX <- x[index,, drop = FALSE]
      subY <- y[index]

      if(!is.null(v))
        {
          if(v > ncol(x)) v <- ncol(x)
          subVars <- sample(1:ncol(subX), ceiling(v))
          subX <- subX[, subVars, drop = FALSE]
        } else subVars <- NULL

      if(ctrl$downSample)
        {
          freaks <- table(subY)
          smallFreak <- min(freaks)
          splitUp <- split(seq(along = subY), subY)
          splitUp <- lapply(splitUp,
                            sample,
                            size = smallFreak)
          keepers <- unlist(splitUp)
          subX <- subX[keepers,,drop = FALSE]
          subY <- subY[keepers]
        }
      fit <- ctrl$fit(subX, subY, ...)
      if(ctrl$oob)
        {
          pred <- ctrl$predict(fit, x[-unique(index), subVars, drop = FALSE])
          if(is.vector(pred))
            {
              out <- data.frame(pred  = pred, obs = y[-unique(index)])
            } else {
              out <- as.data.frame(pred, stringsAsFactors = TRUE)
              out$obs <- y[-unique(index)]
              if(is.factor(y) & !(any(names(out) == "pred")))
                {
                  ## Try to detect class probs and make a pred factor
                  if(all(levels(y) %in% names(out)))
                    {
                      pred <- apply(out[, levels(y)], 1, which.max)
                      pred <- factor(levels(y)[pred], levels = levels(y))
                      out$pred <- pred
                    }
                }
            }
          out$key <- paste(sample(letters, 10, replace = TRUE), collapse = "")
        } else out <- NULL

      list(fit = fit,
           vars = subVars,
           oob = out)
    }

  btSamples <- createResample(y, times = B)

  `%op%` <-  if(bagControl$allowParallel)  `%dopar%` else  `%do%`
  btFits <- foreach(iter = seq(along = btSamples),
                    .verbose = FALSE,
                    .packages = "caret",
                    .errorhandling = "stop") %op%
    fitter(btSamples[[iter]],  x = x, y = y, ctrl = bagControl, v = vars, ...)

  structure(
            list(fits = btFits,
                 control = bagControl,
                 call = funcCall,
                 B = B,
                 vars = vars,
                 smallClass = min(table(y)),
                 dims = dim(x)),
            class = "bag")

}


#' @importFrom stats contrasts model.matrix model.response model.weights na.omit
#' @export
"bag.formula" <-
  function (formula, data = NULL,..., subset, weights, na.action = na.omit)
{
  funcCall <- match.call(expand.dots = TRUE)

  if (!inherits(formula, "formula"))
    stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)
  mIndex <- match(c("formula", "data", "subset", "weights", "na.action"), names(m), 0)
  m <- m[c(1, mIndex)]
  m$... <- m$B <- m$vars <- m$bagControl <- NULL
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

  out <- bag.default(x, y, ...)
  out$call <- funcCall
  out
}

#' @rdname bag
#' @method predict bag
#' @importFrom stats predict
#' @export
"predict.bag" <-
  function(object, newdata = NULL, ...)
{

  if(is.null(newdata)) stop("please provide a data set for prediction")

  predictor <- function(obj, x, ctrl)
    {
      if(!is.null(obj$vars)) x <- x[, obj$vars, drop = FALSE]
      pred <- ctrl$predict(obj$fit, x)
    }
  btPred <- lapply(object$fit, predictor, x = newdata, ctrl = object$control)
  object$control$aggregate(btPred, ...)

}

#' @rdname bag
#' @method print bag
#' @export
print.bag <- function (x, ...)
{
  printCall(x$call)
  cat("\nB:", x$B,"\n")

  cat("Training data:", x$dims[2], "variables and", x$dims[1], "samples\n")
  cat(ifelse(is.null(x$vars) || x$dims[2] == x$vars,
             "All variables were used in each model",
             paste("Each model used", x$vars, "random",
                   ifelse(x$vars == 1, "variable", "variables"), "predictors")))
  cat('\n')
  if(x$control$downSample)
    {
      cat("Training data was down-sampled to balance the classes to",
          x$smallClass, "samples per class\n\n")
    }

  invisible(x)
}

#' @rdname bag
#' @method summary bag
#' @importFrom stats quantile
#' @export
"summary.bag" <-
  function(object, ...)
{

  hasPred <- any(names(object$fits[[1]]$oob) == "pred")
  if(object$control$oob & hasPred)
    {
      ## to avoid a 'no visible binding for global variable' warning
      key <- NULL
      oobData <- lapply(object$fits, function(x) x$oob)
      oobData <- do.call("rbind", oobData)
      oobResults <- ddply(oobData, .(key), defaultSummary)
      oobResults$key <- NULL
      oobStat <- apply(oobResults, 2,
                       function(x) quantile(x,
                                            na.rm = TRUE,
                                            probs = c(0, 0.025, .25, .5, .75, .975, 1)))
      rownames(oobStat) <- paste(format(as.numeric(format(gsub("%", "", rownames(oobStat))))),
                                 "%", sep = "")
      B <- nrow(oobResults)
    } else {
      oobStat <- NULL
      B <- NULL
    }
  out <- list(oobStat = oobStat, call = object$call, B = B)
  class(out) <- "summary.bag"
  out
}

#' @rdname bag
#' @method print summary.bag
#' @export
"print.summary.bag" <-
  function(x, digits = max(3, getOption("digits") - 3), ...)
{
  printCall(x$call)
  if(!is.null(x$oobStat))
    {
      cat("Out of bag statistics (B = ", x$B, "):\n\n", sep = "")
      print(x$oobStat, digits = digits)
    } else cat("No out of bag statistics\n")
  cat("\n")
}

#' @rdname bag
#' @importFrom stats median predict
#' @export
ldaBag <- list(fit = function(x, y, ...)
               {
                 loadNamespace("MASS")
                 MASS::lda(x, y, ...)
               },

               pred = function(object, x)
               {
                 if(!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
                 predict(object, x)$posterior
               },
               aggregate = function(x, type = "class")
               {
                 ## The class probabilities come in as a list of matrices
                 ## For each class, we can pool them then average over them

                 pooled <- x[[1]] * NA
                 n <- nrow(pooled)
                 classes <- colnames(pooled)
                 for(i in 1:ncol(pooled))
                   {
                     tmp <- lapply(x, function(y, col) y[,col], col = i)
                     tmp <- do.call("rbind", tmp)
                     pooled[,i] <- apply(tmp, 2, median)
                   }
                 pooled <- apply(pooled, 1, function(x) x/sum(x))
                 if(n != nrow(pooled)) pooled <- t(pooled)
                 if(type == "class")
                   {
                     out <- factor(classes[apply(pooled, 1, which.max)],
                                   levels = classes)
                   } else out <- as.data.frame(pooled, stringsAsFactors = TRUE)
                 out
               })

#' @rdname bag
#' @importFrom stats median predict
#' @export
plsBag <- list(fit = function(x, y,  ...)
               {
                 loadNamespace("pls")
                 caret::plsda(x, y, ...)
               },

               pred = function(object, x)
               {
                 if(!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
                 predict(object, x, type = "prob")[,,]
               },
               aggregate = function(x, type = "class")
               {

                 pooled <- x[[1]] * NA
                 classes <- colnames(pooled)
                 for(i in 1:ncol(pooled))
                   {
                     tmp <- lapply(x, function(y, col) y[,col], col = i)
                     tmp <- do.call("rbind", tmp)
                     pooled[,i] <- apply(tmp, 2, median)
                   }
                 if(type == "class")
                   {
                     out <- factor(classes[apply(pooled, 1, which.max)],
                                   levels = classes)
                   } else out <- as.data.frame(pooled, stringsAsFactors = TRUE)
                 out
               })

#' @rdname bag
#' @importFrom stats median predict
#' @export
nbBag <- list(fit = function(x, y,  ...)
               {
                 loadNamespace("klaR")
                 klaR::NaiveBayes(x, y, usekernel = TRUE, fL = 2, ...)
               },

               pred = function(object, x)
               {
                 if(!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
                 as.data.frame(predict(object, x)$posterior, stringsAsFactors = TRUE)
               },
               aggregate = function(x, type = "class")
               {
                 pooled <- x[[1]] * NA
                 classes <- colnames(pooled)
                 for(i in 1:ncol(pooled))
                   {
                     tmp <- lapply(x, function(y, col) y[,col], col = i)
                     tmp <- do.call("rbind", tmp)
                     pooled[,i] <- apply(tmp, 2, median)
                   }
                 if(type == "class")
                   {
                     out <- factor(classes[apply(pooled, 1, which.max)],
                                   levels = classes)
                   } else out <- as.data.frame(pooled, stringsAsFactors = TRUE)
                 out
               })


#' @rdname bag
#' @importFrom stats median
#' @export
ctreeBag <- list(fit = function(x, y,  ...)
                {
                  loadNamespace("party")
                  data <- as.data.frame(x, stringsAsFactors = TRUE)
                  data$y <- y
                  party::ctree(y~., data = data)
                },

                pred = function(object, x)
                {
                  if(!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
                  obsLevels <-  levels(object@data@get("response")[,1])
                  if(!is.null(obsLevels))
                    {
                      rawProbs <- party::treeresponse(object, x)
                      probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), byrow = TRUE)
                      out <- data.frame(probMatrix)
                      colnames(out) <- obsLevels
                      rownames(out) <- NULL
                    } else out <- unlist(party::treeresponse(object, x))
                  out
                },
                aggregate = function(x, type = "class")
                 {
                   if(is.matrix(x[[1]]) | is.data.frame(x[[1]]))
                     {
                       pooled <- x[[1]] & NA

                       classes <- colnames(pooled)
                       for(i in 1:ncol(pooled))
                         {
                           tmp <- lapply(x, function(y, col) y[,col], col = i)
                           tmp <- do.call("rbind", tmp)
                           pooled[,i] <- apply(tmp, 2, median)
                         }
                       if(type == "class")
                         {
                           out <- factor(classes[apply(pooled, 1, which.max)],
                                         levels = classes)
                         } else out <- as.data.frame(pooled, stringsAsFactors = TRUE)
                     } else {
                       x <- matrix(unlist(x), ncol = length(x))
                       out <- apply(x, 1, median)
                     }
                   out
                })

#' @rdname bag
#' @importFrom stats median predict
#' @export
svmBag <- list(fit = function(x, y,  ...)
                {

                  loadNamespace("kernlab")

                  out <- kernlab::ksvm(as.matrix(x), y, prob.model = is.factor(y), ...)
                  out
                },

                pred = function(object, x)
                {

                  if(is.character(lev(object)))
                    {
                      out <- predict(object, as.matrix(x), type = "probabilities")
                      colnames(out) <- lev(object)
                      rownames(out) <- NULL
                    } else out <-  predict(object, as.matrix(x))[,1]
                  out
                },
                aggregate = function(x, type = "class")
                 {
                   if(is.matrix(x[[1]]) | is.data.frame(x[[1]]))
                     {
                       pooled <- x[[1]] & NA

                       classes <- colnames(pooled)
                       for(i in 1:ncol(pooled))
                         {
                           tmp <- lapply(x, function(y, col) y[,col], col = i)
                           tmp <- do.call("rbind", tmp)
                           pooled[,i] <- apply(tmp, 2, median)
                         }
                       if(type == "class")
                         {
                           out <- factor(classes[apply(pooled, 1, which.max)],
                                         levels = classes)
                         } else out <- as.data.frame(pooled, stringsAsFactors = TRUE)
                     } else {
                       x <- matrix(unlist(x), ncol = length(x))
                       out <- apply(x, 1, median)
                     }
                   out
                })


#' @rdname bag
#' @importFrom stats median predict
#' @export
nnetBag <- list(fit = function(x, y,  ...)
                {

                  loadNamespace("nnet")
                  factorY <- is.factor(y)
                  if(factorY) y <- class2ind(y)

                  out <- nnet::nnet(x, y, linout = !factorY, trace = FALSE, ...)
                  out$classification <- factorY
                  out
                },

                pred = function(object, x)
                {

                  out <- predict(object, x, type= "raw")
                  if(object$classification)
                    {

                      colnames(out) <- colnames(object$fitted.values)
                      rownames(out) <- NULL
                    } else out <- predict(object, x, type= "raw")[,1]
                  out
                },
                aggregate = function(x, type = "class")
                 {
                   if(is.matrix(x[[1]]) | is.data.frame(x[[1]]))
                     {
                       pooled <- x[[1]] & NA

                       classes <- colnames(pooled)
                       for(i in 1:ncol(pooled))
                         {
                           tmp <- lapply(x, function(y, col) y[,col], col = i)
                           tmp <- do.call("rbind", tmp)
                           pooled[,i] <- apply(tmp, 2, median)
                         }
                       if(type == "class")
                         {
                           out <- factor(classes[apply(pooled, 1, which.max)],
                                         levels = classes)
                         } else out <- as.data.frame(pooled, stringsAsFactors = TRUE)
                     } else {
                       x <- matrix(unlist(x), ncol = length(x))
                       out <- apply(x, 1, median)
                     }
                   out
                })


