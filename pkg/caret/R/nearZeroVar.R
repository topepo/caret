#' Identification of near zero variance predictors
#' 
#' \code{nearZeroVar} diagnoses predictors that have one unique value (i.e. are
#' zero variance predictors) or predictors that are have both of the following
#' characteristics: they have very few unique values relative to the number of
#' samples and the ratio of the frequency of the most common value to the
#' frequency of the second most common value is large. \code{checkConditionalX}
#' looks at the distribution of the columns of \code{x} conditioned on the
#' levels of \code{y} and identifies columns of \code{x} that are sparse within
#' groups of \code{y}.
#' 
#' For example, an example of near zero variance predictor is one that, for
#' 1000 samples, has two distinct values and 999 of them are a single value.
#' 
#' To be flagged, first the frequency of the most prevalent value over the
#' second most frequent value (called the ``frequency ratio'') must be above
#' \code{freqCut}. Secondly, the ``percent of unique values,'' the number of
#' unique values divided by the total number of samples (times 100), must also
#' be below \code{uniqueCut}.
#' 
#' In the above example, the frequency ratio is 999 and the unique value
#' percentage is 0.0001.
#' 
#' Checking the conditional distribution of \code{x} may be needed for some
#' models, such as naive Bayes where the conditional distributions should have
#' at least one data point within a class.
#' 
#' \code{nzv} is the original version of the function.
#' 
#' @aliases nearZeroVar nzv checkResamples checkConditionalX
#' @param x a numeric vector or matrix, or a data frame with all numeric data
#' @param freqCut the cutoff for the ratio of the most common value to the
#' second most common value
#' @param uniqueCut the cutoff for the percentage of distinct values out of the
#' number of total samples
#' @param saveMetrics a logical. If false, the positions of the zero- or
#' near-zero predictors is returned. If true, a data frame with predictor
#' information is returned.
#' @param names a logical. If false, column indexes are returned. If true,
#' column names are returned.
#' @param y a factor vector with at least two levels
#' @param index a list. Each element corresponds to the training set samples in
#' \code{x} for a given resample
#' @param foreach should the \pkg{foreach} package be used for the
#' computations? If \code{TRUE}, less memory should be used.
#' @param allowParallel should the parallel processing via the \pkg{foreach}
#' package be used for the computations? If \code{TRUE}, more memory will be
#' used but execution time should be shorter.
#' @return For \code{nearZeroVar}: if \code{saveMetrics = FALSE}, a vector of
#' integers corresponding to the column positions of the problematic
#' predictors. If \code{saveMetrics = TRUE}, a data frame with columns:
#' \item{freqRatio }{the ratio of frequencies for the most common value over
#' the second most common value} \item{percentUnique }{the percentage of unique
#' data points out of the total number of data points} \item{zeroVar }{a vector
#' of logicals for whether the predictor has only one distinct value} \item{nzv
#' }{a vector of logicals for whether the predictor is a near zero variance
#' predictor}
#' 
#' For \code{checkResamples} or \code{checkConditionalX}, a vector of column
#' indicators for predictors with empty conditional distributions in at least
#' one class of \code{y}.
#' @author Max Kuhn, with speed improvements to nearZeroVar by Allan Engelhardt
#' @keywords utilities
#' @examples
#' 
#' nearZeroVar(iris[, -5], saveMetrics = TRUE)
#' 
#' data(BloodBrain)
#' nearZeroVar(bbbDescr)
#' nearZeroVar(bbbDescr, names = TRUE)
#' 
#' 
#' set.seed(1)
#' classes <- factor(rep(letters[1:3], each = 30))
#' x <- data.frame(x1 = rep(c(0, 1), 45),
#'                 x2 = c(rep(0, 10), rep(1, 80)))
#' 
#' lapply(x, table, y = classes)
#' checkConditionalX(x, classes)
#' 
#' folds <- createFolds(classes, k = 3, returnTrain = TRUE)
#' x$x3 <- x$x1
#' x$x3[folds[[1]]] <- 0
#' 
#' checkResamples(folds, x, classes)
#' 
#' 
#' 
#' @export nearZeroVar
nearZeroVar <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = FALSE, foreach = FALSE, allowParallel = TRUE) {

  if(!foreach) return(nzv(x, freqCut = freqCut, uniqueCut = uniqueCut, saveMetrics = saveMetrics, names = names))

  `%op%` <- getOper(foreach && allowParallel && getDoParWorkers() > 1)

  if(saveMetrics) {
    res <- foreach(name = colnames(x), .combine=rbind) %op% {
      r <- nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut, saveMetrics = TRUE)
      r[,"column" ] <-  name
      r
    }
    res <- res[, c(5, 1, 2, 3, 4)]
    rownames(res) <- as.character(res$column)
    res$column <- NULL
  } else {
    res <- foreach(name = colnames(x), .combine=c) %op% {
      r <- nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut, saveMetrics = FALSE)
      ## needed because either integer() or 1, r is never 0
      if (length(r) > 0 && r == 1) TRUE else FALSE
    }
    res <- which(res)
    if(names){
      res <- colnames(x)[res]
    }
  }
  res
}

#' @export
nzv <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, names = FALSE)
{
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  freqRatio <- apply(x, 2, function(data)
  {
    t <- table(data[!is.na(data)])
    if (length(t) <= 1) {
      return(0);
    }
    w <- which.max(t);
    return(max(t, na.rm=TRUE)/max(t[-w], na.rm=TRUE))
  })
  lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))
  percentUnique <- 100 * lunique / apply(x, 2, length)
  zeroVar <- (lunique == 1) | apply(x, 2, function(data) all(is.na(data)))
  if (saveMetrics)
  {
    out <- data.frame(freqRatio = freqRatio,
                      percentUnique = percentUnique,
                      zeroVar = zeroVar,
                      nzv = (freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
  }
  else {
    out <- which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
    names(out) <- NULL
    if(names){
      out <- colnames(x)[out]
    }
  }
  out
}

zeroVar <- function(x)
{
  x <- x[,colnames(x) != ".outcome", drop = FALSE]
  which(apply(x, 2, function(x) length(unique(x)) < 2))
}

#' @rdname nearZeroVar
#' @export
checkConditionalX <- function(x, y)
{
  x$.outcome <- y
  unique(unlist(dlply(x, .(.outcome), zeroVar)))
}

#' @rdname nearZeroVar
#' @export
checkResamples <- function(index, x, y)
{
  if(!is.factor(y)) stop("y must be a factor")
  if(length(levels(y)) < 2) stop("y must have at least 2 levels")
  wrap <- function(index, x, y) checkConditionalX(x[index,,drop=FALSE], y[index])
  unique(unlist(lapply(index, wrap, x = x, y = y)))
}
