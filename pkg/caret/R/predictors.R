#' List predictors used in the model
#'
#' This class uses a model fit to determine which predictors were used in the
#' final model.
#'
#' For \code{\link[randomForest]{randomForest}}, \code{\link[party]{cforest}},
#' \code{\link[party]{ctree}}, \code{\link[rpart]{rpart}},
#' \code{\link[ipred:bagging]{ipredbagg}}, \code{\link[ipred]{bagging}},
#' \code{\link[earth]{earth}}, \code{\link[mda]{fda}},
#' \code{\link[pamr]{pamr.train}}, \code{\link[superpc]{superpc.train}},
#' \code{\link{bagEarth}} and \code{\link{bagFDA}}, an attempt was made to
#' report the predictors that were actually used in the final model.
#'
#' The \code{predictors} function can be called on the model object (as opposed
#' to the \code{\link{train}}) object) and the package will try to find the
#' appropriate coed (if it exists).
#'
#' In cases where the predictors cannot be determined, \code{NA} is returned.
#' For example, \code{\link[nnet]{nnet}} may return missing values from
#' \code{predictors}.
#'
#' @aliases predictors predictors.formula predictors.terms predictors.train
#' predictors.default predictors.list predictors.rfe predictors.sbf
#' @param x a model object, list or terms
#' @param \dots not currently used
#' @return a character string of predictors or \code{NA}.
#' @keywords models
#' @export predictors
"predictors" <- function(x, ...){
    UseMethod("predictors")
  }

#' @export
predictors.train <- function(x, ...) {
  if(is.null(x$modelInfo)) {
    code <- getModelInfo(x$method, regex = FALSE)[[1]]
  } else code <- x$modelInfo
  if(!is.null(code$predictors)){
    checkInstall(code$library)
    for(i in seq(along = code$library))
      do.call("requireNamespaceQuietStop", list(package = code$library[i]))
    out <- code$predictors(x$finalModel, ...)
  } else {
    if(hasTerms(x)) {
      out <- predictors(x$terms, ...)
    } else out <- NA
  }
  out
  }

#' @export
predictors.default <- function(x, ...) {
  cls <- model2method(class(x)[1])
  if(cls == "gam")  cls <- if(any(names(x) == "optimizer")) "gam" else "gamLoess"
  code <- getModelInfo(cls, regex = FALSE)[[1]]
  if(!is.null(code)) {
    if(!is.null(code$predictors)){
      checkInstall(code$library)
      for(i in seq(along = code$library))
        do.call("requireNamespaceQuietStop", list(package = code$library[i]))
      out <- code$predictors(x, ...)
    } else {
      if(hasTerms(x)) {
        out <- predictors(x$terms, ...)
      } else out <- NA
    }
  } else {
    out <- if(hasTerms(x)) predictors(x$terms) else NA
  }
  out
}

#' @rdname caret-internal
#' @export
hasTerms <- function(x)
  {
    objNames <- c(names(x), slotNames(x))
    "terms" %in% tolower(objNames)
  }

## basicVars tries to detect the actual variable that are used
## when a formula might include other terms (such as interactions)
## For example:
## > x
## [1] "medv" "crim" "zn"   "age"
## > y
## [1] "crim"     "I(age^2)" "zn"
## > basicVars(x, y)
## [1] "crim" "zn"   "age"

basicVars <- function(x, y)
  {
    hasVar <- rep(NA, length(x))
    for(i in seq(along = x))
      hasVar[i] <- length(grep(x[i], y, fixed = TRUE)) > 0
    x[hasVar]
  }

#' @export
predictors.terms <- function(x, ...)
  {
    if(is.null(x)) return(NA)
    everything <- all.vars(x)
    yName <- as.character(x[[2]])
    everything[!(everything %in% yName)]
  }

#' @export
predictors.formula <- function(x, ...)
  {
    everything <- all.vars(x)
    yName <- as.character(x[[2]])
    everything[!(everything %in% yName)]
  }

#' @export
predictors.list <- function(x, ...)
  {
    out <- lapply(x, predictors)
    names(out) <- names(x)
    out
  }
