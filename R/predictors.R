#' List predictors used in the model
#'
#' This class uses a model fit to determine which predictors were used in the
#' final model.
#'
#' For [randomForest::randomForest()], [party::cforest()], [party::ctree()],
#' [rpart::rpart()], [ipred::ipredbagg()], [ipred::bagging()],
#' [earth::earth()], [mda::fda()], [pamr::pamr.train()],
#' [superpc::superpc.train()], [bagEarth()] and [bagFDA()], an attempt was made
#' to report the predictors that were actually used in the final model.
#'
#' The `predictors` function can be called on the model object (as opposed to
#' the [train()]) object) and the package will try to find the appropriate coed
#' (if it exists).
#'
#' In cases where the predictors cannot be determined, `NA` is returned. For
#' example, [nnet::nnet()] may return missing values from `predictors`.
#'
#' @aliases predictors predictors.formula predictors.terms predictors.train predictors.default predictors.list predictors.rfe predictors.sbf
#' @param x a model object, list or terms
#' @param \dots not currently used
#' @return a character string of predictors or `NA`.
#' @keywords models
#' @export predictors
"predictors" <- function(x, ...) {
  UseMethod("predictors")
}

#' @export
predictors.train <- function(x, ...) {
  if (is.null(x$modelInfo)) {
    code <- getModelInfo(x$method, regex = FALSE)[[1]]
  } else {
    code <- x$modelInfo
  }
  if (!is.null(code$predictors)) {
    checkInstall(code$library)
    for (i in seq(along.with = code$library)) {
      do.call("requireNamespaceQuietStop", list(package = code$library[i]))
    }
    out <- code$predictors(x$finalModel, ...)
  } else {
    if (hasTerms(x)) {
      out <- predictors(x$terms, ...)
    } else {
      out <- NA
    }
  }
  out
}

#' @export
predictors.default <- function(x, ...) {
  cls <- model2method(class(x)[1])
  if (cls == "gam") {
    if (any(names(x) == "optimizer")) {
      cls <- "gam"
    } else {
      cls <- "gamLoess"
    }
  }
  code <- getModelInfo(cls, regex = FALSE)[[1]]
  if (!is.null(code)) {
    if (!is.null(code$predictors)) {
      checkInstall(code$library)
      for (i in seq(along.with = code$library)) {
        do.call("requireNamespaceQuietStop", list(package = code$library[i]))
      }
      out <- code$predictors(x, ...)
    } else {
      if (hasTerms(x)) {
        out <- predictors(x$terms, ...)
      } else {
        out <- NA
      }
    }
  } else {
    if (hasTerms(x)) {
      out <- predictors(x$terms)
    } else {
      out <- NA
    }
  }
  out
}

#' @rdname caret-internal
#' @export
hasTerms <- function(x) {
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

basicVars <- function(x, y) {
  hasVar <- rep(NA, length(x))
  for (i in seq(along.with = x)) {
    hasVar[i] <- length(grep(x[i], y, fixed = TRUE)) > 0
  }
  x[hasVar]
}

#' @export
predictors.terms <- function(x, ...) {
  if (is.null(x)) {
    return(NA)
  }
  everything <- all.vars(x)
  yName <- as.character(x[[2]])
  everything[!(everything %in% yName)]
}

#' @export
predictors.formula <- function(x, ...) {
  everything <- all.vars(x)
  yName <- as.character(x[[2]])
  everything[!(everything %in% yName)]
}

#' @export
predictors.list <- function(x, ...) {
  out <- lapply(x, predictors)
  names(out) <- names(x)
  out
}
