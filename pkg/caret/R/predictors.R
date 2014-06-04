"predictors" <- function(x, ...){
    UseMethod("predictors")
  }

predictors.train <- function(x, ...) {
  if(is.null(x$modelInfo)) {
    code <- getModelInfo(x$method, regex = FALSE)[[1]]
  } else code <- x$modelInfo
  if(!is.null(code$predictors)){
    checkInstall(code$library)
    for(i in seq(along = code$library)) 
      do.call("require", list(package = code$library[i]))
    out <- code$predictors(x$finalModel, ...)
  } else {
    if(hasTerms(x)) {
      out <- predictors(x$terms, ...)
    } else out <- NA
  }
  out
  }

predictors.default <- function(x, ...) {
  cls <- model2method(class(x)[1])
  if(cls == "gam")  cls <- if(any(names(x) == "optimizer")) "gam" else "gamLoess"
  code <- getModelInfo(cls, regex = FALSE)[[1]]
  if(!is.null(code)) {
    if(!is.null(code$predictors)){
      checkInstall(code$library)
      for(i in seq(along = code$library)) 
        do.call("require", list(package = code$library[i]))
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

predictors.terms <- function(x, ...)
  {
    if(is.null(x)) return(NA)
    everything <- all.vars(x)
    yName <- as.character(x[[2]])
    everything[!(everything %in% yName)]
  }

predictors.formula <- function(x, ...)
  {
    everything <- all.vars(x)
    yName <- as.character(x[[2]])
    everything[!(everything %in% yName)]
  }

predictors.list <- function(x, ...)
  {
    out <- lapply(x, predictors)
    names(out) <- names(x)
    out
  }
