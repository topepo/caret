
contr.ltfr <- function (n, contrasts = TRUE, sparse = FALSE) 
{
  if (is.numeric(n) && length(n) == 1L) {
    if (n > 1L) 
      levels <- as.character(seq_len(n))
    else stop("not enough degrees of freedom to define contrasts")
  }
  else {
    levels <- as.character(n)
    n <- length(n)
  }
  contr <- .RDiag(levels, sparse = sparse)
  if (contrasts) {
    if (n < 2L) stop(gettextf("contrasts not defined for %d degrees of freedom", n - 1L), domain = NA)
  }
  contr
}

contr.dummy <- function(n, ...)
{
  if (is.numeric(n) && length(n) == 1L) {
    if (n > 1L) 
      levels <- as.character(seq_len(n))
    else stop("not enough degrees of freedom to define contrasts")
  }
  else {
    levels <- as.character(n)
    n <- length(n)
  }
  out <- diag(n)
  rownames(out) <- levels
  colnames(out) <- levels
  out
}


"dummyVars" <-
  function(formula, ...){
    UseMethod("dummyVars")
  }
dummyVars.default <- function (formula, data, sep = ".", levelsOnly = FALSE, fullRank = FALSE, ...) 
{
  formula <- as.formula(formula)
  if(!is.data.frame(data)) data <- as.data.frame(data)
  
  vars <- all.vars(formula)
  if(any(vars == "."))
  {
    vars <- vars[vars != "."]
    vars <- unique(c(vars, colnames(data)))
  }
  isFac <- unlist(lapply(data[,vars,drop = FALSE], is.factor))
  if(sum(isFac) > 0)
  {
    facVars <- vars[isFac] 
    lvls <- lapply(data[,facVars,drop = FALSE], levels)
    if(levelsOnly)
    {
      tabs <- table(unlist(lvls))
      if(any(tabs > 1))
      {
        stop(paste("You requested `levelsOnly = TRUE` but",
                   "the following levels are not unique",
                   "across predictors:",
                   paste(names(tabs)[tabs > 1], collapse = ", ")))
      }
    }
  } else {
    facVars <- NULL
    lvls <- NULL
  }
  trms <- attr(model.frame(formula, data), "terms")
  out <- list(call = match.call(),
              form = formula,
              vars = vars,
              facVars = facVars,
              lvls = lvls,
              sep = sep,
              terms = trms,
              levelsOnly = levelsOnly,
              fullRank = fullRank)
  class(out) <- "dummyVars"
  out
  
}


print.dummyVars <- function(x, ...)
{
  cat("Dummy Variable Object\n\n")
  cat("Formula: ")
  print(x$form)
  cat(length(x$vars),  " variables, ", length(x$facVars), " factors\n", sep = "")
  if(!is.null(x$sep) & !x$levelsOnly) cat("Variables and levels will be separated by '",
                                          x$sep, "'\n", sep = "")
  if(x$levelsOnly) cat("Factor variable names will be removed\n")
  if(x$fullRank) cat("A full rank encoding is used") else cat("A less than full rank encoding is used") 
  cat("\n")
  invisible(x)
}


predict.dummyVars <- function(object, newdata, na.action = na.pass, ...)
{
  if(is.null(newdata)) stop("newdata must be supplied")
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  if(!all(object$vars %in% names(newdata))) stop(
    paste("Variable(s)",
          paste("'", object$vars[object$vars %in% names(newdata)],
                "'", sep = "",
                collapse = ", "),
          "are not in newdata"))
  Terms <- object$terms
  Terms <- delete.response(Terms)
  if(!object$fullRank)
  {
    oldContr <- options("contrasts")$contrasts
    newContr <- oldContr
    newContr["unordered"] <- "contr.ltfr"
    options(contrasts = newContr)
  }
  m <- model.frame(Terms, newdata, na.action = na.action, xlev = object$lvls)
  
  x <- model.matrix(Terms, m)
  if(!object$fullRank) options(contrasts = oldContr)
  
  if(object$levelsOnly) {
    for(i in object$facVars) {
      for(j in object$lvls[[i]]) {
        from_text <- paste0(i, j)
        colnames(x) <- gsub(from_text, j, colnames(x), fixed = TRUE) 
      }
    }
  }
  if(!is.null(object$sep) & !object$levelsOnly) {
    for(i in object$facVars[order(-nchar(object$facVars))]) {
      ## the default output form model.matrix is NAMElevel with no separator. 
      for(j in object$lvls[[i]]) {
        from_text <- paste0(i, j)
        to_text <- paste(i, j, sep = object$sep)
        colnames(x) <- gsub(from_text, to_text, colnames(x), fixed = TRUE)
       }
    }
  }  
  x[, colnames(x) != "(Intercept)", drop = FALSE]
}


