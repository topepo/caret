#' Create A Full Set of Dummy Variables
#'
#' \code{dummyVars} creates a full set of dummy variables (i.e. less than full
#' rank parameterization)
#'
#' Most of the \code{\link[stats]{contrasts}} functions in R produce full rank
#' parameterizations of the predictor data. For example,
#' \code{\link[stats]{contr.treatment}} creates a reference cell in the data
#' and defines dummy variables for all factor levels except those in the
#' reference cell. For example, if a factor with 5 levels is used in a model
#' formula alone, \code{\link[stats]{contr.treatment}} creates columns for the
#' intercept and all the factor levels except the first level of the factor.
#' For the data in the Example section below, this would produce:
#' \preformatted{ (Intercept) dayTue dayWed dayThu dayFri daySat daySun
#'            1      0      0      0      0      0      0
#'            1      0      0      0      0      0      0
#'            1      0      0      0      0      0      0
#'            1      0      1      0      0      0      0
#'            1      0      1      0      0      0      0
#'            1      0      0      0      1      0      0
#'            1      0      0      0      0      1      0
#'            1      0      0      0      0      1      0
#'            1      0      0      0      1      0      0}
#'
#' In some situations, there may be a need for dummy variables for all the
#' levels of the factor. For the same example:
#' \preformatted{ dayMon dayTue dayWed dayThu dayFri daySat daySun
#'       1      0      0      0      0      0      0
#'       1      0      0      0      0      0      0
#'       1      0      0      0      0      0      0
#'       0      0      1      0      0      0      0
#'       0      0      1      0      0      0      0
#'       0      0      0      0      1      0      0
#'       0      0      0      0      0      1      0
#'       0      0      0      0      0      1      0
#'       0      0      0      0      1      0      0}
#'
#' Given a formula and initial data set, the class \code{dummyVars} gathers all
#' the information needed to produce a full set of dummy variables for any data
#' set. It uses \code{contr.ltfr} as the base function to do this.
#'
#' \code{class2ind} is most useful for converting a factor outcome vector to a
#' matrix (or vector) of dummy variables.
#'
#' @aliases dummyVars dummyVars.default predict.dummyVars contr.dummy
#' contr.ltfr class2ind
#' @param formula An appropriate R model formula, see References
#' @param data A data frame with the predictors of interest
#' @param sep An optional separator between factor variable names and their
#' levels. Use \code{sep = NULL} for no separator (i.e. normal behavior of
#' \code{\link[stats]{model.matrix}} as shown in the Details section)
#' @param levelsOnly A logical; \code{TRUE} means to completely remove the
#' variable names from the column names
#' @param fullRank A logical; should a full rank or less than full rank
#' parameterization be used? If \code{TRUE}, factors are encoded to be
#' consistent with \code{\link[stats]{model.matrix}} and the resulting there
#' are no linear dependencies induced between the columns.
#' @param object An object of class \code{dummyVars}
#' @param newdata A data frame with the required columns
#' @param na.action A function determining what should be done with missing
#' values in \code{newdata}. The default is to predict \code{NA}.
#' @param n A vector of levels for a factor, or the number of levels.
#' @param contrasts A logical indicating whether contrasts should be computed.
#' @param sparse A logical indicating if the result should be sparse.
#' @param x A factor vector.
#' @param ... additional arguments to be passed to other methods
#' @return The output of \code{dummyVars} is a list of class 'dummyVars' with
#' elements \item{call }{the function call} \item{form }{the model formula}
#' \item{vars }{names of all the variables in the model} \item{facVars }{names
#' of all the factor variables in the model} \item{lvls }{levels of any factor
#' variables} \item{sep }{\code{NULL} or a character separator} \item{terms
#' }{the \code{\link[stats]{terms.formula}} object} \item{levelsOnly }{a
#' logical}
#'
#' The \code{predict} function produces a data frame.
#'
#' \code{class2ind} returns a matrix (or a vector if \code{drop2nd = TRUE}).
#'
#' \code{contr.ltfr} generates a design matrix.
#' @author \code{contr.ltfr} is a small modification of
#' \code{\link[stats]{contr.treatment}} by Max Kuhn
#' @seealso \code{\link[stats]{model.matrix}}, \code{\link[stats]{contrasts}},
#' \code{\link[stats]{formula}}
#' @references
#' \url{https://cran.r-project.org/doc/manuals/R-intro.html#Formulae-for-statistical-models}
#' @keywords models
#' @examples
#' when <- data.frame(time = c("afternoon", "night", "afternoon",
#'                             "morning", "morning", "morning",
#'                             "morning", "afternoon", "afternoon"),
#'                    day = c("Mon", "Mon", "Mon",
#'                            "Wed", "Wed", "Fri",
#'                            "Sat", "Sat", "Fri"),
#'                            stringsAsFactors = TRUE)
#'
#' levels(when$time) <- list(morning="morning",
#'                           afternoon="afternoon",
#'                           night="night")
#' levels(when$day) <- list(Mon="Mon", Tue="Tue", Wed="Wed", Thu="Thu",
#'                          Fri="Fri", Sat="Sat", Sun="Sun")
#'
#' ## Default behavior:
#' model.matrix(~day, when)
#'
#' mainEffects <- dummyVars(~ day + time, data = when)
#' mainEffects
#' predict(mainEffects, when[1:3,])
#'
#' when2 <- when
#' when2[1, 1] <- NA
#' predict(mainEffects, when2[1:3,])
#' predict(mainEffects, when2[1:3,], na.action = na.omit)
#'
#'
#' interactionModel <- dummyVars(~ day + time + day:time,
#'                               data = when,
#'                               sep = ".")
#' predict(interactionModel, when[1:3,])
#'
#' noNames <- dummyVars(~ day + time + day:time,
#'                      data = when,
#'                      levelsOnly = TRUE)
#' predict(noNames, when)
#'
#' head(class2ind(iris$Species))
#'
#' two_levels <- factor(rep(letters[1:2], each = 5))
#' class2ind(two_levels)
#' class2ind(two_levels, drop2nd = TRUE)
#' @export dummyVars
"dummyVars" <-
  function(formula, ...){
    UseMethod("dummyVars")
  }

#' @rdname dummyVars
#' @method dummyVars default
#' @importFrom stats as.formula model.frame
#' @export
dummyVars.default <- function (formula, data, sep = ".", levelsOnly = FALSE, fullRank = FALSE, ...)
{
  formula <- as.formula(formula)
  if(!is.data.frame(data)) data <- as.data.frame(data, stringsAsFactors = FALSE)

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

#' @rdname dummyVars
#' @method print dummyVars
#' @export
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

#' @rdname dummyVars
#' @method predict dummyVars
#' @importFrom stats delete.response model.frame model.matrix na.pass
#' @export
predict.dummyVars <- function(object, newdata, na.action = na.pass, ...)
{
  if(is.null(newdata)) stop("newdata must be supplied")
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
  if(!all(object$vars %in% names(newdata))) stop(
    paste("Variable(s)",
          paste("'", object$vars[!object$vars %in% names(newdata)],
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
    on.exit(options(contrasts = oldContr))
  }
  m <- model.frame(Terms, newdata, na.action = na.action, xlev = object$lvls)

  x <- model.matrix(Terms, m)

  cnames <- colnames(x)
  if(object$levelsOnly) {
    for(i in object$facVars) {
      for(j in object$lvls[[i]]) {
        from_text <- paste0(i, j)
        cnames[which(cnames == from_text)] <- j
      }
    }
  }
  if(!is.null(object$sep) & !object$levelsOnly) {
    for(i in object$facVars[order(-nchar(object$facVars))]) {
      ## the default output form model.matrix is NAMElevel with no separator.
      for(j in object$lvls[[i]]) {
        from_text <- paste0(i, j)
        to_text <- paste(i, j, sep = object$sep)
        pos = which(cnames == from_text)
        # If there are several identical NAMElevel matching (example: "X1" with level "11" and "X11" with level "1")
        if (length(pos) > 1) {
          # If the level j is not the first level of the feature i
          if (which(object$lvls[[i]] == j) > 1) {
            # Then we just have to test for the preceding NAMElevel being NAME(level-1)
            cnames[pos][cnames[pos-1] == paste(i, object$lvls[[i]][which(object$lvls[[i]] == j)-1], sep = object$sep)] <- to_text
          } else {
            # Otherwise, we have to test for the preceding NAMElevel being (NAME-1)(last_level)
            cnames[pos][cnames[pos-1] == paste(object$facVars[order(-nchar(object$facVars))][which(object$facVars[order(-nchar(object$facVars))] == i) - 1], utils::tail(object$lvls[[object$facVars[order(-nchar(object$facVars))][which(object$facVars[order(-nchar(object$facVars))] == i) - 1]]],n=1), sep = object$sep)] <- to_text
          }
        } else {
          # Otherwise simply replace the last occurence of the pattern
          cnames[pos] <- to_text
        }
      }
    }
  }
  colnames(x) <- cnames
  x[, colnames(x) != "(Intercept)", drop = FALSE]
}

#' @rdname dummyVars
#' @export
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

#' @export
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

#' @rdname dummyVars
#' @importFrom stats model.matrix
#' @export
#' @param drop2nd A logical: if the factor has two levels, should a single binary vector be returned?
class2ind <- function(x, drop2nd = FALSE) {
  if(!is.factor(x)) stop("'x' should be a factor")
  y <- model.matrix(~ x - 1)
  colnames(y) <- gsub("^x", "", colnames(y))
  attributes(y)$assign <- NULL
  attributes(y)$contrasts <- NULL
  if(length(levels(x)) == 2 & drop2nd) {
    y <- y[,1]
  }
  y
}
