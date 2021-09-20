#' Tools for Models Available in \code{train}
#'
#' These function show information about models and packages that are
#' accessible via \code{\link{train}}
#'
#' \code{modelLookup} is good for getting information related to the tuning
#' parameters for a model. \code{getModelInfo} will return all the functions
#' and metadata associated with a model. Both of these functions will only
#' search within the models bundled in this package.
#'
#' \code{checkInstall} will check to see if packages are installed. If they are
#' not and the session is interactive, an option is given to install the
#' packages using \code{\link[utils]{install.packages}} using that functions
#' default arguments (the missing packages are listed if you would like to
#' install them with other options). If the session is not interactive, an
#' error is thrown.
#'
#' @aliases modelLookup getModelInfo checkInstall
#' @param model a character string associated with the \code{method} argument
#' of \code{\link{train}}. If no value is passed, all models are returned. For
#' \code{getModelInfo}, regular expressions can be used.
#' @param regex a logical: should a regular expressions be used? If
#' \code{FALSE}, a simple match is conducted against the whole name of the
#' model.
#' @param pkg a character string of package names.
#' @param ...  options to pass to \code{\link[base]{grepl}}
#' @return \code{modelLookup} produces a data frame with columns \item{model
#' }{a character string for the model code} \item{parameter }{the tuning
#' parameter name} \item{label }{a tuning parameter label (used in plots)}
#' \item{forReg }{a logical; can the model be used for regression?}
#' \item{forClass }{a logical; can the model be used for classification?}
#' \item{probModel }{a logical; does the model produce class probabilities?}
#'
#' \code{getModelInfo} returns a list containing one or more lists of the
#' standard model information.
#'
#' \code{checkInstall} returns not value.
#' @note The column \code{seq} is no longer included in the output of
#' \code{modelLookup}.
#' @author Max Kuhn
#' @seealso \code{\link{train}}, \code{\link[utils]{install.packages}},
#' \code{\link[base]{grepl}}
#' @keywords utilities
#' @examples
#'
#' \dontrun{
#' modelLookup()
#' modelLookup("gbm")
#'
#' getModelInfo("pls")
#' getModelInfo("^pls")
#' getModelInfo("pls", regex = FALSE)
#'
#' checkInstall(getModelInfo("pls")$library)
#' }
#'
#' @export modelLookup
modelLookup <- function(model = NULL){
  load(system.file("models", "models.RData", package = "caret"))
  if(!is.null(model)){
    if(!(model %in% names(models))) stop(paste("Model '", method, "' is not in the ",
                                               "set of existing models", sep = ""))

    models <- models[model == names(models)]
  }
  out <- lapply(models,
                function(x) {
                  out <- x$parameters[, c("parameter", "label")]
                  out$forReg <- "Regression" %in% x$type
                  out$forClass <- "Classification" %in% x$type
                  out$probModel <- !is.null(x$prob)
                  out
                })
  for(i in seq(along = out)) out[[i]]$model <- names(models)[i]
  out <- do.call("rbind", out)
  rownames(out) <- NULL
  out <- out[, c('model', 'parameter', 'label', 'forReg', 'forClass', 'probModel')]
  out[order(out$model),]
}

#' @importFrom utils installed.packages
missing_packages <- function(mods = getModelInfo()) {
  libs <- unique(unlist(lapply(mods, function(x) x$library)))
  here <- rownames(installed.packages())
  libs[!(libs %in% here)]
}

#' @rdname modelLookup
#' @importFrom utils install.packages menu
#' @export
checkInstall <- function(pkg){
  good <- rep(TRUE, length(pkg))
  for(i in seq(along = pkg)){
    tested <- try(find.package(pkg[i]), silent = TRUE)
    if (inherits(tested, "try-error")) good[i] <- FALSE
  }
  if(any(!good)){
    pkList <- paste(pkg[!good], collapse = ", ")
    msg <- paste(sum(!good),
                 ifelse(sum(!good) > 1, " packages are", " package is"),
                 " needed and",
                 ifelse(sum(!good) > 1, " are", " is"),
                 " not installed. (",
                 pkList,
                 "). Would you like to try to install",
                 ifelse(sum(!good) > 1, " them", " it"),
                 " now?",
                 sep = "")

    if(interactive()) {
      cat(msg)
      bioc <- c("affy", "logicFS", "gpls", "vbmp")
      installChoice <- menu(c("yes", "no"))
      if(installChoice == 1){
        hasBioc <- any(pkg[!good] %in% bioc)
        if(!hasBioc) {
          install.packages(pkg[!good])
        } else {
          inst <- pkg[!good]
          instC <- inst[!(inst %in% bioc)]
          instB <- inst[inst %in% bioc]
          if(length(instC) > 0) install.packages(instC)
          biocLite <- NULL
          source("http://bioconductor.org/biocLite.R")
          biocLite(instB)
        }
      } else  {
        stop("Required packages are missing: ", pkList, call. = FALSE)
      }
    } else {
      stop("Required packages are missing: ", pkList, call. = FALSE)
    }
  }
}

#' @rdname modelLookup
#' @export
getModelInfo <- function(model = NULL, regex = TRUE, ...) {
  load(system.file("models", "models.RData", package = "caret"))
  if(!is.null(model)){
    keepers <- if(regex) grepl(model, names(models), ...) else which(model == names(models))[1]
    models <- models[keepers]
  }
  if(length(models) == 0) stop("That model is not in caret's built-in library")
  models
}
