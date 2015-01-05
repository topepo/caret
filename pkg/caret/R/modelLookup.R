#' Install a needed package
#'
#' In interactive mode, the system prompts the user for permission to
#' install.  In non-interactive mode, permission is assumed.
#'
#' @param pkg One more more packages as a vector of characters.
checkInstall <- function(pkg){
  # packages that are not 'good' need to be installed
  good <- rep(TRUE, length(pkg))
  for(i in seq(along = pkg)){
    tested <- try(find.package(pkg[i]), silent = TRUE)
    if(class(tested)[1] == "try-error") good[i] <- FALSE
  }
  if(all(good)) return(invisible())
  # one or more packages need to be installed
  inst <- pkg[!good]
  pkList <- paste(inst, collapse = ", ")
  if(interactive()) {
    msg <- paste(sum(!good),
                 ifelse(sum(!good) > 1, " packages are", " package is"),
                 " needed for this model and",
                 ifelse(sum(!good) > 1, " are", " is"),
                 " not installed. (",
                 pkList,
                 "). Would you like to try to install",
                 ifelse(sum(!good) > 1, " them", " it"),
                 " now?",
                 sep = "")
    cat(msg)
    installChoice <- menu(c("yes", "no"))
    if(1 != installChoice) stop()
    }
  writeLines(paste('Installing packages: ', pkList))
  bioc <- c("affy", "logicFS", "gpls", "vbmp")
  hasBioc <- any(inst %in% bioc)
  if(!hasBioc) {
    install.packages(inst)
  } else {
    # the list of packages to install from CRAN
    instC <- inst[!(inst %in% bioc)]
    # the list of packages to install from bioconductor
    instB <- inst[inst %in% bioc]
    if(length(instC) > 0) install.packages(instC)
    biocLite <- NULL
    source("http://bioconductor.org/biocLite.R")
    biocLite(instB)
  }
}

getModelInfo <- function(model = NULL, regex = TRUE, ...) {
  load(system.file("models", "models.RData", package = "caret"))
  if(!is.null(model)){
    keepers <- if(regex) grepl(model, names(models), ...) else which(model == names(models))[1]
    models <- models[keepers]
  }
  if(length(models) == 0) stop("That model is not in caret's built-in library")
  models
}


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


missing_packages <- function(mods = getModelInfo()) {
  libs <- unique(unlist(lapply(mods, function(x) x$library)))
  here <- rownames(installed.packages())
  libs[!(libs %in% here)]
}
