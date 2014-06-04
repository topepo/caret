checkInstall <- function(pkg){
  good <- rep(TRUE, length(pkg))
  for(i in seq(along = pkg)){
    tested <- try(find.package(pkg[i]), silent = TRUE)
    if(class(tested)[1] == "try-error") good[i] <- FALSE
  }
  if(any(!good)){
    pkList <- paste(pkg[!good], collapse = ", ")
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
    if(interactive()) {
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
      } else stop()
    } else stop()
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
