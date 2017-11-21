
#' @export
levels.train <- function(x, ...) {
  if(any(names(x) == "levels")) {
    out <- x$levels
    attributes(out) <- NULL
  } else {
    if(x$modelType == "Classification") {
      if(!isS4(x$finalModel) && !is.null(x$finalModel$obsLevels))
        return(x$finalModel$obsLevels)
      if(is.null(x$modelInfo)) {
        code <- getModelInfo(x$method, regex = FALSE)[[1]]
      } else code <- x$modelInfo
      if(!is.null(code$levels)){
        checkInstall(code$library)
        for(i in seq(along = code$library))
          do.call("requireNamespaceQuietStop", list(package = code$library[i]))
        out <- code$levels(x$finalModel, ...)
      } else out <- NULL
    } else out <- NULL
  }
  out
}








































