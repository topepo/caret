#' @rdname varImp
#' @export
#' @importFrom recipes juice all_predictors all_outcomes
"varImp.train" <- function(object, useModel = TRUE, nonpara = TRUE, scale = TRUE, ...) {
  code <- object$modelInfo
  if(is.null(code$varImp)) useModel <- FALSE
  if(useModel) {
    checkInstall(code$library)
    for(i in seq(along = code$library))
      do.call("requireNamespaceQuietStop", list(package = code$library[i]))
    imp <- code$varImp(object$finalModel, ...)
    modelName <- object$method
  } else {
    if(inherits(object, "train.recipe")) {
      x_dat <- recipes::juice(object$recipe, all_predictors())
      x_dat <- as.data.frame(x_dat, stringsAsFactors = FALSE)
      y_dat <- recipes::juice(object$recipe, all_outcomes())
      y_dat <- getElement(y_dat, names(y_dat))
    } else {
      isX <- which(!(colnames(object$trainingData) %in% ".outcome"))
      x_dat <- object$trainingData[, isX,drop = FALSE]
      y_dat <- object$trainingData[, -isX]
    }
    imp <- filterVarImp(x_dat, y_dat,
                        nonpara = nonpara,
                        ...)
    modelName <- ifelse(is.factor(y_dat),
                        "ROC curve",
                        ifelse(nonpara, "loess r-squared", "Linear model"))
  }
  if(scale) {
    if(class(object$finalModel)[1] == "pamrtrained") imp <- abs(imp)
    imp <- imp - min(imp, na.rm = TRUE)
    imp <- imp/max(imp, na.rm = TRUE)*100
  }
  out <- list(importance = imp,
              model = modelName,
              calledFrom = "varImp")
  
  structure(out, class = "varImp.train")
}
