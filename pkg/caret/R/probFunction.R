#' @rdname caret-internal
#' @export
probFunction <- function(method, modelFit, newdata = NULL, preProc = NULL, param = NULL)
{
  if(!is.null(newdata) && !is.null(preProc)) newdata <- predict(preProc, newdata)

  obsLevels <- levels(modelFit)

  classProb <- method$prob(modelFit = modelFit,
                           newdata = newdata,
                           submodels = param)
  if(!is.data.frame(classProb) & is.null(param))
  {
    classProb <- as.data.frame(classProb, stringsAsFactors = TRUE)
    if(!is.null(obsLevels)) classprob <- classProb[, obsLevels]
  }
  classProb
}
