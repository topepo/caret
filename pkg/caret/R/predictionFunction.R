#' @rdname caret-internal
#' @importFrom stats predict
#' @export
predictionFunction <- function(method, modelFit, newdata, preProc = NULL, param = NULL)
{
  if(!is.null(newdata) && !is.null(preProc)) newdata <- predict(preProc, newdata)
  out <- method$predict(modelFit = modelFit, 
                        newdata = newdata, 
                        submodels = param)
  ## TODO convert to character with classification
  out 
}


