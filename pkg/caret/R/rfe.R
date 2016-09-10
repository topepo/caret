stripplot.rfe <- function(x,
                          data = NULL,
                          metric = x$metric,
                          ...)
{
  if (!is.null(match.call()$data))
    warning("explicit 'data' specification ignored")

  if(x$control$method %in%  c("oob", "LOOCV"))
    stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

  data <- as.data.frame(x$resample)
  data$Variable <- factor(data$Variable,
                          levels = paste(sort(unique(data$Variable))))
  theDots <- list(...)
  if(any(names(theDots) == "horizontal"))
  {
    formText <- if(theDots$horizontal) paste("Variable ~", metric) else paste(metric, "~ Variable")
  } else  formText <- paste("Variable ~", metric)

  form <- as.formula(formText)

  stripplot(form, data = data, ...)

}

#' @export
update.rfe <- function(object, x, y, size, ...) {
  size <- size[1]
  selectedVars <- object$variables
  bestVar <- object$control$functions$selectVar(selectedVars, size)
  object$fit <- object$control$functions$fit(x[, bestVar, drop = FALSE],
                                             y,
                                             first = FALSE,
                                             last = TRUE,
                                             ...)
  object$bestSubset <- size
  object$bestVar <- bestVar

  if(object$control$returnResamp == "final") {
    warning("The saved resamples are no longer appropriate and were removed")
    object$resampledCM <- object$resample <- NULL
  }
  object
}


