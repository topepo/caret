#' @rdname caret-internal
#' @export
resampleWrapper <- function(x, ind) {
  out <- rep(NA, dim(x$data)[1])
  trainData <- x$data
  x$data <- x$data[ind, ]
  tmpModelFit <- do.call(createModel, x)
  outBagData <- trainData[-ind, ]
  outBagData$.outcome <- NULL

  if (is.factor(x$data$.outcome)) {
    out[-ind] <- as.character(predictionFunction(
      x$method,
      tmpModelFit,
      outBagData
    ))
  } else {
    out[-ind] <- predictionFunction(x$method, tmpModelFit, outBagData)
  }
  out
}
