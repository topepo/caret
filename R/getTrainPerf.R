#' @export
getTrainPerf <- function(x) {
  bestPerf <- x$bestTune
  colnames(bestPerf) <- gsub("^\\.", "", colnames(bestPerf))
  out <- merge(x$results, bestPerf)
  out <- out[, colnames(out) %in% x$perfNames, drop = FALSE]
  colnames(out) <- paste("Train", colnames(out), sep = "")
  out$method <- x$method
  out
}

