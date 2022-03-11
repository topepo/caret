
#' @importFrom stats median predict quantile
additivePlot <- function(x, data, n = 100, quant = 0, plot = TRUE, ...)
  {
    if(inherits(x, "earth"))
      {
        data <- data[, predictors(x), drop = FALSE]
      }
    seqs <- lapply(data,
                   function(x, len, q) list(seq = seq(
                                              quantile(x, na.rm = TRUE, probs = q),
                                              quantile(x, na.rm = TRUE, probs = 1 - q),
                                              length = len),
                                            var = ""),
                   len = n,
                   q = quant)
    for(i in seq(along = seqs)) seqs[[i]]$var <- colnames(data)[i]
    meds <- lapply(data,
                   function(x, len) rep(median(x, na.rm = TRUE), len),
                   len = n)
    meds <- as.data.frame(meds, stringsAsFactors = TRUE)
    predGrid <- lapply(seqs,
                       function(x, m)
                       {
                         m[, x$var] <- x$seq
                         m$variable <- x$var
                         m
                       },
                       m = meds)
    predGrid <- do.call("rbind", predGrid)
    predGrid$predicted <- predict(x, predGrid[, colnames(data), drop = FALSE], ...)
    predGrid$x <- unlist(lapply(seqs, function(x) x$seq))
    if(plot)
      {
        out <- xyplot(predicted ~ x|variable,
                      data = predGrid,
                      between = list(x = 2),
                      scales = list(x = list(relation = "free")),
                      as.table = TRUE,
                      xlab = "",
                      ylab = "Predicted",
                      type = "l")
      } else out <- predGrid
    out
  }

