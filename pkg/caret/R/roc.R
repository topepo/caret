roc <- function(data, class, dataGrid = TRUE, gridLength = 100, positive = levels(class)[1])
{
  warning("This function is deprecated a of 1/3/12. The computations now utilize the pROC package. This function will be removed in a few releases.")

  if(!is.character(positive) | length(positive) != 1) stop("positive argument should be a single character value")
  
  if(!(positive %in%  levels(class))) stop("wrong level specified")
  if(length(levels(class)) != 2) stop("wrong number of levels")
  if(dataGrid) cutoffDF <- data.frame(value = sort(unique(data)))
  else cutoffDF <- data.frame(value = seq(
                                from = min(data, na.rm = TRUE),
                                to = max(data, na.rm = TRUE),
                                length = gridLength))
  numCuts <- dim(cutoffDF)[1]
  out <- matrix(NA, ncol = 3, nrow = numCuts + 1)
  
  out[2:(numCuts + 1), ] <- t(apply(cutoffDF, 1, rocPoint, x = data, y = class, positive = positive))
  out[1, ] <- c(NA, 1, 0)
  colnames(out) <- c("cutoff", "sensitivity", "specificity")
  out
}
