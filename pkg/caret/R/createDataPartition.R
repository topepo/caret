createDataPartition <- function (y, times = 1, p = 0.5, list = TRUE, groups = min(5, length(y)))
{
  out <- vector(mode = "list", times)

  if(length(y) < 2) stop("y must have at least 2 data points")
  
  if(groups < 2) groups <- 2
  
  if(is.numeric(y))
    {
      y <- cut(y, 
               unique(quantile(y, probs = seq(0, 1, length = groups))), 
               include.lowest = TRUE)
    }
  
  y <- factor(y)
  dataInd <- seq(along = y)
  numInClass <- table(y)
  sampleNums <- ceiling(numInClass * p)
  sampleNums <- ifelse(sampleNums == numInClass, sampleNums - 
                       1, sampleNums)
  groupNames <- names(sampleNums)
  for (j in 1:times) {
    for (i in seq(along = sampleNums)) {
      if (sampleNums[i] > 0) {
        trainData <- sort(sample(dataInd[y = which(y == 
                                           groupNames[i])], sampleNums[i]))
        out[[j]] <- append(out[[j]], trainData)
      }
    }
  }
  
  if (!list)
    {
      out <- matrix(unlist(out), ncol = times)
      colnames(out) <- prettySeq(1:ncol(out))
    } else {
      names(out) <- prettySeq(out)
    }
  out
}

