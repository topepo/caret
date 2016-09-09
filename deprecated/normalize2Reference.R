#' @importFrom stats approx
"normalize2Reference" <-
  function (data, refData = NULL, ties = TRUE) 
{
  ## adapted from limma's normalizeQuantiles
  if (is.null(dim(data)))
    {
      numSamples <- 1
      numProbes <- length(data)
      data <- as.matrix(data)
    } else {
      numSamples <- dim(data)[2]
      numProbes <- dim(data)[1]   
    }
  
  dataOrder <- sortedData <- array(, dim(data))  
  if (ties) dataRanks <- dataOrder           
  
  nobs <- rep(numProbes, numSamples)
  
  quantProbs <- (0:(numProbes - 1))/(numProbes - 1)  
  
  for (j in 1:numSamples) 
    {
      ## sort each column of data and get indicies and ranks
      sortedSampleI <- sort(data[, j],
                            method = "quick",
                            index.return = TRUE)
      if (ties) dataRanks[, j] <- rank(data[, j])
      nobsj <- length(sortedSampleI$x)
      if (nobsj < numProbes) {
        nobs[j] <- nobsj
        isna <- is.na(data[, j])
        sortedData[, j] <- approx((0:(nobsj - 1))/(nobsj - 1),
                                  sortedSampleI$x, 
                                  quantProbs,
                                  ties = "ordered")$y
        dataOrder[!isna, j] <- ((1:numProbes)[!isna])[sortedSampleI$ix]
      }
      else {
        sortedData[, j] <- sortedSampleI$x
        dataOrder[, j] <- sortedSampleI$ix
      }
    }
  ## at this point, we have the data sorted within each column and a key in dataOrder
  ## to revert back to the original input data: data[,j] == > sortedData[dataOrder[,j],j]
  
  ## use the row means from the data quantiles when no explicit reference is given.    
  if(is.null(refData))
    {
      refQuantile <- rowMeans(sortedData)
    } else refQuantile <- sort(refData)
  
  for (j in 1:numSamples) {
    if (nobs[j] < numProbes)
      {
      isna <- is.na(data[, j])
      if (ties) 
        ## The approx does a linear approx on the line where 
        ## x is the index scaled to [0, 1] and y is the mean
        ## for that index across all samples (or reference quants)
        ## It returns an approximated mean value 
        data[!isna, j] <- approx(quantProbs,
                                 refQuantile, 
                                 (dataRanks[!isna, j] - 1)/(nobs[j] - 1),
                                 ties = "ordered")$y
      else data[dataOrder[!isna, j], j] <- approx(quantProbs,
                                                  refQuantile, 
                                                  (0:(nobs[j] - 1))/(nobs[j] - 1),
                                                  ties = "ordered")$y
    }
    else {
      if (ties) 
        data[, j] <- approx(quantProbs,
                            refQuantile, 
                            (dataRanks[, j] - 1)/(numProbes - 1),
                            ties = "ordered")$y
      else data[dataOrder[, j], j] <- refQuantile
    }
  }
  data
}

