nearZeroVar <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, foreach = FALSE, allowParallel = TRUE) {
  
  if(!foreach) return(nzv(x, freqCut = freqCut, uniqueCut = uniqueCut, saveMetrics = saveMetrics))
  
  `%op%` <- getOper(foreach && allowParallel && getDoParWorkers() > 1)
  
  if(saveMetrics) {
    res <- foreach(name = colnames(x), .combine=rbind) %op% {
      r <- nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut, saveMetrics = TRUE)
      r[,"column" ] <-  name
      r
    }
    res <- res[, c(5, 1, 2, 3, 4)]
    rownames(res) <- as.character(res$column)
    res$column <- NULL
  } else {
    res <- foreach(name = colnames(x), .combine=c) %op% {
      nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut, saveMetrics = FALSE)
    } 
  }
  res
}

nzv <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE)
{
  if (is.vector(x)) x <- matrix(x, ncol = 1)
  freqRatio <- apply(x, 2, function(data)
  {
    t <- table(data[!is.na(data)]) 
    if (length(t) <= 1) {
      return(0);
    }
    w <- which.max(t);
    return(max(t, na.rm=TRUE)/max(t[-w], na.rm=TRUE))
  })
  lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))
  percentUnique <- 100 * lunique / apply(x, 2, length)
  zeroVar <- (lunique == 1) | apply(x, 2, function(data) all(is.na(data)))
  if (saveMetrics)
  {
    out <- data.frame(freqRatio = freqRatio,
                      percentUnique = percentUnique,
                      zeroVar = zeroVar,
                      nzv = (freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
  }
  else {
    out <- which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
    names(out) <- NULL
  }
  out
}

zeroVar <- function(x)
{
  x <- x[,colnames(x) != ".outcome", drop = FALSE]
  which(apply(x, 2, function(x) length(unique(x)) < 2))
}

checkConditionalX <- function(x, y)
{
  x$.outcome <- y
  unique(unlist(dlply(x, .(.outcome), zeroVar)))
}


checkResamples <- function(index, x, y)
{
  if(!is.factor(y)) stop("y must be a factor")
  if(length(levels(y)) < 2) stop("y must have at least 2 levels")
  wrap <- function(index, x, y) checkConditionalX(x[index,,drop=FALSE], y[index])
  unique(unlist(lapply(index, wrap, x = x, y = y)))
}

nearZeroVarOld <- function(x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE)
{
  if(is.vector(x)) x <- matrix(x, ncol = 1)
  freqRatio <- apply(
    x, 
    2, 
    function(data)
    {
      dataTable <- sort(table(data[!is.na(data)]), decreasing = TRUE)
      if(length(dataTable ) >= 2) 
      {
        dataTable [1]/dataTable[2]
      } else 0
    })
  percentUnique <- apply(
    x, 
    2, 
    function(data) 100*length(unique(data[!is.na(data)]))/length(data))
  
  zeroVar <- apply(x, 2, function(data) length(unique(data[!is.na(data)])) == 1 | all(is.na(data)))
  
  
  if(saveMetrics)
  {
    out <- data.frame(freqRatio = freqRatio, 
                      percentUnique = percentUnique, 
                      zeroVar = zeroVar,
                      nzv = (freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
  } else {
    out <- which((freqRatio > freqCut & percentUnique <= uniqueCut) | zeroVar)
    names(out) <- NULL
  }
  out
}
