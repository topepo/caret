createResample <- function(y, times = 10, list = TRUE)
{
  trainIndex <- matrix(0, ncol = times, nrow = length(y))   
  out <- apply(trainIndex, 2, 
               function(data)
               {    
                 index <- seq(along = data)
                 out <- sort(sample(index, size = length(index), replace = TRUE))
                 out      
               })

  if (list) 
    {
      out <- as.data.frame(out)
      attributes(out) <- NULL
      names(out) <- prettySeq(out)
    } else {
      colnames(out) <- prettySeq(1:ncol(out))
    }
  
  out
}

