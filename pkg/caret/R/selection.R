
## In these functions, x is the data fram of performance values and tuning parameters.


oneSE <- function(x, metric, num, maximize)
  {
    index <- 1:nrow(x)
    
    if(!maximize)
      {
        bestIndex <- which.min(x[,metric])  
        perf <- x[bestIndex,metric] + (x[bestIndex,paste(metric, "SD", sep = "")])/sqrt(num)
        candidates <- index[x[, metric] <= perf]
        bestIter <- min(candidates)
      } else {
        bestIndex <- which.max(x[,metric])  
        perf <- x[bestIndex,metric] - (x[bestIndex,paste(metric, "SD", sep = "")])/sqrt(num)

        candidates <- index[x[, metric] >= perf]
        bestIter <- min(candidates)
      }
    bestIter
  }

tolerance <- function(x, metric, tol = 1.5, maximize)
  {
       
    index <- 1:nrow(x)
    
    if(!maximize)
      {
        best <- min(x[,metric])  
        perf <- (x[,metric] - best)/best * 100
        candidates <- index[perf < tol]
        bestIter <- min(candidates)
      } else {
        best <- max(x[,metric])  
        perf <- (x[,metric] - best)/best * -100
        candidates <- index[perf < tol]
        bestIter <- min(candidates)
      }
    bestIter
  }


