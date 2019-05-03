#' Summary of resampled performance estimates
#' 
#' This function uses the out-of-bag predictions to calculate overall
#' performance metrics and returns the observed and predicted data.
#' 
#' The mean and standard deviation of the values produced by
#' \code{\link{postResample}} are calculated.
#' 
#' @param obs A vector (numeric or factor) of the outcome data
#' @param resampled For bootstrapping, this is either a matrix (for numeric
#' outcomes) or a data frame (for factors). For cross-validation, a vector is
#' produced.
#' @param index The list to index of samples in each cross-validation fold
#' (only used for cross-validation).
#' @param keepData A logical for returning the observed and predicted data.
#' @return A list with: \item{metrics }{A vector of values describing the
#' bootstrap distribution.} \item{data }{A data frame or \code{NULL}. Columns
#' include \code{obs}, \code{pred} and \code{group} (for tracking
#' cross-validation folds or bootstrap samples)}
#' @author Max Kuhn
#' @seealso \code{\link{postResample}}
#' @keywords utilities
#' @examples
#' 
#' resampleSummary(rnorm(10), matrix(rnorm(50), ncol = 5))
#' 
#' @export resampleSummary
resampleSummary <- function(obs, resampled, index = NULL, keepData = TRUE)
{
   numPred <- apply(resampled, 2, function(u) sum(!is.na(u)))
   # for everything but LOO, we should get multiple predictions per resample
   if(all(numPred >= 2))
      {
           
         # calculate performance metrics for each resample
         performanceStats <- apply(
            resampled, 
            2, 
            postResample, 
            obs = obs)
            
         #summarize resample dists
         out <- c(
            apply(performanceStats, 1, mean, na.rm = TRUE), 
            apply(performanceStats, 1, sd, na.rm = TRUE))
            
         # optionally returen the data in "vertical" format
         # to conserve space remove the many missing values
         if(keepData)
         {
            # stack has issues when there are a lot of missing values,
            # so we'll use lapply to stack the columns of resampled
            if(is.factor(obs))
            {
               outResample <- data.frame(
                  obs = rep(obs, dim(resampled)[2]),
                  pred = factor(unlist(lapply(resampled, as.character)), 
                     levels = levels(obs)),
                  group = paste(
                     "Resample", 
                     rep(
                        1:dim(resampled)[2], 
                        each = dim(resampled)[1], sep = "")))
            } else {
               outResample <- data.frame(
                  obs = rep(obs, dim(resampled)[2]),
                  pred = unlist(lapply(resampled, I)), 
                  group = paste(
                     "Resample", 
                     rep(
                        1:dim(resampled)[2], 
                        each = dim(resampled)[1], sep = "")))     
            }
        } else outResample <- NULL
        
      } else {  
      

         pred <- apply(resampled, 2, function(u) u[!is.na(u)])
         if(is.factor(obs)) pred <- factor(as.character(pred), levels = levels(obs))
         
         tmp <- postResample(pred, obs)
         tmp2 <- tmp * 0
         
         out <- c(
            tmp, 
            tmp * 0) 
        
         outResample <- data.frame(
            obs = obs,
            pred = pred,
            group = "Resample1")
      }
      
   if(keepData) outResample <- outResample[!is.na(outResample$pred),] 
   list(metrics = out, data = outResample)
}

