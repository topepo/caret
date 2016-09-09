#' Plot the resampling distribution of the model statistics
#' 
#' Create a lattice histogram or densityplot from the resampled outcomes from a
#' \code{train} object.
#' 
#' All the metrics from the object are plotted, but only for the final model.
#' For more comprehensive plots functions, see \code{\link{histogram.train}},
#' \code{\link{densityplot.train}}, \code{\link{xyplot.train}},
#' \code{\link{stripplot.train}}.
#' 
#' For the plot to be made, the \code{returnResamp} argument in
#' \code{\link{trainControl}} should be either "final" or "all".
#' 
#' @param object an object resulting form a call to \code{\link{train}}
#' @param type a character string. Either "hist" or "density"
#' @param \dots options to pass to histogram or densityplot
#' @return a object of class \code{trellis}
#' @author Max Kuhn
#' @seealso \code{\link{train}}, \code{\link[lattice]{histogram}},
#' \code{\link[lattice:histogram]{densityplot}}, \code{\link{histogram.train}},
#' \code{\link{densityplot.train}}, \code{\link{xyplot.train}},
#' \code{\link{stripplot.train}}
#' @keywords hplot
#' @examples
#' 
#' 
#' \dontrun{
#' data(iris)
#' TrainData <- iris[,1:4]
#' TrainClasses <- iris[,5]
#' 
#' knnFit <- train(TrainData, TrainClasses, "knn")
#' 
#' resampleHist(knnFit)
#' }
#' 
#' @export resampleHist
resampleHist <- function(object, type = "density", ...)
{
  if(object$control$method == "oob") stop("out-of-bag error rate was selected. This plot cannot be created")
  if(is.null(object$resample)) stop("No resample values were found. This plot cannot be created")


  resample <- object$resample
  tuneNames <- as.character(object$modelInfo$parameter$parameter)
  if(any(names(resample) %in% tuneNames))
    {
      bestTune <- object$bestTune
      colnames(bestTune) <- gsub("^\\.", "", colnames(bestTune))
      resample <- merge(bestTune, resample)        
      resample <- resample[, !(names(resample) %in% tuneNames), drop = FALSE]

    }
  results <- melt(resample, id.vars = "Resample")
  
  if(type == "density")
    {
      out <- densityplot(~ value|variable, 
                         data = results, 
                         scales = list(relation = "free"),
                         xlab = "",
                         as.table = TRUE,
                         ...)  

    } else {
      out <- histogram(~ value|variable, 
                       data = results, 
                       scales = list(relation = "free"),
                       as.table = TRUE,         
                       xlab = "",
                       ...)    
    }
  out
}
