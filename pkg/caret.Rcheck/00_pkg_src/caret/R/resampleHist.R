
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
