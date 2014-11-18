plotObsVsPred <- function(object, equalRanges = TRUE, ...)
{
  
   object <- object[object$dataType != "Unknown",]
   object$dataType <- factor(object$dataType)

   if(is.factor(object$obs))
   {
      agreement <- object$obs == object$pred
      accuracyTable <- by(agreement, list(model = object$model, data = object$dataType), mean)
      accuracyDF <- data.frame(unclass(accuracyTable))
      accuracyStacked <- stack(accuracyDF)
      accuracyStacked$model <- rep(dimnames(accuracyDF)[[1]], dim(accuracyDF)[2])
      names(accuracyStacked) <- c("Accuracy", "Data", "Model")
      accuracyStacked$Data <- factor(
         ifelse(accuracyStacked$Data == "Training", "Training (uncorrected)", 
         as.character(accuracyStacked$Data)))
      out <- dotplot(Model ~ Accuracy, accuracyStacked, groups = accuracyStacked$Data, ...)      
   } else {
      
      if(equalRanges)
      {
         xLimits <- yLimits <- extendrange(c(object$obs, object$pred))      
      } else {
         xLimits <- extendrange(object$obs)      
         yLimits <- extendrange(object$pred)            
      }
      
      out <- xyplot(obs ~ pred|model * dataType, object, 
         xlim = xLimits, ylim = yLimits,
         panel = function(x, y, groups, subscripts, ...)
         {      
            panel.xyplot(x, y,  cex = .6)
            panel.abline(0, 1, col = trellis.par.get("superpose.line")$col[1], lty = 2)            
            panel.loess(x, y,  span = .75)
         },
         xlab = "Predicted", ylab = "Observed", ...)
   }
   out

}

