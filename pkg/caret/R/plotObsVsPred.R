#' Plot Observed versus Predicted Results in Regression and Classification
#' Models
#' 
#' This function takes an object (preferably from the function
#' \code{\link{extractPrediction}}) and creates a lattice plot. For numeric
#' outcomes, the observed and predicted data are plotted with a 45 degree
#' reference line and a smoothed fit. For factor outcomes, a dotplot plot is
#' produced with the accuracies for the different models.
#' 
#' If the call to \code{\link{extractPrediction}} included test data, these
#' data are shown, but if unknowns were also included, they are not plotted
#' 
#' 
#' @param object an object (preferably from the function
#' \code{\link{extractPrediction}}. There should be columns named \code{obs},
#' \code{pred}, \code{model} (e.g. "rpart", "nnet" etc.)  and \code{dataType}
#' (e.g. "Training", "Test" etc)
#' @param equalRanges a logical; should the x- and y-axis ranges be the same?
#' @param \dots parameters to pass to \code{\link[lattice]{xyplot}} or
#' \code{\link[lattice:xyplot]{dotplot}}, such as \code{auto.key}
#' @return A lattice object. Note that the plot has to be printed to be
#' displayed (especially in a loop).
#' @author Max Kuhn
#' @keywords hplot
#' @examples
#' 
#' \dontrun{
#' # regression example
#' data(BostonHousing)
#' rpartFit <- train(BostonHousing[1:100, -c(4, 14)], 
#'                   BostonHousing$medv[1:100], 
#'                   "rpart", tuneLength = 9)
#' plsFit <- train(BostonHousing[1:100, -c(4, 14)], 
#'                 BostonHousing$medv[1:100], 
#'                 "pls")
#' 
#' predVals <- extractPrediction(list(rpartFit, plsFit), 
#'                               testX = BostonHousing[101:200, -c(4, 14)], 
#'                               testY = BostonHousing$medv[101:200], 
#'                               unkX = BostonHousing[201:300, -c(4, 14)])
#' 
#' plotObsVsPred(predVals)
#' 
#' 
#' #classification example
#' data(Satellite)
#' numSamples <- dim(Satellite)[1]
#' set.seed(716)
#' 
#' varIndex <- 1:numSamples
#' 
#' trainSamples <- sample(varIndex, 150)
#' 
#' varIndex <- (1:numSamples)[-trainSamples]
#' testSamples <- sample(varIndex, 100)
#' 
#' varIndex <- (1:numSamples)[-c(testSamples, trainSamples)]
#' unkSamples <- sample(varIndex, 50)
#' 
#' trainX <- Satellite[trainSamples, -37]
#' trainY <- Satellite[trainSamples, 37]
#' 
#' testX <- Satellite[testSamples, -37]
#' testY <- Satellite[testSamples, 37]
#' 
#' unkX <- Satellite[unkSamples, -37]
#' 
#' knnFit  <- train(trainX, trainY, "knn")
#' rpartFit <- train(trainX, trainY, "rpart")
#' 
#' predTargets <- extractPrediction(list(knnFit, rpartFit), 
#'                                  testX = testX, 
#'                                  testY = testY, 
#'                                  unkX = unkX)
#' 
#' plotObsVsPred(predTargets)
#' }
#' 
#' @export plotObsVsPred
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

