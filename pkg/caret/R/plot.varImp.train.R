#' Plotting variable importance measures
#' 
#' This function produces lattice and ggplot plots of objects with class
#' "varImp.train". More info will be forthcoming.
#' 
#' For models where there is only one importance value, such a regression
#' models, a "Pareto-type" plot is produced where the variables are ranked by
#' their importance and a needle-plot is used to show the top variables.
#' Horizontal bar charts are used for \code{ggplot}.
#' 
#' When there is more than one importance value per predictor, the same plot is
#' produced within conditioning panels for each class. The top predictors are
#' sorted by their average importance.
#' 
#' @aliases plot.varImp.train ggplot.varImp.train
#' @param x,data an object with class \code{varImp}.
#' @param top a scalar numeric that specifies the number of variables to be
#' displayed (in order of importance)
#' @param \dots arguments to pass to the lattice plot function
#' (\code{\link[lattice:xyplot]{dotplot}} and \code{\link{panel.needle}})
#' @param mapping,environment unused arguments to make consistent with
#' \pkg{ggplot2} generic method
#' @return a lattice plot object
#' @author Max Kuhn
#' @keywords hplot
#' @method plot varImp.train
#' @export

"plot.varImp.train" <-
function(x, top = dim(x$importance)[1],  ...)
{  
   plotObj <- sortImp(x, top)
          
   if(ncol(plotObj) == 2) {
      plotObj <- plotObj[,1,drop = FALSE]
      names(plotObj) <- "Importance"
   }
             
   featureNames <- rownames(plotObj)         
   outcomeNames <- colnames(plotObj)
   
   if(ncol(plotObj) > 1) {            
      stackedData <- stack(plotObj)
      stackedData$Feature <- factor(rep(featureNames, length(outcomeNames)),
                                    levels = rev(featureNames))      
      names(stackedData) <- c("Importance", "Class", "Feature")      
   } else {
      stackedData <- plotObj
      stackedData$Feature <- factor(rep(featureNames, length(outcomeNames)),
                                    levels = rev(featureNames))            
      names(stackedData) <- c("Importance", "Feature")
   }
   
   formulaText <- ifelse(ncol(plotObj)> 1, 
                         "Feature ~ Importance|Class", "
                         Feature ~ Importance")
   
   if(x$model == "pam")
   {
     impSign <- factor(ifelse(stackedData$Importance > 0, "Positive", "Negative"),
                       levels = c("Positive", "Negative"))
     stackedData$Importance <- abs(stackedData$Importance)
     impPlot <- dotplot(as.formula(formulaText), stackedData,
                        groups = impSign,
                        panel = panel.needle, ...)   
   } else {
     impPlot <- dotplot(as.formula(formulaText), stackedData,
                        panel = panel.needle, ...)
   }
   impPlot
}

#' @rdname plot.varImp.train
#' @importFrom utils stack
#' @export
ggplot.varImp.train <- function (data, mapping = NULL, top = dim(data$importance)[1], ..., environment = NULL)  {
  plotObj <- sortImp(data, top)
  if (ncol(plotObj) == 2) {
    plotObj <- plotObj[, 1, drop = FALSE]
    names(plotObj) <- "Importance"
  }
  featureNames <- rownames(plotObj)
  outcomeNames <- colnames(plotObj)
  if (ncol(plotObj) > 1) {
    stackedData <- stack(plotObj)
    stackedData$Feature <- factor(rep(featureNames, length(outcomeNames)), 
                                  levels = rev(featureNames))
    names(stackedData) <- c("Importance", "Class", "Feature")
    ## to avoid R CMD check warnings: 
#     ggplot.varImp.train: no visible binding for global variable 'Feature'
#     ggplot.varImp.train: no visible binding for global variable 'Importance'
    Feature <- Importance <- NULL
    out <-  ggplot(stackedData, aes(x = Feature, y = Importance))+ 
      geom_bar(stat = "identity") + facet_wrap(~Class) + 
      coord_flip()
  } else {
    stackedData <- plotObj
    stackedData$Feature <- factor(rep(featureNames, length(outcomeNames)), 
                                  levels = rev(featureNames))
    names(stackedData) <- c("Importance", "Feature")
    out <-  ggplot(stackedData, aes(x = Feature, y = Importance))+ 
      geom_bar(stat = "identity") + 
      coord_flip()
  }
  out
}

