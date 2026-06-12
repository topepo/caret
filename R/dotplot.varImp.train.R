#' Create a dotplot of variable importance values
#' 
#' A lattice \code{\link[lattice:xyplot]{dotplot}} is created from an object of
#' class \code{varImp.train}.
#' 
#' 
#' @param x an object of class \code{varImp.train}
#' @param top the number of predictors to plot
#' @param \dots options passed to \code{\link[lattice:xyplot]{dotplot}}
#' @return an object of class \code{trellis}.
#' @author Max Kuhn
#' @seealso \code{\link{varImp}}, \code{\link[lattice:xyplot]{dotplot}}
#' @keywords hplot
#' @examples
#' 
#' 
#' data(iris)
#' TrainData <- iris[,1:4]
#' TrainClasses <- iris[,5]
#' 
#' knnFit <- train(TrainData, TrainClasses, "knn")
#' 
#' knnImp <- varImp(knnFit)
#' 
#' dotPlot(knnImp)
#' 
#' 
#' @export dotPlot
dotPlot <- function (x, top = min(20, dim(x$importance)[1]), ...) 
{
   varSubset <- sortImp(x, top)
   plotObj <- stack(varSubset)

   if(dim(varSubset)[2] == 1)
   {
      plotObj <- varSubset
      names(plotObj) <- "values"
      plotObj$ind <- "Overall"
   } else plotObj <- stack(varSubset)
   
   plotObj$Var <- rep(rownames(varSubset), dim(varSubset)[2])
   plotObj$Var <- factor(plotObj$Var, levels = rev(rownames(varSubset)))
   if(dim(varSubset)[2] < 3)
   {
      if(dim(varSubset)[2] > 1) plotObj <- plotObj[plotObj$ind == levels(plotObj$ind)[1],]
      out <- dotplot(
         Var ~ values, 
         data = plotObj, 
         as.table = TRUE, 
         xlab = "Importance",
         ...)   
   
   } else {
      out <- dotplot(
         Var ~ values, 
         data = plotObj, 
         groups = plotObj$ind, 
         auto.key = list(columns = min(3, length(levels(plotObj$ind)))), 
         as.table = TRUE, 
         xlab = "Importance",
         ...)   
   }
   out
}

