#' Wrapper for Lattice Plotting of Predictor Variables
#'
#' A shortcut to produce lattice graphs
#'
#' This function ``stacks'' data to get it into a form compatible with lattice
#' and creates the plots
#'
#' @param x a matrix or data frame of continuous feature/probe/spectra data.
#' @param y a factor indicating class membership.
#' @param plot the type of plot. For classification: \code{box}, \code{strip},
#' \code{density}, \code{pairs} or \code{ellipse}.  For regression,
#' \code{pairs} or \code{scatter}
#' @param labels a bad attempt at pre-defined axis labels
#' @param \dots options passed to lattice calls.
#' @return An object of class ``trellis''. The `update' method can be used to
#' update components of the object and the `print' method (usually called by
#' default) will plot it on an appropriate plotting device.
#' @author Max Kuhn
#' @keywords hplot
#' @examples
#'
#' x <- matrix(rnorm(50*5),ncol=5)
#' y <- factor(rep(c("A", "B"),  25))
#'
#' trellis.par.set(theme = col.whitebg(), warn = FALSE)
#' featurePlot(x, y, "ellipse")
#' featurePlot(x, y, "strip", jitter = TRUE)
#' featurePlot(x, y, "box")
#' featurePlot(x, y, "pairs")
#'
#' @export featurePlot
"featurePlot" <-
function(x, y,
   plot = if(is.factor(y)) "strip" else "scatter",
   labels = c("Feature", ""), ...)
{
   if(!is.data.frame(x))  x <- as.data.frame(x, stringsAsFactors = TRUE)
   numFeat <- dim(x)[2]

   if(plot != "pairs")
   {
      stackX <- stack(x)
      stackX$.y <- rep(y, numFeat)
   } else {
      if(!is.factor(y))
      {
         x <- data.frame(cbind(x, y))
      }
   }

   if(is.factor(y))
   {
      featPlot <- switch(tolower(plot),
         strip = stripplot(values ~ .y|ind, stackX,
            xlab = labels[1], ylab = labels[2], ...),
         box =, boxplot = bwplot(values ~ .y|ind, stackX,
            xlab = labels[1], ylab = labels[2], ...),
         density = densityplot(~values |ind, stackX,
            groups = stackX$.y,
            xlab = labels[1], ylab = labels[2], ...),
         pairs = splom(~x, groups = y, ...),
         ellipse =  splom(~x, groups = y,
            panel = function(x, y, groups, subscripts, ...)
            {
               requireNamespaceQuietStop("ellipse")
               lineInfo <-  trellis.par.get("superpose.line")
               pointInfo <-  trellis.par.get("superpose.symbol")
               uniqueGroups <- sort(unique(groups))
               for (i in seq(along=uniqueGroups))
               {
                  id <- which(groups[subscripts] == uniqueGroups[i])
                  panel.xyplot(x[id], y[id], pch = pointInfo$pch[i],
                      col = pointInfo$col[i], cex = pointInfo$cex[i], ...)
                  groupVar<-var(cbind(x[id],y[id]))
                  groupMean<-cbind(mean(x[id]),mean(y[id]))
                  groupEllipse<-ellipse::ellipse(groupVar, centre = groupMean, level = 0.95)
                  panel.xyplot(groupEllipse[,1], groupEllipse[,2], type="l", col = lineInfo$col[i], lty = lineInfo$lty[i], ...)
               }
            },
         ...)
      )
   } else {
      featPlot <- switch(tolower(plot),
         scatter =, xyplot = xyplot(.y ~ values|ind, stackX,
            scales = list( x = list(relation = "free")),
            xlab = labels[1], ylab = labels[2], ...),
         pairs = splom(~x,  ...))
   }

   featPlot
}

