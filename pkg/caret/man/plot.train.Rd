\name{plot.train}
\alias{plot.train}
\alias{ggplot.train}
\title{Plot Method for the train Class}
\description{
  This function takes the output of a \code{\link{train}} object and creates
      a line or level plot using the \pkg{lattice} or \pkg{ggplot2} libraries.
}
\usage{
\method{plot}{train}(x, 
     plotType = "scatter",
     metric = x$metric[1],
     digits = getOption("digits") - 3, 
     xTrans = NULL, 
     nameInStrip = FALSE,
     ...)

\method{ggplot}{train}(data = NULL, 
       metric = data$metric[1], 
       plotType = "scatter", 
       output = "layered", 
       nameInStrip = FALSE,
       ...) 
   
}
\arguments{
  \item{x}{an object of class \code{\link{train}}.}
  \item{metric}{What measure of performance to plot. Examples of possible values are "RMSE", "Rsquared", "Accuracy" or "Kappa". Other values can be used depending on what metrics have been calculated.}
  \item{plotType}{a string describing the type of plot (\code{"scatter"}, \code{"level"} or \code{"line"} (\code{plot} only))}
  \item{digits}{an integer specifying the number of significant digits used to label the parameter value.}
  \item{xTrans}{a function that will be used to scale the x-axis in scatter plots.}
  \item{data}{an object of class \code{\link{train}}.}  
  \item{output}{either "data", "ggplot" or "layered". The first returns a data frame while the second returns a simple \code{ggplot} object with no layers. The third value returns a plot with a set of layers. }  
  \item{nameInStrip}{a logical: if there are more than 2 tuning parameters, should the name and value be included in the panel title?}
  \item{\dots}{\code{plot} only: specifications to be passed to \code{\link[lattice]{levelplot}}, \code{\link[lattice]{xyplot}}, \code{\link[lattice:xyplot]{stripplot}} (for line plots). The function automatically sets some arguments (e.g. axis labels) but passing in values here will over-ride the defaults }
}
\details{
If there are no tuning parameters, or none were varied, an error is produced.
  
If the model has one tuning parameter with multiple candidate values, a
plot is produced showing the profile of the results over the
parameter. Also, a plot can be produced if there are multiple
tuning parameters but only one is varied.

If there are two tuning parameters with different values, a 
plot can be produced where a different line is shown for each value of
of the other parameter. For three parameters, the same line plot is
created within conditioning panels/facets of the other parameter.

Also, with two tuning parameters (with different values), a levelplot
(i.e. un-clustered heatmap) can be created. For more than two
parameters, this plot is created inside conditioning panels/facets.

  
}
\author{Max Kuhn}

\references{Kuhn (2008), ``Building Predictive Models in R Using the caret'' (\url{http://www.jstatsoft.org/v28/i05/})}

\seealso{ \code{\link{train}}, \code{\link[lattice]{levelplot}}, \code{\link[lattice]{xyplot}}, \code{\link[lattice:xyplot]{stripplot}},  \code{\link[ggplot2]{ggplot}}}
\examples{

\dontrun{
library(klaR)
rdaFit <- train(Species ~ .,
                data = iris, 
                method = "rda", 
                control = trainControl(method = "cv"))
plot(rdaFit)
plot(rdaFit, plotType = "level")

ggplot(rdaFit) + theme_bw()

}
 }
   
\keyword{hplot} 
