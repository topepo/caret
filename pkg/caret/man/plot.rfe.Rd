\name{plot.rfe}
\alias{plot.rfe}
\alias{ggplot.rfe}

\title{
Plot RFE Performance Profiles
}
\description{
These functions plot the resampling results for the candidate subset sizes evaluated during the recursive feature elimination (RFE) process
}
\usage{
\method{plot}{rfe}(x, metric = x$metric,  ...)

\method{ggplot}{rfe}(data = NULL, metric = data$metric[1], output = "layered", ...)

}
\arguments{
  \item{x}{
an object of class \code{\link{rfe}}.
}

 \item{metric}{What measure of performance to plot. Examples of possible values are "RMSE", "Rsquared", "Accuracy" or "Kappa". Other values can be used depending on what metrics have been calculated.}

  \item{\dots}{\code{plot} only: specifications to be passed to \code{\link[lattice]{xyplot}}. The function automatically sets some arguments (e.g. axis labels) but passing in values here will over-ride the defaults. 
 }
  \item{data}{
an object of class \code{\link{rfe}}.
}
  \item{output}{either "data", "ggplot" or "layered". The first returns a data frame while the second returns a simple \code{ggplot} object with no layers. The third value returns a plot with a set of layers. }  
}
\details{
These plots show the average performance versus the subset sizes. 
}
\value{
a lattice or ggplot object
}
\author{Max Kuhn}

\references{Kuhn (2008), ``Building Predictive Models in R Using the caret'' (\url{http://www.jstatsoft.org/v28/i05/})}

\seealso{ \code{\link{rfe}}, \code{\link[lattice]{xyplot}}, \code{\link[ggplot2]{ggplot}}}

\examples{
\dontrun{
data(BloodBrain)

x <- scale(bbbDescr[,-nearZeroVar(bbbDescr)])
x <- x[, -findCorrelation(cor(x), .8)]
x <- as.data.frame(x)

set.seed(1)
lmProfile <- rfe(x, logBBB,
                 sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
                 rfeControl = rfeControl(functions = lmFuncs, 
                                         number = 200))
plot(lmProfile)
plot(lmProfile, metric = "Rsquared")
ggplot(lmProfile)
}
}
\keyword{hplot} 

