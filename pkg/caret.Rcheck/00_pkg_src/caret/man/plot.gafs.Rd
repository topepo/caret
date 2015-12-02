\name{plot.gafs}
\alias{plot.safs}
\alias{plot.gafs}
\title{
Plot Method for the gafs and safs Classes
}
\description{
Plot the performance values versus search iteration
}
\usage{
\method{plot}{gafs}(x, metric = x$control$metric["external"], 
     estimate = c("internal", "external"), output = "ggplot", ...)

\method{plot}{safs}(x, metric = x$control$metric["external"], 
     estimate = c("internal", "external"), output = "ggplot", ...)
}

\arguments{
  \item{x}{an object of class \code{\link{gafs}} or \code{\link{safs}}}
  \item{metric}{the measure of performance to plot (e.g. RMSE, accuracy, etc)}
  \item{estimate}{the type of estimate: either "internal" or "external"}
  \item{output}{either "data", "ggplot" or "lattice"}
  \item{\dots}{options passed to \code{\link[lattice]{xyplot}}}
}

\details{
The mean (averaged over the resamples) is plotted against the search iteration using a scatter plot. 

When \code{output = "data"}, the unaveraged data are returned with columns for all the performance metrics and the resample indicator. 
}

\value{
Either a data frame, ggplot object or lattice object
}

\author{
Max Kuhn
}

\seealso{\code{\link{gafs}}, \code{\link{safs}}, \code{\link[ggplot2]{ggplot}}, \code{\link[lattice]{xyplot}} }

\examples{
\dontrun{
set.seed(1)
train_data <- twoClassSim(100, noiseVars = 10)
test_data  <- twoClassSim(10,  noiseVars = 10)

## A short example 
ctrl <- safsControl(functions = rfSA, 
                    method = "cv",
                    number = 3)

rf_search <- safs(x = train_data[, -ncol(train_data)],
                  y = train_data$Class,
                  iters = 50,
                  safsControl = ctrl)

plot(rf_search)
plot(rf_search, 
	 output = "lattice", 
	 auto.key = list(columns = 2))

plot_data <- plot(rf_search, output = "data")   
summary(plot_data)                 
    }
}
\keyword{hplot} 

