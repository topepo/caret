\name{lattice.rfe}
\Rdversion{1.1}
\alias{xyplot.rfe}
\alias{stripplot.rfe}
\alias{densityplot.rfe}
\alias{histogram.rfe}
\title{Lattice functions for plotting resampling results of recursive feature selection}
\description{
A set of lattice functions are provided to plot the resampled performance estimates (e.g. classification accuracy, RMSE) over different subset sizes.
}
\usage{
\method{histogram}{rfe}(x, data = NULL, metric = x$metric, ...)

\method{densityplot}{rfe}(x, data = NULL, metric = x$metric, ...)

\method{xyplot}{rfe}(x, data = NULL, metric = x$metric, ...)

\method{stripplot}{rfe}(x, data = NULL, metric = x$metric, ...)
}
\arguments{
  \item{x}{ An object produced by  \code{\link{rfe}} }
  \item{data}{This argument is not used }
  \item{metric}{A character string specifying  the single performance metric that will be plotted }
  \item{\dots}{arguments to pass to either  \code{\link[lattice:histogram]{histogram}}, \code{\link[lattice:histogram]{densityplot}}, \code{\link[lattice:xyplot]{xyplot}} or \code{\link[lattice:xyplot]{stripplot}}}
}

\details{
By default, only the resampling results for the optimal model are saved in the \code{rfe} object. The function \code{\link{rfeControl}} can be used to save all the results using the \code{returnResamp} argument.

If leave-one-out or out-of-bag resampling was specified, plots cannot be produced (see the \code{method} argument of \code{\link{rfeControl}})

}
\value{A lattice plot object}
\author{Max Kuhn}

\seealso{\code{\link{rfe}}, \code{\link{rfeControl}}, \code{\link[lattice:histogram]{histogram}}, \code{\link[lattice:histogram]{densityplot}}, \code{\link[lattice:xyplot]{xyplot}}, \code{\link[lattice:xyplot]{stripplot}}}

\examples{
\dontrun{
library(mlbench)
n <- 100
p <- 40
sigma <- 1
set.seed(1)
sim <- mlbench.friedman1(n, sd = sigma)
x <- cbind(sim$x,  matrix(rnorm(n * p), nrow = n))
y <- sim$y
colnames(x) <- paste("var", 1:ncol(x), sep = "")

normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)
subsets <- c(10, 15, 20, 25)

ctrl <- rfeControl(
                   functions = lmFuncs,
                   method = "cv",
                   verbose = FALSE,
                   returnResamp = "all")

lmProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)
xyplot(lmProfile)
stripplot(lmProfile)

histogram(lmProfile)
densityplot(lmProfile)
}
}


\keyword{hplot} 
