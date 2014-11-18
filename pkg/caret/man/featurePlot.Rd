\name{featurePlot}
\alias{featurePlot}
\title{Wrapper for Lattice Plotting of Predictor Variables}
\description{
A shortcut to produce lattice graphs
}
\usage{
featurePlot(x, y, 
            plot = if(is.factor(y)) "strip" else "scatter",
            labels = c("Feature", ""), 
            ...)
}
\arguments{
  \item{x}{a matrix or data frame of continuous feature/probe/spectra data.}
  \item{y}{a factor indicating class membership.}
  \item{plot}{the type of plot. For classification: \code{box},  \code{strip},  \code{density}, \code{pairs} or  \code{ellipse}. 
  For regression, \code{pairs} or \code{scatter}}
  \item{labels}{a bad attempt at pre-defined axis labels}
  \item{\dots}{options passed to lattice calls.}
}
\details{
This function ``stacks'' data to get it into a form compatible with lattice and creates the plots
}
\value{
   An object of class ``trellis''. The `update' method can be used to
     update components of the object and the `print' method (usually
     called by default) will plot it on an appropriate plotting device.

}

\author{Max Kuhn}

\examples{
x <- matrix(rnorm(50*5),ncol=5)
y <- factor(rep(c("A", "B"),  25))

trellis.par.set(theme = col.whitebg(), warn = FALSE)
featurePlot(x, y, "ellipse")
featurePlot(x, y, "strip", jitter = TRUE)
featurePlot(x, y, "box")
featurePlot(x, y, "pairs")
}
\keyword{hplot}
