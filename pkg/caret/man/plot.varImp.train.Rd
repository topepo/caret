\name{plot.varImp.train}
\alias{plot.varImp.train}

\title{Plotting variable importance measures}
\description{
This function produces lattice plots of objects with class "varImp.train". More info will be forthcoming.
}

\usage{
\method{plot}{varImp.train}(x, top = dim(x$importance)[1],  ...)
}

\arguments{
  \item{x}{an object with class \code{varImp}. }
  \item{top}{a scalar numeric that specifies the number of variables to be displayed (in order of importance)}
  \item{\dots}{arguments to pass to the lattice plot function (\code{\link[lattice:xyplot]{dotplot}} and \code{\link{panel.needle}})}
}
\details{
For models where there is only one importance value, such a regression
models, a "Pareto-type" plot is produced where the variables are ranked
by their importance and a needle-plot is used to show the top variables.

When there is more than one importance value per predictor, the same
plot is produced within conditioning panels for each class. The top
predictors are sorted by their average importance.
}
\value{
  a lattice plot object
}

\author{Max Kuhn}

\keyword{hplot}

