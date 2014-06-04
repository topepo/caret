\name{caretSBF}
\alias{caretSBF}
\alias{lmSBF}
\alias{rfSBF}
\alias{treebagSBF}
\alias{ldaSBF}
\alias{nbSBF}
\alias{gamScores}
\alias{anovaScores}


\title{Selection By Filtering (SBF) Helper Functions}
\description{
Ancillary functions for univariate feature selection
}
\usage{
anovaScores(x, y)
gamScores(x, y) 

caretSBF
lmSBF
rfSBF
treebagSBF
ldaSBF
nbSBF
}

\arguments{
  \item{x}{a matrix or data frame of numeric predictors}
  \item{y}{a numeric or factor vector of outcomes}
}
\details{
More details on these functions can be found at \url{http://caret.r-forge.r-project.org/featureselection.html}.

This page documents the functions that are used in selection by filtering (SBF). The functions described here are passed to the algorithm via the 
\code{functions} argument of \code{\link{sbfControl}}. 

See \code{\link{sbfControl}} for details on how these functions should be defined.

\code{anovaScores} and \code{gamScores} are two examples of univariate filtering functions. \code{anovaScores} fits a simple linear model between a single feature and the outcome, then the p-value for the whole model F-test is returned. \code{gamScores} fits a generalized additive model between a single predictor and the outcome using a smoothing spline basis function. A p-value is generated using the whole model test from \code{\link[gam]{summary.gam}} and is returned.

If a particular model fails for \code{lm} or \code{gam}, a p-value of 1 is returned.

}

\author{Max Kuhn}

\seealso{\code{\link{sbfControl}}, \code{\link{sbf}}, \code{\link[gam]{summary.gam}}}

\keyword{models}
