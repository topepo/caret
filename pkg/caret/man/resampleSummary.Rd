\name{resampleSummary}
\alias{resampleSummary}
\title{Summary of resampled performance estimates}
\description{
This function uses the out-of-bag predictions to calculate
overall performance metrics and returns the observed and predicted data.
}
\usage{
resampleSummary(obs, resampled, index = NULL, keepData = TRUE)
}
\arguments{
  \item{obs}{A vector (numeric or factor) of the outcome data}
  \item{resampled}{ For bootstrapping, this is either a matrix (for numeric outcomes) or a data frame  (for factors). For cross-validation, a vector is produced. }
  \item{index}{The list to index of samples in each cross--validation fold (only used for cross-validation).}
  \item{keepData}{A logical for returning the observed and predicted data.}
}
\details{
   The mean and standard deviation of the values produced by \code{\link{postResample}} are calculated.}
\value{
A list with:
  \item{metrics }{A vector of values describing the bootstrap distribution.}
  \item{data }{A data frame or \code{NULL}. Columns include \code{obs}, \code{pred} 
   and \code{group} (for tracking cross-validation folds or bootstrap samples)}
}
\author{Max Kuhn}

\seealso{\code{\link{postResample}}}
\examples{
resampleSummary(rnorm(10), matrix(rnorm(50), ncol = 5))
}
\keyword{utilities}
