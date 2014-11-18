\name{summary.bagEarth}
\alias{summary.bagEarth}
\alias{summary.bagFDA}
\title{Summarize a bagged earth or FDA fit}
\description{
The function shows a summary of the results from a bagged earth model
}
\usage{
\method{summary}{bagEarth}(object, ...)
\method{summary}{bagFDA}(object, ...)

}
\arguments{
  \item{object}{an object of class "bagEarth" or "bagFDA"}
  \item{\dots}{optional arguments (not used)}
}
\details{
The out-of-bag statistics are summarized, as well as the distribution of the number of model terms and number of variables used across all the bootstrap samples.
}
\value{
a list with elements
  \item{modelInfo}{a matrix with the number of model terms and variables used}
  \item{oobStat }{a summary of the out-of-bag statistics}
  \item{bmarsCall }{the original call to \code{bagEarth}}
  
}
\author{Max Kuhn}

\examples{
\dontrun{
data(trees)
fit <- bagEarth(trees[,-3], trees[3])
summary(fit)
}
}
\keyword{manip }
