\name{findCorrelation}
\alias{findCorrelation}
\title{Determine highly correlated variables}
\description{
This function searches through a correlation matrix and returns a vector of integers
corresponding to columns to remove to reduce pair-wise correlations.
}
\usage{
findCorrelation(x, cutoff = .90, verbose = FALSE)
}

\arguments{
  \item{x}{A correlation matrix}
  \item{cutoff}{A numeric value for the pair-wise absolute correlation cutoff}
  \item{verbose}{A boolean for printing the details}
}
\details{
   The absolute values of pair-wise correlations are considered. If two variables have a high correlation, 
   the function looks at the mean absolute correlation of each variable and removes the variable with the
   largest mean absolute correlation.

   There are several function in the \pkg{subselect} package (\code{\link[subselect:eleaps]{leaps}}, \code{\link[subselect:genetic]{genetic}}, \code{\link[subselect:anneal]{anneal}}) that can also be used
   to accomplish the same goal.
}
\value{
  A vector of indices denoting the columns to remove. If no correlations meet the criteria, \code{numeric(0)} is returned.
}
\author{Original R code by Dong Li, modified by Max Kuhn}

\seealso{\code{\link[subselect:eleaps]{leaps}}, \code{\link[subselect:genetic]{genetic}}, \code{\link[subselect:anneal]{anneal}}, \code{\link{findLinearCombos}}}

\examples{
corrMatrix <- diag(rep(1, 5))
corrMatrix[2, 3] <- corrMatrix[3, 2] <- .7
corrMatrix[5, 3] <- corrMatrix[3, 5] <- -.7
corrMatrix[4, 1] <- corrMatrix[1, 4] <- -.67

corrDF <- expand.grid(row = 1:5, col = 1:5)
corrDF$correlation <- as.vector(corrMatrix)
levelplot(correlation ~ row+ col, corrDF)

findCorrelation(corrMatrix, cutoff = .65, verbose = TRUE)

findCorrelation(corrMatrix, cutoff = .99, verbose = TRUE)

\dontshow{
   removeCols <- findCorrelation(corrMatrix, cutoff = .65, verbose = FALSE)
   if(!isTRUE(all.equal(corrMatrix[-removeCols, -removeCols], diag(rep(1, 3))))) stop("test 1 failed")
   if(!isTRUE(all.equal( findCorrelation(corrMatrix, .99, verbose = FALSE), numeric(0)))) stop("test 2 failed")
   }
}
\keyword{manip}
