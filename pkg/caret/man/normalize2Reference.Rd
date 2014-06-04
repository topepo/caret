\name{normalize2Reference}
\alias{normalize2Reference}
\title{Quantile Normalize Columns of a Matrix Based on a Reference Distribution}
\description{
Normalize the columns of a matrix to have the same quantiles, allowing for missing values.
Users do not normally need to call this function directly - use \code{normalize.AffyBatch.normalize2Reference} instead.
}
\usage{
normalize2Reference(data, refData = NULL, ties = TRUE)
}
\arguments{
  \item{data}{numeric matrix. Missing values are allowed.}
  \item{refData}{A vector of reference values.}  
  \item{ties}{logical. If \code{TRUE}, ties in each column of \code{A} are treated in careful way. Tied values will be normalized to the mean of the corresponding pooled quantiles.}
}
\details{
This function is intended to normalize single channel or A-value microarray intensities between arrays.
Each quantile of each column is set to either: quantiles of the reference distribution (\code{refData} supplied) or the mean of that quantile across arrays (\code{refData} is \code{NULL}) .
The intention is to make all the normalized columns have the same empirical distribution.
This will be exactly true if there are no missing values and no ties within the columns: the normalized columns are then simply permutations of one another.

If there are ties amongst the intensities for a particular array, then with \code{ties=FALSE} the ties are broken in an unpredictable order.
If \code{ties=TRUE}, all the tied values for that array will be normalized to the same value, the average of the quantiles for the tied values.
}
\value{
A matrix of the same dimensions as \code{A} containing the normalized values.
}
\references{
Bolstad, B. M., Irizarry R. A., Astrand, M., and Speed, T. P. (2003), A comparison of normalization methods for high density oligonucleotide array data based on bias and variance. \emph{Bioinformatics} \bold{19}, 185-193.
}
\author{Max Kuhn, adapted from Gordon Smyth}
\seealso{
\code{normalize.AffyBatch.normalize2Reference}
}  
\keyword{models}
