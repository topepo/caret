\name{BoxCoxTrans.default}
\alias{BoxCoxTrans.default}
\alias{BoxCoxTrans}
\alias{predict.BoxCoxTrans}
\alias{expoTrans.default}
\alias{expoTrans}
\alias{predict.expoTrans}

\title{
Box-Cox and Exponential Transformations
}
\description{
These classes can be used to estimate transformations and apply them to existing and future data
}
\usage{
BoxCoxTrans(y, ...)
expoTrans(y, ...)

\method{BoxCoxTrans}{default}(y, x = rep(1, length(y)), 
            fudge = 0.2, numUnique = 3, na.rm = FALSE, ...)
\method{expoTrans}{default}(y, na.rm  = TRUE, init = 0, 
     lim = c(-4, 4), method = "Brent",
     numUnique = 3, ...)

\method{predict}{BoxCoxTrans}(object, newdata, ...)
\method{predict}{expoTrans}(object, newdata, ...)
}
\arguments{
  \item{y}{a numeric vector of data to be transformed. For \code{BoxCoxTrans}, the data must be strictly positive.
}
  \item{x}{
an optional dependent variable to be used in a linear model.
}
  \item{fudge}{
a tolerance value: lambda values within +/-fudge will be coerced to 0 and within 1+/-fudge will be coerced to 1.
}
  \item{numUnique}{
how many unique values should \code{y} have to estimate the transformation?
}
  \item{na.rm}{a logical value indicating whether \code{NA} values should be stripped from \code{y} and \code{x} before the computation proceeds.}
  \item{init, lim, method}{initial values, limits and optimization method for \code{\link{optim}}.}

  \item{\dots}{
for \code{BoxCoxTrans}: options to pass to \code{\link[MASS]{boxcox}}. \code{plotit} should not be passed through. For \code{predict.BoxCoxTrans}, additional arguments are ignored.
}
  \item{object}{
an object of class \code{BoxCoxTrans} or \code{expoTrans}.
}
  \item{newdata}{
a numeric vector of values to transform.
}
}
\details{
\code{BoxCoxTrans} function is basically a wrapper for the \code{\link[MASS]{boxcox}} function in the MASS library. It can be used to estimate the transformation and apply it to new data. 

\code{expoTrans} estimates the exponential transformation of Manly (1976) but assumes a common mean for the data. The transformation parameter is estimated by directly maximizing the likelihood. 

If \code{any(y <= 0)} or  if \code{length(unique(y)) < numUnique}, lambda is not estimated and no transformation is applied.

}
\value{
Both functions returns a list of class of either \code{BoxCoxTrans} or \code{expoTrans} with elements
  \item{lambda }{estimated transformation value}
  \item{fudge }{value of \code{fudge}}
  \item{n }{number of data points used to estimate lambda}
  \item{summary }{the results of \code{summary(y)}}
  \item{ratio }{\code{max(y)/min(y)}}
  \item{skewness }{sample skewness statistic}

 \code{BoxCoxTrans} also returns:
  \item{fudge }{value of \code{fudge}}
 

The \code{predict} functions returns numeric vectors of transformed values
}
\references{
Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations (with discussion). Journal of the Royal Statistical Society B, 26, 211-252.

Manly, B. L. (1976) Exponential data transformations. The Statistician, 25, 37 - 42.
}

\author{
Max Kuhn
}

\seealso{
\code{\link[MASS]{boxcox}}, \code{\link{preProcess}}, \code{\link{optim}}
}
\examples{
data(BloodBrain)

ratio <- exp(logBBB)
bc <- BoxCoxTrans(ratio)
bc

predict(bc, ratio[1:5])

ratio[5] <- NA
bc2 <- BoxCoxTrans(ratio, bbbDescr$tpsa, na.rm = TRUE)
bc2

manly <- expoTrans(ratio)
manly


}
\keyword{ utilities }
