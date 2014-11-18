\name{predict.knnreg}
\alias{predict.knnreg}
\title{ Predictions from k-Nearest Neighbors Regression Model }
\description{
  Predict the outcome of a new observation based on k-NN. 
}
\usage{
\method{predict}{knnreg}(object, newdata, ...)
}
\arguments{
  \item{object}{object of class \code{knnreg}.}
  \item{newdata}{a data frame or matrix of new observations. }
  \item{...}{additional arguments.}
}
\details{
  This function is a method for the generic function \code{\link{predict}}
for class \code{knnreg}. For the details see \code{\link{knnreg}}. This is essentially
a copy of \code{\link[ipred]{predict.ipredknn}}.

}
\value{
a numeric vector
}

\author{Max Kuhn, Chris Keefer, adapted from \code{\link[class]{knn}} and  \code{\link[ipred]{predict.ipredknn}}}

\keyword{multivariate}
