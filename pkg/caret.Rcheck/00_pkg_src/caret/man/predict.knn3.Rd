\name{predict.knn3}
\alias{predict.knn3}
\title{ Predictions from k-Nearest Neighbors }
\description{
  Predict the class of a new observation based on k-NN. 
}
\usage{
\method{predict}{knn3}(object, newdata, type=c("prob", "class"), ...)
}
\arguments{
  \item{object}{object of class \code{knn3}.}
  \item{newdata}{a data frame of new observations. }
  \item{type}{return either the predicted class or  
              the proportion of the votes for the winning
              class.}
  \item{...}{additional arguments.}
}
\details{
  This function is a method for the generic function \code{\link{predict}}
for class \code{knn3}. For the details see \code{\link{knn3}}. This is essentially
a copy of \code{\link[ipred]{predict.ipredknn}}.

}
\value{
Either the predicted class or 
the proportion of the votes for each class.
}

\author{\code{\link[ipred]{predict.ipredknn}} by Torsten.Hothorn <Torsten.Hothorn@rzmail.uni-erlangen.de> }

\keyword{multivariate}
