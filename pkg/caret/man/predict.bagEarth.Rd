\name{predict.bagEarth}
\alias{predict.bagEarth}
\alias{predict.bagFDA}
\title{Predicted values based on bagged Earth and FDA models}
\description{
Predicted values based on bagged Earth and FDA models
}
\usage{
\method{predict}{bagEarth}(object, newdata = NULL, type = "response", ...)
\method{predict}{bagFDA}(object, newdata = NULL, type = "class", ...)
}
\arguments{
  \item{object}{Object of class inheriting from \code{bagEarth}}
  \item{newdata}{ An optional data frame or matrix in which to look for variables with
          which to predict.  If omitted, the fitted values are used (see note below).}
\item{type}{The type of prediction. For bagged \code{\link[earth]{earth}} regression model, \code{type = "response"} will produce a numeric vector of the usual model predictions. \code{\link[earth]{earth}} also allows the user to fit generalized linear models. In this case, \code{type = "response"} produces the inverse link results as a vector. In the case of a binomial generalized linear model, \code{type = "response"}  produces a vector of probabilities, \code{type = "class"} generates a factor vector and \code{type = "prob"} produces a two-column matrix with probabilities for both classes (averaged across the individual models). Similarly, for bagged \code{\link[mda]{fda}} models, \code{type = "class"} generates a factor vector and \code{type = "probs"} outputs a matrix of class probabilities.}
  \item{\dots}{not used}
}

\value{
a vector of predictions
}
\author{Max Kuhn}
\note{
If the predictions for the original training set are needed, there are two ways to calculate them. First, the original data set can be predicted by each bagged earth model. Secondly, the predictions from each bootstrap sample could be used (but are more likely to overfit). If the original call to \code{bagEarth} or \code{bagFDA} had \code{keepX = TRUE}, the first method is used, otherwise the values are calculated via the second method.
}
\seealso{\code{\link{bagEarth}}}
\examples{
\dontrun{
data(trees)
## out of bag predictions vs just re-predicting the training set
fit1 <- bagEarth(Volume ~ ., data = trees, keepX = TRUE)
fit2 <- bagEarth(Volume ~ ., data = trees, keepX = FALSE)
hist(predict(fit1) - predict(fit2))
}
}
\keyword{regression}


