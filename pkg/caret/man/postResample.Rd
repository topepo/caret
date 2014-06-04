\name{postResample}
\alias{postResample}
\alias{defaultSummary}
\alias{twoClassSummary}
\alias{getTrainPerf}
\alias{R2}
\alias{RMSE}
\title{Calculates performance across resamples}
\description{
Given two numeric vectors of data, the mean squared error and R-squared
are calculated. For two factors, the overall agreement rate and Kappa are 
determined.
}
\usage{
postResample(pred, obs)
defaultSummary(data, lev = NULL, model = NULL)

twoClassSummary(data, lev = NULL, model = NULL)

R2(pred, obs, formula = "corr", na.rm = FALSE)
RMSE(pred, obs, na.rm = FALSE)

getTrainPerf(x)
}
\arguments{
  \item{pred}{A vector of numeric data (could be a factor)}
  \item{obs}{A vector of numeric data (could be a factor)}
  \item{data}{a data frame or matrix with columns \code{obs} and \code{pred} for the observed and predicted outcomes. For \code{twoClassSummary}, columns should also
             include predicted probabilities for each class. See the \code{classProbs} argument to \code{\link{trainControl}}}
  \item{lev}{a character vector of factors levels for the response. In regression cases, this would be \code{NULL}.}
  \item{model}{a character string for the model name (as taken form the \code{method} argument of \code{\link{train}}.}
  \item{formula}{which \eqn{R^2} formula should be used? Either "corr" or "traditional". See Kvalseth (1985) for a summary of the different equations.}
  \item{na.rm}{a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.}
  \item{x}{an object of class  \code{\link{train}}}.
}
\details{
\code{postResample} is meant to be used with \code{apply} across a matrix. For numeric data
the code checks to see if the standard deviation of either vector is zero. If so, the correlation
between those samples is assigned a value of zero. \code{NA} values are ignored everywhere.

Note that many models have more predictors (or parameters) than data points, so the typical mean squared
error denominator (n - p) does not apply. Root mean squared error is calculated using \code{sqrt(mean((pred - obs)^2}. 
Also, \eqn{R^2} is calculated wither using as the square of the correlation between the observed and predicted outcomes when \code{form = "corr"}. when \code{form = "traditional"}, 
\deqn{
R^2 = 1-\frac{\sum (y_i - \hat{y}_i)^2}{\sum (y_i - \bar{y}_i)^2}
}

For \code{defaultSummary} is the default function to compute performance metrics in \code{\link{train}}. It is a wrapper around \code{postResample}. 

\code{twoClassSummary} computes sensitivity, specificity and the area under the ROC curve. To use this function, the  \code{classProbs} argument of \code{\link{trainControl}} should be \code{TRUE}.

Other functions can be used via the \code{summaryFunction} argument of \code{\link{trainControl}}. Custom functions must have the same arguments as\code{defaultSummary}.

The function \code{getTrainPerf} returns a one row data frame with the resampling results for the chosen model. The statistics will have the prefix "\code{Train}" (i.e. "\code{TrainROC}"). There is also a column called "\code{method}" that echoes the argument of the call to  \code{\link{trainControl}} of the same name. 


}
\value{
   A vector of performance estimates.
}
\author{Max Kuhn}
\references{Kvalseth. Cautionary note about \eqn{R^2}. American Statistician (1985) vol. 39 (4) pp. 279-285}

\seealso{\code{\link{trainControl}}}
\examples{
predicted <-  matrix(rnorm(50), ncol = 5)
observed <- rnorm(10)
apply(predicted, 2, postResample, obs = observed)

}
\keyword{utilities}


