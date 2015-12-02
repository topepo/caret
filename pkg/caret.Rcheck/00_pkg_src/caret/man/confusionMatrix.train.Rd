\name{confusionMatrix.train}
\alias{confusionMatrix.train}
\alias{confusionMatrix.rfe}
\alias{confusionMatrix.sbf}
\title{Estimate a Resampled Confusion Matrix}
\description{
Using a \code{\link{train}}, \code{\link{rfe}}, \code{\link{sbf}} object, determine a confusion matrix based on the resampling procedure}
\usage{
\method{confusionMatrix}{train}(data, norm = "overall", 
                dnn = c("Prediction", "Reference"), ...)

\method{confusionMatrix}{rfe}(data, norm = "overall", 
                dnn = c("Prediction", "Reference"), ...)

\method{confusionMatrix}{sbf}(data, norm = "overall", 
                dnn = c("Prediction", "Reference"), ...)

}
\arguments{
  \item{data}{an object of class \code{\link{train}}, \code{\link{rfe}}, \code{\link{sbf}} that did not use out-of-bag resampling or leave-one-out cross-validation.}
  \item{norm}{a character string indicating how the table entries should be normalized. Valid values are "none", "overall" or "average". }
  \item{dnn}{a character vector of dimnames for the table}
  \item{\dots}{not used here}
}
\details{
When \code{\link{train}} is used for tuning a model, it tracks the confusion matrix cell entries for the hold-out samples. These can be aggregated and used for diagnostic purposes. For \code{\link{train}}, the matrix is estimated for the final model tuning parameters determined by \code{\link{train}}. For \code{\link{rfe}}, the matrix is associated with the optimal number of variables.

There are several ways to show the table entries. Using \code{norm = "none"} will show the frequencies of samples on each of the cells (across all resamples). \code{norm = "overall"} first divides the cell entries by the total number of data points in the table, then averages these percentages. \code{norm = "average"} takes the raw, aggregate cell counts across resamples and divides by the number of resamples (i.e. to yield an average count for each cell). 
}
\value{
a list of class \code{confusionMatrix.train}, \code{confusionMatrix.rfe} or \code{confusionMatrix.sbf} with elements
  \item{table}{the normalized matrix}
  \item{norm}{an echo fo the call}
  \item{text}{a character string with details about the resampling procedure (e.g. "Bootstrapped (25 reps) Confusion Matrix"}
 
}
\author{Max Kuhn}

\seealso{\code{\link{confusionMatrix}}, \code{\link{train}}, \code{\link{rfe}}, \code{\link{sbf}}, \code{\link{trainControl}}}
\examples{

data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit <- train(TrainData, TrainClasses,
                method = "knn",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
confusionMatrix(knnFit)
confusionMatrix(knnFit, "average")
confusionMatrix(knnFit, "none")

}
\keyword{utilities}

