\name{createDataPartition}
\alias{createDataPartition}
\alias{createResample}
\alias{createFolds}
\alias{createMultiFolds}
\alias{createTimeSlices}
\title{Data Splitting functions}
\description{
  A series of test/training partitions are created using
  \code{createDataPartition} while \code{createResample} creates one or
  more bootstrap samples. \code{createFolds} splits the data into
  \code{k} groups while \code{createTimeSlices} creates cross-validation
  sample information to be used with time series data.
}
\usage{
createDataPartition(y, 
                    times = 1,
                    p = 0.5,
                    list = TRUE,
                    groups = min(5, length(y)))
createResample(y, times = 10, list = TRUE)
createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
createMultiFolds(y, k = 10, times = 5)
createTimeSlices(y, initialWindow, horizon = 1, fixedWindow = TRUE)
}
\arguments{
  \item{y}{a vector of outcomes. For \code{createTimeSlices}, these should be in chronological order.}
  \item{times}{the number of partitions to create}
  \item{p}{the percentage of data that goes to training}
  \item{list}{logical - should the results be in a list (\code{TRUE}) or a matrix 
  with the number of rows equal to \code{floor(p * length(y))} and \code{times}
  columns.}
  \item{groups}{for numeric \code{y}, the number of breaks in the quantiles
    (see below)}
  \item{k}{an integer for the number of folds.}
  \item{returnTrain}{a logical. When true, the values returned are the
    sample positions corresponding to the data used during
    training. This argument only works in conjunction with \code{list = TRUE}}  
  \item{initialWindow}{The initial number of consecutive values in each training set sample}
  \item{horizon}{The number of consecutive values in test set sample}
  \item{fixedWindow}{A logical: if \code{FALSE}, the training set always start at the first sample.}
}
\details{

  For bootstrap samples, simple random sampling is used.

  For other data splitting,  the random sampling is done within the
  levels of \code{y} when \code{y} is a factor in an attempt to balance
  the class distributions within the splits. 

  For numeric \code{y}, the sample is split into groups sections based
  on percentiles and sampling is done within these subgroups. For 
  \code{createDataPartition}, the number of percentiles is set via the 
  \code{groups} argument. For \code{createFolds} and \code{createMultiFolds}, 
  the number of groups is set dynamically based on the sample size and \code{k}. 
  For smaller samples sizes, these two functions may not do stratified 
  splitting and, at most, will split the data into quartiles. 


  Also, for \code{createDataPartition}, very  small class sizes (<= 3) the 
  classes may not show up in both the training and test data

 For multiple k-fold cross-validation, completely independent folds are created. 
 The names of the list objects will denote the fold membership using the pattern 
 "Foldi.Repj" meaning the ith section (of k) of the jth cross-validation set 
 (of \code{times}). Note that this function calls \code{createFolds} with 
 \code{list = TRUE} and \code{returnTrain = TRUE}.

  Hyndman and Athanasopoulos (2013)) discuss rolling forecasting origin< techniques that move the training and test sets in time. \code{createTimeSlices} can create the indices for this type of splitting. 
}
\value{
   A list or matrix of row position integers corresponding to the training
   data
}
\references{
\url{http://topepo.github.io/caret/splitting.html}

Hyndman and Athanasopoulos (2013), Forecasting: principles and practice. \url{https://www.otexts.org/fpp}
}

\author{Max Kuhn, \code{createTimeSlices} by Tony Cooper}

\examples{
data(oil)
createDataPartition(oilType, 2)

x <- rgamma(50, 3, .5)
inA <- createDataPartition(x, list = FALSE)

plot(density(x[inA]))
rug(x[inA])

points(density(x[-inA]), type = "l", col = 4)
rug(x[-inA], col = 4)

createResample(oilType, 2)

createFolds(oilType, 10)
createFolds(oilType, 5, FALSE)

createFolds(rnorm(21))

createTimeSlices(1:9, 5, 1, fixedWindow = FALSE)
createTimeSlices(1:9, 5, 1, fixedWindow = TRUE)
createTimeSlices(1:9, 5, 3, fixedWindow = TRUE)
createTimeSlices(1:9, 5, 3, fixedWindow = FALSE)
}
\keyword{utilities}


