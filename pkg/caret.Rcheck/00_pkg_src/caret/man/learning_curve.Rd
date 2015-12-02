\name{learing_curve_dat}
\alias{learing_curve_dat}
\title{
Create Data to Plot a Learning Curve
}
\description{
For a given model, this function fits several versions on different sizes of the total training set and returns the results
}
\usage{
learing_curve_dat(dat, outcome = NULL, 
                  proportion = (1:10)/10, test_prop = 0, 
                  verbose = TRUE, ...)
}
\arguments{
  \item{dat}{
the training data
}
  \item{outcome}{
a character string identifying the outcome column name
}
  \item{proportion}{
the incremental proportions of the training set that are used to fit the model
}
  \item{test_prop}{
an optional proportion of the data to be used to measure performance. 
}
  \item{verbose}{
a logical to print logs to the screen as models are fit
}
  \item{\dots}{
options to pass to \code{\link{train}} to specify the model. These should not include \code{x}, \code{y}, \code{formula}, or \code{data}. 
}
}
\details{
This function creates a data set that can be used to plot how well the model performs over different sized versions of the training set. For each data set size, the performance metrics are determined and saved. If \code{test_prop == 0}, the apparent measure of performance (i.e. re-predicting the training set) and the resampled estimate of performance are available. Otherwise, the test set results are also added.  

If the model being fit has tuning parameters, the results are based on the optimal settings determined by \code{\link{train}}.
}
\value{
a data frame with columns for each performance metric calculated by \code{\link{train}} as well as columns:
\item{Training_Size }{the number of data points used in the current model fit}
\item{Data }{which data were used to calculate performance. Values are "Resampling", "Training", and (optionally) "Testing"}
In the results, each data set size will have one row for the apparent error rate, one row for the test set results (if used) and as many rows as resamples (e.g. 10 rows if 10-fold CV is used).

}

\author{
Max Kuhn
}


\seealso{
\code{\link{train}}
}
\examples{
\dontrun{
set.seed(1412)
class_dat <- twoClassSim(1000)

set.seed(29510)
lda_data <- learing_curve_dat(dat = class_dat, 
                              outcome = "Class",
                              test_prop = 1/4, 
                              ## `train` arguments:
                              method = "lda", 
                              metric = "ROC",
                              trControl = trainControl(classProbs = TRUE, 
                                                       summaryFunction = twoClassSummary))



ggplot(lda_data, aes(x = Training_Size, y = ROC, color = Data)) + 
  geom_smooth(method = loess, span = .8) + 
  theme_bw()
 }
}

\keyword{models}

