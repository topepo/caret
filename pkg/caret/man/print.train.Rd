\name{print.train}
\alias{print.train}
\title{Print Method for the train Class}
\description{
 Print the results of a \code{\link{train}} object.
}
\usage{
\method{print}{train}(x, 
      digits = min(3, getOption("digits") - 3), 
      printCall = FALSE, 
      details = FALSE, 
      selectCol = FALSE,
      ...)
}
\arguments{
  \item{x}{an object of class  \code{\link{train}}.}
  \item{digits}{an integer specifying the number of significant digits
    to print.}
  \item{printCall}{a logical to print the call at the top of the output}
  \item{details}{a logical to show print or summary methods for the
  final model. In some cases (such as \code{gbm}, \code{knn},
  \code{lvq}, naive Bayes and bagged tree models), no information will
  be printed even if \code{details = TRUE}}
  \item{selectCol}{a logical to a column with a star next to the final model}
  
  \item{\ldots}{options passed to the generic print method}
}
\details{
   The table of complexity parameters used, their resampled performance and a
   flag for which rows are optimal.
}
\value{
   A data frame with the complexity parameter(s) and performance (invisibly).
}
\author{Max Kuhn}
\seealso{\code{\link{train}}}
\examples{
\dontrun{
data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

library(klaR)
rdaFit <- train(TrainData, TrainClasses, method = "rda",
                control = trainControl(method = "cv"))
print(rdaFit)
}
}
\keyword{print}
