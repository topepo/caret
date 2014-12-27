\name{predict.gafs}
\alias{predict.gafs}
\alias{predict.safs}

\title{Predict new samples}
\description{
Predict new samples using \code{\link{safs}} and \code{\link{gafs}} objects.
}
\usage{
\method{predict}{gafs}(object, newdata, ...)

\method{predict}{safs}(object, newdata, ...)
}

\arguments{
  \item{object}{an object of class \code{\link{safs}} or \code{\link{gafs}}}
  \item{newdata}{a data frame or matrix of predictors. }
  \item{\dots}{not currently used}
}

\details{
Only the predictors listed in \code{object$optVariables} are required.
}

\value{
The type of result depends on what was specified in \code{object$control$functions$predict}.
}

\author{Max Kuhn}

\seealso{\code{\link{safs}}, \code{\link{gafs}}}
\examples{
\dontrun{

set.seed(1)
train_data <- twoClassSim(100, noiseVars = 10)
test_data  <- twoClassSim(10,  noiseVars = 10)

## A short example 
ctrl <- safsControl(functions = rfSA, 
                    method = "cv",
                    number = 3)

rf_search <- safs(x = train_data[, -ncol(train_data)],
                  y = train_data$Class,
                  iters = 3,
                  safsControl = ctrl)

rf_search

predict(rf_search, train_data)  
}
}
\keyword{multivariate}

