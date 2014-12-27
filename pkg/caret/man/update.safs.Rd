\name{update.safs}
\alias{update.safs}
\alias{update.gafs}
\title{
Update or Re-fit a SA or GA Model
}
\description{
\code{update} allows a user to over-ride the search iteration selection process.
}
\usage{
\method{update}{gafs}(object, iter, x, y, ...)

\method{update}{safs}(object, iter, x, y, ...)
}

\arguments{
  \item{object}{An object produced by \code{\link{gafs}} or \code{\link{safs}}}
  \item{iter}{a single numeric integer}
  \item{x, y}{the original training data used in the call to \code{\link{gafs}} or \code{\link{safs}}}
  \item{\dots}{not currently used}
}
\details{
Based on the results of plotting a \code{\link{gafs}} or \code{\link{safs}} object, these functions can be used to supersede the number of iterations determined analytically from the resamples. 

Any values of \code{...} originally passed to \code{\link{gafs}} or \code{\link{safs}} are automatically passed on to the updated model (i.e. they do not need to be supplied again to \code{update}.
}
\value{
an object of class \code{\link{gafs}} or \code{\link{safs}}
}

\author{
Max Kuhn
}

\seealso{
\code{\link{gafs}}, \code{\link{safs}}
}
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

rf_search2 <- update(rf_search, 
	                 iter = 1,
	                 x = train_data[, -ncol(train_data)],
                     y = train_data$Class)
rf_search2
}
}

\keyword{ models }


