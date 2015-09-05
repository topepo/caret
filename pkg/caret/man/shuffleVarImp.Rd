\name{shuffleVarImp}
\alias{shuffleVarImp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of variable importance for regression and classification models using predictor shuffling
}
\description{
A generic method for calculating variable importance for objects produced by
\code{train} by shuffling predictor columns and rescoring the predictions
}
\usage{
shuffleVarImp.train(model, test_x, test_y, shuffleTimes = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
an object corresponding to a fitted model
}
  \item{test_x}{
a data frame corresponding to the predictors of a test data set
}
  \item{test_y}{
a vector corresponding to the target of a test data set
}
  \item{shuffleTimes}{
number of times to shuffle and recompute performance metric
}
}
\details{
This function first computes the model prediction score on the given test set. Considering this score as base score it computes the score \code{shuffTimes} times for each feature on a data set created from test_x where that feature's column is randomly shuffled. The average number of times score gets worse is considered as a measure of that features importance for this model.
}
\value{
A data frame with variable name and importances in (0,1) ordered with respect to importance with values closer to 1 being more important. The importance is the mean number of times the score worsens.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Can Candan
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Function copied from mlbench package, its the "Friedman 1"
# benchmark with 5 real, 5 bogus features

mlbench.friedman1<-function (n, sd = 1) 
{
  x <- matrix(runif(10 * n), ncol = 10)
  y <- 10 * sin(pi * x[, 1] * x[, 2])
  y <- y + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
  if (sd > 0) {
    y <- y + rnorm(n, sd = sd)
  }
  list(x = x, y = y)
}

n <- 100
sigma <- 1
set.seed(1)
sim <- mlbench.friedman1(n, sd = sigma)
colnames(sim$x) <- c(paste("real", 1:5, sep = ""),
                     paste("bogus", 1:5, sep = ""))

normalization <- preProcess(sim$x)
x <- predict(normalization, sim$x)
x <- as.data.frame(x)

train_x=x[1:80,];train_y=sim$y[1:80]
test_x=x[81:100,];test_y=sim$y[81:100]

model=train(x=train_x,y=train_y,method="glmnet")


# 'normal' varImp
vimp=varImp(m)
vimp

# shuffleVarImp
svimp=shuffleVarImp(model = model,test_x = test_x,
                    test_y = test_y,shuffleTimes = 200)
svimp
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
