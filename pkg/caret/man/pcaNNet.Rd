\name{pcaNNet.default}
\alias{pcaNNet.default}
\alias{predict.pcaNNet}
\alias{pcaNNet.formula}
\alias{pcaNNet}
\title{Neural Networks with a Principal Component Step }
\description{
Run PCA on a dataset, then use it in a neural network model
}
\usage{
\method{pcaNNet}{default}(x, y, thresh = 0.99, ...)
\method{pcaNNet}{formula}(formula, data, weights, ..., 
        thresh = .99, subset, na.action, contrasts = NULL)

\method{predict}{pcaNNet}(object, newdata, type = c("raw", "class"), ...)
}

\arguments{

\item{formula}{
A formula of the form \code{class ~ x1 + x2 + \dots}
}
\item{x}{
matrix or data frame of \code{x} values for examples.
}
\item{y}{
matrix or data frame of target values for examples.
}
\item{weights}{
(case) weights for each example -- if missing defaults to 1.
}
\item{thresh}{a threshold for the cumulative proportion of variance to capture from the PCA analysis. For example, to retain enough PCA components to capture 95 percent of variation, set \code{thresh = .95}}

\item{data}{
Data frame from which variables specified in  \code{formula} are
preferentially to be taken.
}
\item{subset}{
An index vector specifying the cases to be used in the training
sample.  (NOTE: If given, this argument must be named.)
}
\item{na.action}{
A function to specify the action to be taken if \code{NA}s are found.
The default action is for the procedure to fail.  An alternative is
na.omit, which leads to rejection of cases with missing values on
any required variable.  (NOTE: If given, this argument must be named.)
}
\item{contrasts}{
a list of contrasts to be used for some or all  of
the  factors  appearing as variables in the model formula.
}

\item{object}{
an object of class \code{pcaNNet} as  returned by \code{pcaNNet}.
}
\item{newdata}{
matrix or data frame of test examples. A vector is considered to be
a row vector comprising a single case.
}
\item{type}{
Type of output
}

\item{\dots}{
arguments passed to \code{\link[nnet]{nnet}}
}

}
\details{
The function first will run principal component analysis on the data. The cumulative percentage of variance is computed for each principal component. The function uses the \code{thresh} argument to determine how many components must be retained to capture this amount of variance in the predictors.

The principal components are then used in a neural network model.

When predicting samples, the new data are similarly transformed using the information from the PCA analysis on the training data and then predicted.
 
Because the variance of each predictor is used in the PCA analysis, the code does a quick check to make sure that each predictor has at least two distinct values. If a predictor has one unique value, it is removed prior to the analysis.

}
\value{
For \code{pcaNNet}, an object of  \code{"pcaNNet"} or \code{"pcaNNet.formula"}. Items of interest in the output are:
  \item{pc }{the output from  \code{\link{preProcess}}}
  \item{model }{the model generated from  \code{\link[nnet]{nnet}}}
  \item{names }{if any predictors had only one distinct value, this is a character string of the remaining columns. Otherwise a value of \code{NULL}}
}
\references{
  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks.} Cambridge.
}
\author{These are heavily based on the \code{nnet} code from Brian Ripley.}

\seealso{ \code{\link[nnet]{nnet}},  \code{\link{preProcess}}}
\examples{
data(BloodBrain)
modelFit <- pcaNNet(bbbDescr[, 1:10], logBBB, size = 5, linout = TRUE, trace = FALSE)
modelFit

predict(modelFit, bbbDescr[, 1:10])
}
\keyword{ neural }


