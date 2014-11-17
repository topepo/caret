\name{avNNet.default}
\alias{avNNet.default}
\alias{predict.avNNet}
\alias{avNNet.formula}
\alias{avNNet}
\title{Neural Networks Using Model Averaging}
\description{
Aggregate several neural network models
}
\usage{
\method{avNNet}{default}(x, y, repeats = 5, bag = FALSE, allowParallel = TRUE, ...)
\method{avNNet}{formula}(formula, data, weights, ..., 
        repeats = 5, bag = FALSE, allowParallel = TRUE,
        subset, na.action, contrasts = NULL)

\method{predict}{avNNet}(object, newdata, type = c("raw", "class", "prob"), ...)
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
\item{repeats}{the number of neural networks with different random number seeds}

\item{bag}{a logical for bagging for each repeat}
\item{allowParallel}{if a parallel backend is loaded and available, should the function use it?}

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
\code{na.omit}, which leads to rejection of cases with missing values on
any required variable.  (NOTE: If given, this argument must be named.)
}
\item{contrasts}{
a list of contrasts to be used for some or all  of
the  factors  appearing as variables in the model formula.
}

\item{object}{
an object of class \code{avNNet} as  returned by \code{avNNet}.
}
\item{newdata}{
matrix or data frame of test examples. A vector is considered to be
a row vector comprising a single case.
}
\item{type}{
Type of output, either: \code{raw} for the raw outputs, \code{code} for the predicted class or \code{prob} for the class probabilities.
}

\item{\dots}{
arguments passed to \code{\link[nnet]{nnet}}
}

}
\details{
Following Ripley (1996), the same neural network model is fit using different random number seeds. All the resulting models are used for prediction. For regression, the output from each network are averaged. For classification, the model scores are first averaged, then translated to predicted classes. Bagging can also be used to create the models.

If a parallel backend is registered, the \pkg{foreach} package is used to train the networks in parallel. 

}
\value{
  For \code{avNNet}, an object of  \code{"avNNet"} or \code{"avNNet.formula"}. Items of interest in the output are:
  \item{model }{a list of the models generated from  \code{\link[nnet]{nnet}}}
  \item{repeats }{an echo of the model input}
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
\dontrun{
modelFit <- avNNet(bbbDescr, logBBB, size = 5, linout = TRUE, trace = FALSE)
modelFit

predict(modelFit, bbbDescr)
}
}
\keyword{ neural }


