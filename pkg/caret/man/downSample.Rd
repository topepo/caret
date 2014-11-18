\name{downSample}
\alias{downSample}
\alias{upSample}
\title{
Down- and Up-Sampling Imbalanced Data
}
\description{
\code{downSample} will randomly sample a data set so that all classes have the same frequency as the minority class. \code{upSample} samples with replacement to make the class distributions equal
}
\usage{
downSample(x, y, list = FALSE, yname = "Class")

upSample(x, y, list = FALSE, yname = "Class")
}

\arguments{
  \item{x}{
a matrix or data frame of predictor variables
}
  \item{y}{
a factor variable with the class memberships
}
  \item{list}{
should the function return \code{list(x, y)} or bind \code{x} and \code{y} together? If \code{TRUE}, the output will be coerced to a data frame.
}
  \item{yname}{
if \code{list = FALSE}, a label for the class column
}
}
\details{
Simple random sampling is used to down-sample for the majority class(es). Note that the minority class data are left intact and that the samples will be re-ordered in the down-sampled version.

For up-sampling, all the original data are left intact and additional samples are added to the minority classes with replacement.
}
\value{
Either a data frame or a list with elements \code{x} and \code{y}.
}

\author{
Max Kuhn
}

\examples{
## A ridiculous example...
data(oil)
table(oilType)
downSample(fattyAcids, oilType)

upSample(fattyAcids, oilType)

}

\keyword{ utilities }
