\name{print.confusionMatrix}
\alias{print.confusionMatrix}
\title{Print method for confusionMatrix}
\description{
a print method for \code{confusionMatrix}
}
\usage{
\method{print}{confusionMatrix}(x, digits = max(3, getOption("digits") - 3),
      printStats = TRUE, ...)
}
\arguments{
  \item{x}{an object of class \code{confusionMatrix}}
  \item{digits}{number of significant digits when printed}
  \item{printStats}{a logical: if \code{TRUE} then table statistics are also printed}
  \item{\dots}{optional arguments to pass to \code{print.table}}
}

\value{
\code{x} is invisibly returned
}
\author{Max Kuhn}

\seealso{\code{\link{confusionMatrix}}}

\keyword{utilities}

