\name{var_seq}
\alias{var_seq}
\title{
Sequences of Variables for Tuning
}
\description{
This function generates a sequence of \code{mtry} values for random forests. 
}
\usage{
var_seq(p, classification = FALSE, len = 3)
}
\arguments{
  \item{p}{
The number of predictors
}
  \item{classification}{
Is the outcome a factor (\code{classification = TRUE} or numeric?)
}
  \item{len}{
The number of \code{mtry} values to generate.
}
}

\details{
If the number of predictors is less than 500, a simple sequence of values of length \code{len} is generated between 2 and \code{p}. For larger numbers of predictors, the sequence is created using \code{log2} steps. 

If \code{len = 1}, the defaults from the \code{randomForest} package are used. 
}
\value{
a numeric vector
}

\author{
Max Kuhn
}

\examples{
var_seq(p = 100, len = 10)
var_seq(p = 600, len = 10)
}

\keyword{models}
 