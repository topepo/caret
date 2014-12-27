\name{index2vec}
\alias{index2vec}

\title{
Convert indicies to a binary vector
}
\description{
The function performs the opposite of \code{which} converting a set of integers to a binary vector
}
\usage{
index2vec(x, vars, sign = FALSE)
}
\arguments{
  \item{x}{a vector of integers}
  \item{vars}{the number of possible locations}
  \item{sign}{a lgical; when true the data are encoded as -1/+1, and 0/1 otherwise}
}

\value{a numeric vector}

\author{Max Kuhn}

\examples{
index2vec(x = 1:2, vars = 5)
index2vec(x = 1:2, vars = 5, sign = TRUE)
}

