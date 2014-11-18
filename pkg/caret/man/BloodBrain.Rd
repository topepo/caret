\name{BloodBrain}
\docType{data}
\alias{BloodBrain}
\alias{bbbDescr}
\alias{logBBB}
\title{Blood Brain Barrier Data}
\description{
Mente and Lombardo (2005) develop models to predict the log of the ratio of the concentration of a
compound in the brain and the concentration in blood. For each compound, they computed three
sets of molecular descriptors: MOE 2D, rule-of-five and Charge Polar Surface Area (CPSA). In
all, 134 descriptors were calculated. Included in this package are 208 non-proprietary literature
compounds. The vector \code{logBBB} contains the concentration ratio and the data fame \code{bbbDescr}
contains the descriptor values.
}

\usage{data(BloodBrain)}

\value{
  \item{bbbDescr}{data frame of chemical descriptors}
  \item{logBBB}{vector of assay results}
  }

\source{Mente, S.R. and Lombardo, F. (2005). A recursive-partitioning model for blood-brain barrier permeation, 
\emph{Journal of Computer-Aided Molecular Design}, Vol. 19, pg. 465--481.
}

\keyword{datasets}  

