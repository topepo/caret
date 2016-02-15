\name{Sacramento}
\docType{data}
\alias{Sacramento}
\title{Sacramento CA Home Prices}
\description{
This data frame contains house and sale price data for 932 homes in Sacramento CA. 
The original data were obtained from the website for the SpatialKey software. From their website: "The Sacramento real estate transactions file is a list of 985 real estate transactions in the Sacramento area reported over a five-day period, as reported by the Sacramento Bee." Google was used to fill in missing/incorrect data.
}

\usage{data(Sacramento)}

\value{
  \item{Sacramento}{a data frame with columns '\code{city}', '\code{zip}', '\code{beds}', '\code{baths}', '\code{sqft}', '\code{type}', '\code{price}', '\code{latitude}', and '\code{longitude}'}
  }
\source{SpatialKey website: \url{https://support.spatialkey.com/spatialkey-sample-csv-data}}

\examples{
data(Sacramento)

set.seed(955)
in_train <- createDataPartition(log10(Sacramento$price), p = .8, list = FALSE)

training <- Sacramento[ in_train,]
testing  <- Sacramento[-in_train,]

}

\keyword{datasets}  

