\name{normalize.AffyBatch.normalize2Reference}
\alias{normalize.AffyBatch.normalize2Reference}
\title{Quantile Normalization to a Reference Distribution}
\description{
  Quantile normalization based upon a reference distribution. This function
  normalizes a matrix of data (typically Affy probe level intensities).
}
\usage{
normalize.AffyBatch.normalize2Reference(
   abatch, 
   type = c("separate", "pmonly", "mmonly", "together"), 
   ref = NULL)
}
\arguments{
  \item{abatch}{An \code{{AffyBatch}}}
  \item{type}{A string specifying how the normalization should be
    applied. See details for more.}
  \item{ref}{A vector of reference values. See details for more.}    
}
\details{This method is based upon the concept of a quantile-quantile
  plot extended to n dimensions. No special allowances are made for
  outliers. If you make use of quantile normalization either through
  \code{rma} or \code{expresso}
  please cite Bolstad et al, Bioinformatics (2003).

  The type argument should be one of
  \code{"separate","pmonly","mmonly","together"} which indicates whether
  to normalize only one probe type (PM,MM) or both together or separately.
  
  The function uses the data supplied in \code{ref} to use as the reference
  distribution. In other words, the PMs in \code{abatch} will be normalized
  to have the same distribution as the data in \code{ref}. If \code{ref} is
  \code{NULL}, the normalizing takes place using the average quantiles
  of the PM values in \code{abatch} (just as in \code{normalize.AffyBatch.quantile}).
    
}

\value{
  A normalized \code{AffyBatch}.
}
\references{
  Bolstad, B (2001) \emph{Probe Level Quantile Normalization of High Density
    Oligonucleotide Array Data}. Unpublished manuscript

  Bolstad, B. M., Irizarry R. A., Astrand, M, and Speed, T. P. (2003)
  \emph{A Comparison of Normalization Methods for High Density
    Oligonucleotide Array Data Based on Bias and Variance.}
   Bioinformatics 19(2) ,pp 185-193.
  }

\author{Max Kuhn, adapted from Ben Bolstad, \email{bolstad@stat.berkeley.edu}}

\seealso{\code{normalize}}

\examples{
# first, let affy/expresso know that the method exists
# normalize.AffyBatch.methods <- c(normalize.AffyBatch.methods, "normalize2Reference")

# example not run, as it would take a while
# RawData <- ReadAffy(celfile.path=FilePath)

# Batch1Step1 <- bg.correct(RawData, "rma")
# Batch1Step2 <- normalize.AffyBatch.quantiles(Batch1Step1)
# referencePM <- pm(Batch1Step2)[,1]
# Batch1Step3 <- computeExprSet(Batch1Step2, "pmonly", "medianpolish")  
   
# Batch2Step1 <- bg.correct(RawData2, "rma")
# Batch2Step2 <- normalize.AffyBatch.normalize2Reference(Batch2Step1, ref = referencePM)
# Batch2Step3 <- computeExprSet(Batch2Step2, "pmonly", "medianpolish")    
}

\keyword{manip} 
