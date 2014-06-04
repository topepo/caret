\name{Alternate Affy Gene Expression Summary Methods.}
\alias{generateExprVal.method.trimMean}
\title{Generate Expression Values from Probes}
\description{
  Generate an expression from the probes
}
\usage{
generateExprVal.method.trimMean(probes, trim = 0.15)
}
\arguments{
  \item{probes}{a matrix of probe intensities with rows representing
    probes and columns representing
    samples. Usually \code{pm(probeset)} where \code{probeset} is a
    of class \code{\link[affy:ProbeSet-class]{ProbeSet}}}
  \item{trim}{the fraction (0 to 0.5) of observations to be trimmed
    from each end of the data before the mean is computed.}
}
\value{
  A list containing entries:
  \item{exprs}{The expression values.}
  \item{se.exprs}{The standard error estimate.}
}
\examples{
\dontrun{
# first, let affy/expresso know that the method exists
 express.summary.stat.methods <- c(express.summary.stat.methods, "trimMean")

 example not run, as it would take a while
 RawData <- ReadAffy(celfile.path=FilePath)

 expresso(RawData, 
   bgcorrect.method="rma",
   normalize.method="quantiles",
   normalize.param = list(type= "pmonly"),
   pmcorrect.method="pmonly",
   summary.method="trimMean")
  
 step1 <- bg.correct(RawData, "rma")
 step2 <- normalize.AffyBatch.quantiles(step1)
 step3 <- computeExprSet(step2, "pmonly", "trimMean")  
}
}
\seealso{
  \code{\link[affy]{generateExprSet-methods}}
}
\keyword{manip}
