\name{as.table.confusionMatrix}
\alias{as.table.confusionMatrix}
\alias{as.matrix.confusionMatrix}
\title{Save Confusion Table Results}
\description{
Conversion functions for class \code{confusionMatrix}
}
\usage{
\method{as.matrix}{confusionMatrix}(x, what = "xtabs", ...)

\method{as.table}{confusionMatrix}(x, ...)
}
\arguments{
  \item{x}{an object of class \code{\link{confusionMatrix}}}
  \item{what}{data to conver to matrix. Either \code{"xtabs"}, \code{"overall"} or  \code{"classes"}}
  \item{\dots}{ not currently used }
}
\details{
For \code{as.table}, the cross-tabulations are saved. For \code{as.matrix}, the three object types are saved in matrix format.
}
\value{
A matrix or table
}
\author{Max Kuhn}

\seealso{\code{\link{confusionMatrix}}}
\examples{
###################
## 2 class example

lvs <- c("normal", "abnormal")
truth <- factor(rep(lvs, times = c(86, 258)),
                levels = rev(lvs))
pred <- factor(
               c(
                 rep(lvs, times = c(54, 32)),
                 rep(lvs, times = c(27, 231))),               
               levels = rev(lvs))

xtab <- table(pred, truth)

results <- confusionMatrix(xtab)
as.table(results)
as.matrix(results)
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")
  
###################
## 3 class example

xtab <- confusionMatrix(iris$Species, sample(iris$Species))
as.matrix(xtab)
}
\keyword{utilities }


