\name{dotPlot}
\alias{dotPlot}
\title{Create a dotplot of variable importance values}
\description{
A lattice \code{\link[lattice:xyplot]{dotplot}} is created from an object of class \code{varImp.train}.
}
\usage{
dotPlot(x, top = min(20, dim(x$importance)[1]), ...)
}
\arguments{
  \item{x}{an object of class \code{varImp.train}}
  \item{top}{the number of predictors to plot}
  \item{\dots}{options passed to \code{\link[lattice:xyplot]{dotplot}}}
}

\value{
an object of class \code{trellis}.
}
\author{Max Kuhn}
\seealso{\code{\link{varImp}}, \code{\link[lattice:xyplot]{dotplot}}}
\examples{

data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit <- train(TrainData, TrainClasses, "knn")

knnImp <- varImp(knnFit)

dotPlot(knnImp)

}
\keyword{hplot}
