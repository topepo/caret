\name{gafs_initial}
\alias{gafs_initial}
\alias{gafs_lrSelection}
\alias{gafs_rwSelection}
\alias{gafs_tourSelection}
\alias{gafs_uCrossover}
\alias{gafs_spCrossover}
\alias{gafs_raMutation}
\alias{caretGA}
\alias{rfGA}
\alias{treebagGA}
\title{
Ancillary genetic algorithm functions
}
\description{
Built-in functions related to genetic algorithms}
\usage{
gafs_initial(vars, popSize, ...)


gafs_lrSelection(population, fitness, 
                 r = NULL, 
                 q = NULL, ...)

gafs_rwSelection(population, fitness, ...) 

gafs_tourSelection(population, fitness, k = 3, ...)


gafs_spCrossover(population, fitness, parents, ...)

gafs_uCrossover(population, parents, ...)


gafs_raMutation(population, parent, ...) 


caretGA
rfGA
treebagGA
}

\arguments{
  \item{vars}{number of possible predictors}
  \item{popSize}{the population size passed into \code{\link{gafs}}}
  \item{population}{a binary matrix of the current subsets with predictors in columns and individuals in rows}
  \item{fitness}{a vector of fitness values}
  \item{parent, parents}{integer(s) for which chromosomes are altered}
  \item{r, q, k}{tuning parameters for the specific selection operator}  
  \item{\dots}{not currently used}
}
\details{
These functions are used with the \code{functions} argument of the \code{\link{gafsControl}} function. More information on the details of these functions are at \url{http://topepo.github.io/caret/GA.html}.

Most of the \code{gafs_*} functions are based on those from the GA package by Luca Scrucca. These functions here are small re-writes to work outside of the GA package. 

The objects \code{caretGA}, \code{rfGA} and \code{treebagGA} are example lists that can be used with the \code{functions} argument of \code{\link{gafsControl}}. 

In the case of \code{caretGA}, the \code{...} structure of \code{\link{gafs}} passes through to the model fitting routine. As a consequence, the \code{\link{train}} function can easily be accessed by passing important arguments belonging to \code{\link{train}} to \code{\link{gafs}}. See the examples below. By default, using \code{caretGA} will used the resampled performance estimates produced by  \code{\link{train}} as the internal estimate of fitness.

For \code{rfGA} and \code{treebagGA}, the \code{randomForest} and \code{bagging} functions are used directly (i.e. \code{\link{train}} is not used). Arguments to either of these functions can also be passed to them though the \code{\link{gafs}} call (see examples below). For these two functions, the internal fitness is estimated using the out-of-bag estimates naturally produced by those functions. While faster, this limits the user to accuracy or Kappa (for classification) and RMSE and R-squared (for regression). 


}
\value{
The return value depends on the function. 

}
\references{
Scrucca L (2012). GA: A Package for Genetic Algorithms in R. Journal of Statistical Software, 53(4), 1-37.

\url{cran.r-project.org/web/packages/GA/}

\url{http://topepo.github.io/caret/GA.html}
}
\author{
Luca Scrucca, \code{gafs_initial}, \code{caretGA}, \code{rfGA} and \code{treebagGA} by Max Kuhn
}


\seealso{\code{\link{gafs}}, \code{\link{gafsControl}}}

\examples{
pop <- gafs_initial(vars = 10, popSize = 10)
pop

gafs_lrSelection(population = pop, fitness = 1:10)

gafs_spCrossover(population = pop, fitness = 1:10, parents = 1:2)


\dontrun{
## Hypothetical examples
lda_ga <- gafs(x = predictors,
               y = classes,
               gafsControl = gafsControl(functions = caretGA),
               ## now pass arguments to `train`
               method = "lda",
               metric = "Accuracy"
               trControl = trainControl(method = "cv", classProbs = TRUE))

rf_ga <- gafs(x = predictors,
              y = classes,
              gafsControl = gafsControl(functions = rfGA),
              ## these are arguments to `randomForest`
              ntree = 1000,
              importance = TRUE)
	}

}

