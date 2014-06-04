\name{rfe}
\alias{rfe}
\alias{rfe.default}
\alias{rfeIter}
\alias{predict.rfe}

\title{Backwards Feature Selection}
\description{
A simple backwards selection, a.k.a. recursive feature selection (RFE), algorithm
}
\usage{
rfe(x, ...)

\method{rfe}{default}(x, y, 
    sizes = 2^(2:4), 
    metric = ifelse(is.factor(y), "Accuracy", "RMSE"),
    maximize = ifelse(metric == "RMSE", FALSE, TRUE),
    rfeControl = rfeControl(), 
    ...)

rfeIter(x, y, 
        testX, testY, 
        sizes, 
        rfeControl = rfeControl(), 
        label = "",
        seeds = NA,
        ...)

\method{predict}{rfe}(object, newdata, ...)
}

\arguments{
  \item{x}{a matrix or data frame of predictors for model training. This object must have unique column names.}
  \item{y}{a vector of training set outcomes (either numeric or factor) }
  \item{testX}{a matrix or data frame of test set predictors. This must have the same column names as \code{x}}
  \item{testY}{a vector of test set outcomes }
  \item{sizes}{a numeric vector of integers corresponding to the number of features that should be retained}
  \item{metric}{a string that specifies what summary metric will be used to select the optimal model. By default, possible values are "RMSE" and "Rsquared" for regression and "Accuracy" and "Kappa" for classification. If custom performance metrics are used (via the \code{functions} argument in \code{\link{rfeControl}}, the value of \code{metric} should match one of the arguments.}         
  \item{maximize}{a logical: should the metric be maximized or minimized?}
  \item{rfeControl}{a list of options, including functions for fitting and prediction. The web page  \url{http://caret.r-forge.r-project.org/} has more details and examples related to this function.}
  \item{object}{an object of class \code{rfe}}
  \item{newdata}{a matrix or data frame of new samples for prediction}
  \item{label}{an optional character string to be printed when in verbose mode.}
  \item{seeds}{an optional vector of integers for the size. The vector should have length of \code{length(sizes)+1}}
  \item{\dots}{options to pass to the model fitting function (ignored in \code{predict.rfe})}
}
\details{
More details on this function can be found at \url{http://caret.r-forge.r-project.org/featureselection.html}.

This function implements backwards selection of predictors based on predictor importance ranking. The predictors are ranked and the less important ones are sequentially eliminated prior to modeling. The goal is to find a subset of predictors that can be used to produce an accurate model. The web page  \url{http://caret.r-forge.r-project.org/} has more details and examples related to this function.


\code{rfe} can be used with "explicit parallelism", where different resamples (e.g. cross-validation group) can be split up and run on multiple machines or processors. By default, \code{rfe} will use a single processor on the host machine. As of version 4.99 of this package, the framework used for parallel processing uses the \pkg{foreach} package. To run the resamples in parallel, the code for \code{rfe} does not change; prior to the call to \code{rfe}, a parallel backend is registered with \pkg{foreach} (see the examples below). 

\code{rfeIter} is the basic algorithm while \code{rfe} wraps these operations inside of resampling. To avoid selection bias, it is better to use the function \code{rfe} than \code{rfeIter}.
}
\value{
A list with elements
  \item{finalVariables}{a list of size \code{length(sizes) + 1} containing the column names of the ``surviving'' predictors
           at each stage of selection. The first element corresponds to all the predictors (i.e. \code{size = ncol(x)})}
  \item{pred }{a data frame with columns for the test set outcome, the predicted outcome and the subset size.}

}

\author{Max Kuhn}

\seealso{\code{\link{rfeControl}}}
\examples{
\dontrun{
data(BloodBrain)

x <- scale(bbbDescr[,-nearZeroVar(bbbDescr)])
x <- x[, -findCorrelation(cor(x), .8)]
x <- as.data.frame(x)

set.seed(1)
lmProfile <- rfe(x, logBBB,
                 sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
                 rfeControl = rfeControl(functions = lmFuncs, 
                                         number = 200))
set.seed(1)
lmProfile2 <- rfe(x, logBBB,
                 sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
                 rfeControl = rfeControl(functions = lmFuncs, 
                                         rerank = TRUE, 
                                         number = 200))

xyplot(lmProfile$results$RMSE + lmProfile2$results$RMSE  ~ 
       lmProfile$results$Variables, 
       type = c("g", "p", "l"), 
       auto.key = TRUE)

rfProfile <- rfe(x, logBBB,
                 sizes = c(2, 5, 10, 20),
                 rfeControl = rfeControl(functions = rfFuncs))

bagProfile <- rfe(x, logBBB,
                  sizes = c(2, 5, 10, 20),
                  rfeControl = rfeControl(functions = treebagFuncs))

set.seed(1)
svmProfile <- rfe(x, logBBB,
                  sizes = c(2, 5, 10, 20),
                  rfeControl = rfeControl(functions = caretFuncs, 
                                          number = 200),
                  ## pass options to train()
                  method = "svmRadial")

## classification 

data(mdrr)
mdrrDescr <- mdrrDescr[,-nearZeroVar(mdrrDescr)]
mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .8)]

set.seed(1)
inTrain <- createDataPartition(mdrrClass, p = .75, list = FALSE)[,1]

train <- mdrrDescr[ inTrain, ]
test  <- mdrrDescr[-inTrain, ]
trainClass <- mdrrClass[ inTrain]
testClass  <- mdrrClass[-inTrain]

set.seed(2)
ldaProfile <- rfe(train, trainClass,
                  sizes = c(1:10, 15, 30),
                  rfeControl = rfeControl(functions = ldaFuncs, method = "cv"))
plot(ldaProfile, type = c("o", "g"))

postResample(predict(ldaProfile, test), testClass)

}

#######################################
## Parallel Processing Example via multicore

\dontrun{
library(doMC)

## Note: if the underlying model also uses foreach, the
## number of cores specified above will double (along with
## the memory requirements)
registerDoMC(cores = 2)

set.seed(1)
lmProfile <- rfe(x, logBBB,
                 sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
                 rfeControl = rfeControl(functions = lmFuncs, 
                                         number = 200))


}

}

\keyword{models}
