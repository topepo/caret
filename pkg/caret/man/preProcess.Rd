\name{preProcess}
\alias{preProcess}
\alias{preProcess.default}
\alias{predict.preProcess}

\title{Pre-Processing of Predictors }
\description{
Pre-processing transformation (centering, scaling etc.) can be estimated from the training data and applied to any data set with the same variables. 
}
\usage{
preProcess(x, ...)

\method{preProcess}{default}(x, 
           method = c("center", "scale"), 
           thresh = 0.95,
           pcaComp = NULL,
           na.remove = TRUE,
           k = 5,
           knnSummary = mean,
           outcome = NULL,
           fudge = .2,
           numUnique = 3,
           verbose = FALSE,
           ...)

\method{predict}{preProcess}(object, newdata, ...)
}

\arguments{
  \item{x}{a matrix or data frame. All variables must be numeric.}
  \item{method}{a character vector specifying the type of processing. Possible values are "BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "range", "knnImpute", "bagImpute", "medianImpute", pca", "ica" and "spatialSign" (see Details below)}
  \item{thresh}{a cutoff for the cumulative percent of variance to be retained by PCA}
  \item{pcaComp}{the specific number of PCA components to keep. If specified, this over-rides \code{thresh}}
  \item{na.remove}{a logical; should missing values be removed from the calculations?}
  \item{object}{an object of class \code{preProcess}}
  \item{newdata}{a matrix or data frame of new data to be pre-processed}
  \item{k}{the number of nearest neighbors from the training set to use for imputation }
  \item{knnSummary}{function to average the neighbor values per column during imputation}
  \item{outcome}{a numeric or factor vector for the training set outcomes. This can be used to help estimate the Box-Cox transformation of the predictor variables (see Details below)}
    \item{fudge}{
    a tolerance value: Box-Cox transformation lambda values within +/-fudge will be coerced to 0 and within 1+/-fudge will be coerced to 1
    }
      \item{numUnique}{
      how many unique values should \code{y} have to estimate the Box-Cox transformation?
      }
  \item{verbose}{a logical: prints a log as the computations proceed}

  \item{\dots}{additional arguments to pass to \code{\link[fastICA]{fastICA}}, such as \code{n.comp}}
}
\details{
The Box-Cox, Yeo-Johnson and exponential transformations have been "repurposed" here: they are being used to transform the predictor variables. The Box-Cox transformation was developed for transforming the response variable while another method, the Box-Tidwell transformation, was created to estimate transformations of predictor data. However, the Box-Cox method is simpler, more computationally efficient and is equally effective for estimating power transformations. The Yeo-Johnson transformation is similar to the Box-Cox model but can accommodate predictors with zero and/or negative values (while the predictors values for the Box-Cox transformation must be strictly positive.) The exponential transformation of Manly (1976) can also be used for positive or negative data.
 
The "range" transformation scales the data to be within [0, 1]. If new samples have values larger or smaller than those in the training set, values will be outside of this range.

The operations are applied in this order: Box-Cox/Yeo-Johnson transformation, centering, scaling, range, imputation, PCA, ICA then spatial sign. This is a departure from versions of \pkg{caret} prior to version 4.76 (where imputation was done first) and is not backwards compatible if bagging was used for imputation.

If PCA is requested but centering and scaling are not, the values will still be centered and scaled. Similarly, when ICA is requested, the data are automatically centered and scaled.

k-nearest neighbor imputation is carried out by finding the k closest samples (Euclidian distance) in the training set. Imputation via bagging fits a bagged tree model for each predictor (as a function of all the others). This method is simple, accurate and accepts missing values, but it has much higher computational cost. Imputation via medians takes the median of each predictor in the training set, and uses them to fill missing values.  This method is simple, fast, and accepts missing values, but treats each predictor independently, and may be inaccurate.

A warning is thrown if both PCA and ICA are requested. ICA, as implemented by the \code{\link[fastICA]{fastICA}} package automatically does a PCA decomposition prior to finding the ICA scores.

The function will throw an error of any variables in \code{x} has less than two unique values. 
}
\value{
\code{preProcess} results in a list with elements
  \item{call}{the function call}
  \item{dim}{the dimensions of \code{x}}
  \item{bc}{Box-Cox transformation values, see \code{\link{BoxCoxTrans}}}
  \item{mean}{a vector of means (if centering was requested)}
  \item{std}{a vector of standard deviations (if scaling or PCA was requested)}
  \item{rotation}{a matrix of eigenvectors if PCA was requested}
  \item{method}{the value of\code{method}}
  \item{thresh}{the value of\code{thresh}}
  \item{ranges}{a matrix of min and max values for each predictor when \code{method} includes "range" (and \code{NULL} otherwise)}
  \item{numComp}{the number of principal components required of capture the specified amount of variance}
  \item{ica}{contains values for the \code{W} and \code{K} matrix of the decomposition}
  \item{median}{a vector of medians (if median imputation was requested)}
}

\author{ Max Kuhn, median imputation by Zachary Mayer }

\references{
\url{http://topepo.github.io/caret/preprocess.html}

Kuhn and Johnson (2013), Applied Predictive Modeling, Springer, New York (chapter 4)


Kuhn (2008), Building predictive models in R using the caret (\url{http://www.jstatsoft.org/v28/i05/})

Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations (with discussion). Journal of the Royal Statistical Society B, 26, 211-252.

Box, G. E. P. and Tidwell, P. W. (1962) Transformation of the independent variables. Technometrics 4, 531-550.

Manly, B. L. (1976) Exponential data transformations. The Statistician, 25, 37 - 42.

Yeo, I-K. and Johnson, R. (2000). A new family of power transformations to improve normality or symmetry. Biometrika, 87, 954-959.
}

\seealso{\code{\link{BoxCoxTrans}}, \code{\link{expoTrans}} \code{\link[MASS]{boxcox}}, \code{\link[stats]{prcomp}}, \code{\link[fastICA]{fastICA}}, \code{\link{spatialSign}}}

\examples{
data(BloodBrain)
# one variable has one unique value
\dontrun{
preProc <- preProcess(bbbDescr)

preProc  <- preProcess(bbbDescr[1:100,-3])
training <- predict(preProc, bbbDescr[1:100,-3])
test     <- predict(preProc, bbbDescr[101:208,-3])
}
}

\keyword{ utilities }

