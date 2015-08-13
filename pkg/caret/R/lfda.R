#' Local Fisher Discriminant Analysis
#'
#' @param x The data frame to be used to train the metric
#' @param ... Additional arguments
"lfda" <- function(x, ...) UseMethod("lfda")

#' Training LFDA Metric
#'
#' This function trains a lfda metric, returns a transformed original matrix and transforming matrix used to
#' transform other data set, usually for testing set
#'
#' @param x n x d matrix of original samples.
#'          n is the number of samples.
#' @param y length n vector of class labels
#' @param r dimensionality of reduced space (default: d)
#' @param metric type of metric in the embedding space (no default)
#'               'weighted'        --- weighted eigenvectors
#'               'orthonormalized' --- orthonormalized
#'               'plain'           --- raw eigenvectors
#' @param knn parameter used in local scaling method (default: 5)
#' @param ... Additional arguments
#' @return list of the LFDA results:
#' \item{T}{d x r transformation matrix (Z = x * T)}
#' \item{Z}{n x r matrix of dimensionality reduced samples}
#'
#' @author Yuan Tang
#'
#' @keywords lfda local fisher discriminant transformation mahalanobis metric
#'
#' @author Yuan Tang
#'
#' @references
#' Sugiyama, M (2007).
#' Dimensionality reduction of multimodal labeled data by
#' local Fisher discriminant analysis.
#' \emph{Journal of Machine Learning Research}, vol.\bold{8}, 1027--1061.
#'
#' Sugiyama, M (2006).
#' Local Fisher discriminant analysis for supervised dimensionality reduction.
#' In W. W. Cohen and A. Moore (Eds.), \emph{Proceedings of 23rd International
#' Conference on Machine Learning (ICML2006)}, 905--912.
#'
#' #' @examples
#' \dontrun{
#' ## example without dimension reduction
#' k <- trainData[,-1]
#' y <- trainData[,1]
#' r <- 26 # dimensionality of reduced space. Here no dimension reduction is performed
#' result <- lfda(k,y,r,metric="plain")
#' transformedMat <- result$Z # transformed training data
#' metric.train <- as.data.frame(cbind(trainData[,1],transformedMat))
#' colnames(metric.train) <- colnames(trainData)
#'
#' ## example with dimension reduction
#' k <- trainData[,-1]
#' y <- trainData[,1]
#' r <- 3 # dimensionality of reduced space
#'
#' result <- lfda(k,y,r,metric="weighted")
#' transformMat  <- result$T # transforming matrix - distance metric
#'
#' # transformed training data with Style
#' transformedMat <- result$Z # transformed training data
#' metric.train <- as.data.frame(cbind(trainData[,1],transformedMat))
#' colnames(metric.train)[1] <- "Style"
#'
#' # transformed testing data with Style
#' metric.test <- as.matrix(testData[,-1]) %*% transformMat
#' metric.test <- as.data.frame(cbind(testData[,1],metric.test))
#' colnames(metric.test)[1] <- "Style"
#' }
#'
"lfda.default" <-
  function(x, y, r = 3, metric = c("orthonormalized","plain","weighted"),knn = 5, ...)
  {
    # TODO: The following checkings for input arguments need to be implemented in lfda package
    metric <- match.arg(metric) # the type of the transforming matrix (metric)
    x <- t(as.matrix(x)) # transpose of original samples
    y <- t(as.matrix(y)) # transpose of original class labels
    d <- nrow(x) # number of predictors
    n <- ncol(x) # number of samples

    if(is.null(r)){
      r <- d # if no dimension reduction requested, set r to d
    } else{
      if(r>d){stop("variable r needs to be no greater than the dimension of the data set. ")}
      if(r<=0){stop("variable r cannot be negative. ")}
    }

    if(!is.null(knn)){
      if(knn<=0){stop("variable knn needs to be positive. ")}
      if(d<=knn){stop("dimension of the data set must be greater than the variable knn. ")}
    }

    if(is.data.frame(x)) x <- as.matrix(x)
    if(r==3){print("Reduced dimension to 3 by default. ")}

    metric <- match.arg(metric)
    modelArgs <- c(list(x,y,r,metric,knn))
    out <- do.call("lfda", modelArgs)

    out$call <- NULL
    class(out) <- "lfda"
    out
  }

#' LFDA Transformation
#'
#' This function transforms a data set, usually a testing set, using the trained LFDA metric
#' @param object The result from lfda function, which contains a transformed data and a transforming
#'        matrix that can be used for transforming testing set
#' @param newdata The data to be transformed
#' @param type The output type, in this case it defaults to "raw" since the output is a matrix
#' @param ... Additional arguments
#' @return the transformed matrix
#' @author Yuan Tang
"predict.lfda" <- # for transforming a testing set
function(object, newdata = NULL, type = "raw", ...)
  {
    if(is.null(newdata)){stop("You must provide data to be used for transformation. ")}
    if(type!="raw"){stop('Types other than "raw" are currently unavailable. ')}
    if(is.data.frame(newdata)) newdata <- as.matrix(newdata)

    transformMatrix <- object$T

    result <- newdata %*% transformMatrix
    result
}

"print.lfda" <- function(x, ...){
  cat("Results for Local Fisher Discriminant Analysis \n\n")
  cat("The trained transforming matric is: \n")
  print(head(x$T))

  cat("\n\n The original data set after applying this metric transformation is:  \n")
  print(head(x$Z))

  cat("\n")
  cat("Only partial output is shown above. Please see the model output for more details. \n")
  invisible(x)
}
