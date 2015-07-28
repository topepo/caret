"lfda" <- function(x, ...) UseMethod("lfda")

## The followings are helper functions for implementation of lfda
#' Matlab-Syntaxed Repmat
#' 
#' This function mimics the behavior and syntax of repmat() in Matlab
#' it generates a large matrix consisting of an N-by-M tiling copies of A 
#' 
#' @param A original matrix to be used as copies
#' @param N the number of rows of tiling copies of A
#' @param M the number of columns of tiling copies of A
#' 
#' @return matrix consisting of an N-by-M tiling copies of A
#' 
repmat <- function(A, N, M) {
  kronecker(matrix(1, N, M), A)
}
#' Negative One Half Matrix Power Operator
#'
#' This function defines operation for negative one half matrix 
#' power operator
#' 
#' @param x the matrix we want to operate on
#' @param n the exponent
#' 
#' @return the matrix after negative one half power
#' 
"%^%" <- function(x, n) {
  with(eigen(as.matrix(x)), vectors %*% (values^n * t(vectors)))
}
#' Local Fisher Discriminant Analysis for
#' Supervised Dimensionality Reduction
#'
#' Performs local fisher discriminant analysis (LFDA) on the given data. 
#' 
#' LFDA is a method for linear dimensionality reduction that maximizes
#' between-class scatter and minimizes within-class scatter while at the
#' same time maintain the local structure of the data so that multimodal
#' data can be embedded appropriately. Its limitation is that it only 
#' looks for linear boundaries between clusters. In this case, a non-linear
#' version called kernel LFDA will be used instead. Three metric types can
#' be used if needed. 
#' 
#' @importFrom rARPACK eigs
#' 
#' @export lfda
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
#' 
#' @return list of the LFDA results:
#' \item{T}{d x r transformation matrix (Z = x * T)}
#' \item{Z}{n x r matrix of dimensionality reduced samples}
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
#' @importFrom rARPACK eigs
#' 
#' @examples
#' \dontrun{
#' ## example without dimension reduction
#' k <- trainData[,-1]
#' y <- trainData[,1]
#' r <- 26 # dimensionality of reduced space. Here no dimension reduction is performed
#' result <- lfda_calc(k,y,r,metric="plain")
#' transformedMat <- result$Z # transformed training data
#' metric.train <- as.data.frame(cbind(trainData[,1],transformedMat))
#' colnames(metric.train) <- colnames(trainData)
#' 
#' ## example with dimension reduction
#' k <- trainData[,-1]
#' y <- trainData[,1]
#' r <- 3 # dimensionality of reduced space
#' 
#' result <- lfda_calc(k,y,r,metric="weighted")
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
#' 
lfda_calc <- function(x, y, r, metric = c("orthonormalized","plain","weighted"),knn = 5) {
  
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
  
  tSb <- mat.or.vec(d, d) # initialize between-class scatter matrix (to be maximized)
  tSw <- mat.or.vec(d, d) # initialize within-class scatter matrix (to be minimized)
  
  # compute the optimal scatter matrices in a classwise manner
  for (i in unique(as.vector(t(y)))) { 
    
    Xc <- x[, y == i, drop = FALSE] # data for this class
    nc <- ncol(Xc)
    
    # determine local scaling for locality-preserving projection
    Xc2 <- t(as.matrix(colSums(Xc^2)))
    # calculate the distance, using a self-defined repmat function that's the same 
    # as repmat() in Matlab
    distance2 <- repmat(Xc2, nc, 1) + repmat(t(Xc2), 1, nc) - 2 * t(Xc) %*% Xc
    sorted <- apply(distance2, 2, sort) # sort for each column by distance
    kNNdist2 <- t(as.matrix(sorted[knn + 1, ])) # knn-th nearest neighbor
    sigma <- sqrt(kNNdist2)
    
    localscale <- t(sigma) %*% sigma
    # use only non-zero entries in localscale since this will be used in the denominator
    # to calculate the affinity matrix 
    flag <- (localscale != 0) 
    
    # define affinity matrix - the larger the element in the matrix, the closer two data points are
    A <- mat.or.vec(nc, nc) 
    A[flag] <- exp(-distance2[flag]/localscale[flag]) 
    
    Xc1 <- as.matrix(rowSums(Xc))
    G <- Xc %*% (repmat(as.matrix(colSums(A)), 1, d) * t(Xc)) - Xc %*% A %*% t(Xc)
    tSb <- tSb + (G/n) + Xc %*% t(Xc) * (1 - nc/n) + Xc1 %*% (t(Xc1)/n)
    tSw <- tSw + G/nc
  }
  
  X1 <- as.matrix(rowSums(x))
  tSb <- tSb - X1 %*% t(X1)/n - tSw
  
  tSb <- (tSb + t(tSb))/2 # final between-class cluster matrix
  tSw <- (tSw + t(tSw))/2 # final within-class cluster matrix
  
  # find generalized eigenvalues and normalized eigenvectors of the problem
  if (r == d) { 
    # without dimensionality reduction
    eigTmp <- eigen(solve(tSw) %*% tSb)  # eigenvectors here are normalized     
    eigVec <- eigTmp$vectors
    eigVal <- as.matrix(eigTmp$values)
    
  } else { 
    # dimensionality reduction (select only the r largest eigenvalues of the problem)
    eigTmp <- rARPACK::eigs(A=solve(tSw) %*% tSb,k=r,which='LM') # r largest magnitude eigenvalues
    eigVec <- eigTmp$vectors
    eigVal <- as.matrix(eigTmp$values)
  }
  
  T0 <- eigVec # the raw transforming matrix
  
  # options to require a particular type of returned transform matrix
  # transforming matrix (do not change the "=" in the switch statement)
  Tr <- switch(metric,
               # this weighting scheme is explained in section 3.3 in the first reference
               weighted = T0 * repmat(t(sqrt(eigVal)), d, 1), 
               orthonormalized = qr.Q(qr(T0)),  
               plain = T0
  )
  
  Z <- t(t(Tr) %*% x) # transformed data
  
  return(list("T" = Tr, "Z" = Z))
}

#' This function trains a lfda metric, returns a transformed original matrix and transforming matrix used to 
#' transform other data set, usually for testing set
"lfda.default" <- 
  function(x, y, r = 3, metric = c("orthonormalized","plain","weighted"),knn = 5, ...)
  {
    if(is.data.frame(x)) x <- as.matrix(x)
    if(r==3){print("Reduced dimension to 3 by default. ")}

    metric <- match.arg(metric)
    modelArgs <- c(list(x,y,r,metric,knn))
    out <- do.call("lfda_calc", modelArgs)
    
    out$call <- NULL
    class(out) <- "lfda"
    out
  }

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
