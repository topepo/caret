#' Compute and predict the distances to class centroids
#'
#' @aliases classDist.default classDist predict.classDist
#' @description This function computes the class centroids and covariance matrix for a training set for determining Mahalanobis distances of samples to each class centroid.
#'
#'
#' @param x a matrix or data frame of predictor variables
#' @param y a numeric or factor vector of class labels
#' @param groups an integer for the number of bins for splitting a numeric outcome
#' @param pca a logical: should principal components analysis be  applied to the dataset prior to splitting the data by class?
#' @param keep an integer for the number of PCA components that should by used to predict new samples (\code{NULL} uses all within a tolerance of \code{sqrt(.Machine$double.eps)})
#' @param object an object of class \code{classDist}
#' @param newdata a matrix or data frame. If \code{vars} was previously specified, these columns should be in \code{newdata}
#' @param trans an optional function that can be applied to each class distance. \code{trans = NULL} will not apply a function
#' @param \dots optional arguments to pass (not currently used)
#'
#' @details
#' For factor outcomes, the data are split into groups for each class
#' and the mean and covariance matrix are calculated. These are then
#' used to compute Mahalanobis distances to the class centers (using
#' \code{predict.classDist} The function will check for non-singular matrices.
#'
#' For numeric outcomes, the data are split into roughly equal sized
#' bins based on \code{groups}. Percentiles are used to split the data.
#'
#' @return
#' for \code{classDist}, an object of class \code{classDist} with
#' elements:
#'   \item{values }{a list with elements for each class. Each element
#'                  contains a mean vector for the class centroid and the
#'                  inverse of the class covariance matrix}
#'   \item{classes}{a character vector of class labels}
#'   \item{pca}{the results of \code{\link[stats]{prcomp}} when
#'              \code{pca = TRUE}}
#'   \item{call}{the function call}
#'   \item{p}{the number of variables}
#'   \item{n}{a vector of samples sizes per class}
#'
#' For \code{predict.classDist}, a matrix with columns for each class.
#' The columns names are the names of the class with the prefix
#' \code{dist.}. In the case of numeric \code{y}, the class labels are
#' the percentiles. For example, of \code{groups = 9}, the variable names
#' would be \code{dist.11.11}, \code{dist.22.22}, etc.
#'
#' @author Max Kuhn
#'
#' @references Forina et al. CAIMAN brothers: A family of powerful classification and class modeling techniques. Chemometrics and Intelligent Laboratory Systems (2009) vol. 96 (2) pp. 239-245
#'
#' @seealso \code{\link[stats]{mahalanobis}}
#'
#' @examples
#' trainSet <- sample(1:150, 100)
#'
#' distData <- classDist(iris[trainSet, 1:4],
#'                       iris$Species[trainSet])
#'
#' newDist <- predict(distData,
#'                    iris[-trainSet, 1:4])
#'
#' splom(newDist, groups = iris$Species[-trainSet])
#'
#' @keywords manip
#' @export
classDist <- function (x, ...)  UseMethod("classDist")

#' @rdname classDist
#' @method classDist default
#' @importFrom stats cov predict quantile prcomp
#' @export
classDist.default <- function(x, y, groups = 5,
                              pca = FALSE,
                              keep = NULL,
                              ...)
{
  if(is.numeric(y))
    {
      y <- cut(y,
               unique(quantile(y, probs = seq(0, 1, length = groups + 1))),
               include.lowest = TRUE)
      classLabels <- paste(round((1:groups)/groups*100, 2))
      y <- factor(y)
      cuts <- levels(y)
    } else {
      classLabels <- levels(y)
      cuts <- NULL
    }

  p <- ncol(x)

  if(pca)
    {
      pca <- prcomp(x, center = TRUE, scale. = TRUE,
                    tol = sqrt(.Machine$double.eps))
      keep <- min(keep, ncol(pca$rotation))
      if(!is.null(keep)) pca$rotation <- pca$rotation[, 1:keep, drop = FALSE]
      x <- as.data.frame(predict(pca, newdata = x), stringsAsFactors = FALSE)
    } else pca <- NULL

  x <- split(x, y)

  getStats <- function(u)
    {
      if(nrow(u) < ncol(u))
        stop("there must be more rows than columns for this class")
      A <- try(cov(u), silent = TRUE)
      if(inherits(A, "try-error"))
        stop("Cannot compute the covariance matrix")
      A <- try(solve(A), silent = TRUE)
      if(inherits(A, "try-error"))
        stop("Cannot invert the covariance matrix")
      list(means = colMeans(u, na.rm = TRUE),
           A = A)
    }
  structure(
            list(values = lapply(x, getStats),
                 classes = classLabels,
                 cuts = cuts,
                 pca = pca,
                 call = match.call(),
                 p = p,
                 n = unlist(lapply(x, nrow))),
            class = "classDist")
}

#' @export
print.classDist <- function(x, ...)
  {
    printCall(x$call)

    if(!is.null(x$cuts))
      {
        cat("Classes based on", length(x$cuts) - 1,
            "cuts of the data\n")
        paste(x$cuts, collapse = " ")
        cat("\n")
      }

    if(!is.null(x$pca)) cat("PCA applied,",
                            ncol(x$pca$rotation),
                            "components retained\n\n")
    cat("# predictors variables:", x$p, "\n")
    cat("# samples:",
        paste(
              paste(x$n,
                    ifelse(is.null(x$cuts), " (", " "),
                    names(x$n),
                    ifelse(is.null(x$cuts), ")", ""),
                    sep = ""),
              collapse = ", "),
        "\n")
    invisible(x)
  }

#' @rdname classDist
#' @method predict classDist
#' @importFrom stats mahalanobis predict
#' @export
predict.classDist <- function(object, newdata, trans = log, ...)
{
  if(!is.null(object$pca))
    {
      newdata <- predict(object$pca, newdata = newdata)
    }

  pred <- function(a, x) mahalanobis(x, center = a$means, cov = a$A, inverted = TRUE)

  out <- lapply(object$values, pred, x = newdata)
  out <- do.call("cbind", out)
  colnames(out) <- paste("dist.", object$classes, sep = "")

  if(!is.null(trans)) out <- apply(out, 2, trans)
  out
}
