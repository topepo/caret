classDist <- function (x, ...)  UseMethod("classDist")

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
      x <- as.data.frame(predict(pca, newdata = x))
    } else pca <- NULL
  
  x <- split(x, y)
  getStats <- function(u)
    {
      if(nrow(u) < ncol(u))
        stop("there must be more rows than columns for this class")
      A <- try(cov(u), silent = TRUE)
      if(class(A) == "try-error")
        stop("Cannot compute the covariance matrix")
      A <- try(solve(A), silent = TRUE)
      if(class(A) == "try-error")
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
