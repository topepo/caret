BoxCoxTrans <- function(y, ...) UseMethod("BoxCoxTrans")


##TODO add an epsilon?
## TODO add exclusion list to preProc?

BoxCoxTrans.default <- function(y, x = rep(1, length(y)), fudge = .2, numUnique = 3,  na.rm = FALSE, ...) {
  requireNamespaceQuietStop("MASS")
  requireNamespaceQuietStop("e1071")
  if(na.rm && (any(is.na(y)) | any(is.na(x)))) {
    rmv <- is.na(y) | is.na(x)
    y <- y[!rmv]
    x <- x[!rmv]
  }
  if(!is.numeric(y) | is.factor(y) | is.character(y)) stop("y must be numeric")
  
  if(any(y <= 0) | length(unique(y)) < numUnique) {
    out <- list(lambda = NA,
                summary = summary(y),
                ratio = NA,
                n = length(y))
  } else {
    bc <- MASS::boxcox(y~x, plotit = FALSE, ...)
    out <- list(lambda = bc$x[which.max(bc$y)])
  }
  out$fudge <- fudge
  out$n <- length(y)
  out$summary <- summary(y)
  out$ratio <- max(y)/min(y)
  out$skewness <- e1071::skewness(y)
  class(out) <- "BoxCoxTrans"
  out
} 

print.BoxCoxTrans <- function(x, newdata, digits = 3, ...){
  cat("Box-Cox Transformation\n\n")
  
  cat(x$n, "data points used to estimate Lambda\n\n")
  cat("Input data summary:\n")
  print(x$summary)
  if(!is.na(x$lambda)) {
    cat("\nLargest/Smallest:", signif(x$ratio, digits), "\n")
    cat("Sample Skewness:", signif(x$skewness, digits), "\n")      
    cat("\nEstimated Lambda:", signif(x$lambda, digits), "\n")
    if(x$lambda < x$fudge & x$lambda > -x$fudge)        
      cat("With fudge factor, Lambda = 0 will be used for transformations\n")
    if(x$lambda < 1+x$fudge & x$lambda > 1-x$fudge)
      cat("With fudge factor, no transformation is applied\n")
  } else cat("\nLambda could not be estimated; no transformation is applied\n")
  cat("\n")
  invisible(x)
}


predict.BoxCoxTrans <- function(object, newdata, ...) {
  if(!is.vector(newdata) || !is.numeric(newdata)) stop("newdata should be a numeric vector")
  if(is.na(object$lambda))  {
    out <- newdata
  } else {
    if(object$lambda < object$fudge & object$lambda > -object$fudge)  {
      if(any(newdata[!is.na(newdata)] <= 0)) warning("newdata should have values > 0")
      out <- log(newdata)
    } else {
      if(object$lambda < 1+object$fudge & object$lambda > 1-object$fudge) {
        out <- newdata
      } else out <- (newdata^object$lambda - 1)/object$lambda
    }
  }
  out
}
