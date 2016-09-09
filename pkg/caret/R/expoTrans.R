
#' @export
expoTrans <- function(y, ...) UseMethod("expoTrans")

#' @importFrom stats optim
#' @export
expoTrans.default <- function(y, na.rm  = TRUE, init = 0, lim = c(-4, 4), method = "Brent", numUnique = 3, ...)
{
  requireNamespaceQuietStop("e1071")
  if(any(is.na(y)) & !na.rm) stop("missing data found")
  call <- match.call()
  rat <- max(y, na.rm = TRUE)/min(y, na.rm = TRUE)
  if(length(unique(y[!is.na(y)])) >= numUnique)
  {
    results <- optim(init, manlyLik, x = y[!is.na(y)], neg = TRUE, method = method, 
                     lower = lim[1], upper = lim[2]) 
    out <- list(lambda = results$par, est = manly(y, results$par))
    if(length(unique(out$est)) == 1 | results$convergence > 0) 
      out <- list(lambda = NA, est = y)
  } else out <- list(lambda = NA, est = y)
  out$n <- sum(!is.na(y))
  out$skewness <- e1071::skewness(y, na.rm = TRUE)
  out$summary <- summary(y)
  out$ratio <- max(y, na.rm = TRUE)/min(y, na.rm = TRUE)
  out$class <- class
  class(out) <- "expoTrans"
  out
}

#' @importFrom stats optim
expoTrans.numeric <- function(y, na.rm  = TRUE, init = 0, lim = c(-4, 4), method = "Brent", numUnique = 3, ...)
{
  requireNamespaceQuietStop("e1071")
  if(any(is.na(y)) & !na.rm) stop("missing data found")
  call <- match.call()
  rat <- max(y, na.rm = TRUE)/min(y, na.rm = TRUE)
  if(length(unique(y[!is.na(y)])) >= numUnique)
  {
    results <- optim(init, manlyLik, x = y[!is.na(y)], neg = TRUE, method = method, 
                     lower = lim[1], upper = lim[2]) 
    out <- list(lambda = results$par, est = manly(y, results$par))
    if(length(unique(out$est)) == 1 | results$convergence > 0) 
      out <- list(lambda = NA, est = y)
  } else out <- list(lambda = NA, est = y)
  out$n <- sum(!is.na(y))
  out$skewness <- e1071::skewness(y, na.rm = TRUE)
  out$summary <- summary(y)
  out$ratio <- max(y, na.rm = TRUE)/min(y, na.rm = TRUE)
  out$class <- class
  class(out) <- "expoTrans"
  out
}

#' @export
print.expoTrans <- function(x, digits = max(3L, getOption("digits") - 3L), ...) 
{
  cat("Exponential Transformation\n\n")
  
  cat(x$n, "data points used to estimate Lambda\n\n")
  cat("Input data summary:\n")
  print(x$summary)
  cat("\nLargest/Smallest:", signif(x$ratio, digits), "\n")
  cat("Sample Skewness:", signif(x$skewness, digits), "\n")      
  if(!is.na(x$lambda))
  {
    cat("\nEstimated Lambda:", signif(x$lambda, digits), "\n")
  } else cat("\nLambda could not be estimated; no transformation is applied\n")
  cat("\n")
  invisible(x)
}
  
#' @export
predict.expoTrans <- function(object, newdata, ...)
{
  if(!is.vector(newdata) || !is.numeric(newdata)) stop("newdata should be a numeric vector")
  if(is.na(object$lambda))
  {
    out <- newdata
  } else out <- manly(newdata, object$lambda)
  out
}

  
manly <- function(x, lambda) 
  if(lambda == 0) x else (exp(lambda*x) - 1)/lambda

#' @importFrom stats var
manlyLik <- function(lambda, x, neg = FALSE)
{
  y <- manly(x, lambda)
  v <- var(y, na.rm = TRUE)
  L1 <- lambda * sum(x, na.rm= TRUE)
  L2 <- .5 * sum(y - mean(y, na.rm = TRUE))/v
  L3 <- sum(!is.na(x)) * log(sqrt(2*pi)*sqrt(v))
  out <- L1 - L2 - L3
  if(!is.finite(out) | is.na(out)) out <- .Machine$double.xmax  
  if(neg) -out else out
}

