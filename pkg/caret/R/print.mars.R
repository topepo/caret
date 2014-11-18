print.bmars <- function (x, ...) 
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    if(!is.null(x$x))cat("Data:\n   # variables:\t", dim(x$x)[2], "\n   # samples:\t", dim(x$x)[1], "\n")
    cat(
      "\nModel:",
      "\n   B:        \t", x$B,
      "\n   degree:   \t", x$fit[[1]]$degree,
      "\n   nk:       \t", x$fit[[1]]$nk,      
      "\n   penalty:  \t", x$fit[[1]]$penalty,      
      "\n   threshold:\t", x$fit[[1]]$thresh, "\n")
     
    cat("\n")
    invisible(x)
}

