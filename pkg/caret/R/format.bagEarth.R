format.bagEarth <- function(x, file = "", cat = TRUE, ...) 
{
  library(earth)

  eachEq <- lapply(
                   x$fit,
                   function(u, ...) format(u, ...),
                   ...)
  allEq <- paste(
                 "(",
                 paste(eachEq, collapse = "+"),
                 ") /",
                 x$B,
                 "\n")
  
  if(cat) cat(allEq, file = file) else return(allEq)  
}

