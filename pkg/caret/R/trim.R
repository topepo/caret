"trim" <- function(x, ...) UseMethod("trim")

trim_cat <- function(old, new) {
  cat("Reduction of", signif(old - new), "or", 
      round((old - new)/old*100, 1), "%\n")
}

trim.rpart <- function(x, verbose = FALSE) {
  if(verbose) orig_size <- object.size(x)
  x$call <- list(na.action = (x$call)$na.action)
  x$x <- NULL
  x$y <- NULL
  x$where <- NULL
  if(verbose) {
    new_size <- object.size(x)
    trim_cat(orig_size, new_size)
  }
  x
}

trim.regbagg <- function(x, verbose = FALSE) {
  if(verbose) orig_size <- object.size(x)  
  x$mtrees <- lapply(x$mtrees, 
                     function(x){
                       x$bindx <- NULL
                       x$btree <- trim(x$btree)
                       x
                     } )
  if(verbose) {
    new_size <- object.size(x)
    trim_cat(orig_size, new_size)
  }
  x
}


