


downSample <- function(x, y, list = FALSE, yname = "Class")
  {
    xc <- class(x)
    if(!is.data.frame(x)) x <- as.data.frame(x)
    if(!is.factor(y))
      {
        warning("Down-sampling requires a factor variable as the response. The original data was returned.")
        return(list(x = x, y = y))
      }

    minClass <- min(table(y))
    x$.outcome <- y
    
    x <- ddply(x, .(y),
               function(dat, n) dat[sample(seq(along = dat$.outcome), n),,drop = FALSE],
               n = minClass)
    y <- x$.outcome
    x <- x[, !(colnames(x) %in% c("y", ".outcome")), drop = FALSE]
    if(list)
      {
        if(xc[1] == "matrix") x <- as.matrix(x)
        out <- list(x = x, y = y)
      } else {
        out <- cbind(x, y)
        colnames(out)[ncol(out)] <- yname
      }
    out
  }



upSample <- function(x, y, list = FALSE, yname = "Class")
  {
    xc <- class(x)
    if(!is.data.frame(x)) x <- as.data.frame(x)
    if(!is.factor(y))
      {
        warning("Up-sampling requires a factor variable as the response. The original data was returned.")
        return(list(x = x, y = y))
      }

    maxClass <- max(table(y))
    x$.outcome <- y
    
    x <- ddply(x, .(y),
               function(x, top = maxClass)
              {
                if(nrow(x) < top)
                  {
                    ind <- sample(1:nrow(x),
                                  size = top - nrow(x),
                                  replace = TRUE)
                    ind <- c(1:nrow(x), ind)
                    x <- x[ind,,drop = FALSE]
                  }
                x
              })
    y <- x$.outcome
    x <- x[, !(colnames(x) %in% c("y", ".outcome")), drop = FALSE]
    if(list)
      {
        if(xc[1] == "matrix") x <- as.matrix(x)
        out <- list(x = x, y = y)
      } else {
        out <- cbind(x, y)
        colnames(out)[ncol(out)] <- yname
      }
    out
  }




