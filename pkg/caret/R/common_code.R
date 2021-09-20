#' @importFrom stats t.test
get_fitness_differences <- function(pnames, subsets, fitness, label) {
  signs <- lapply(subsets, index2vec, vars = length(pnames))
  signs <- do.call("rbind", signs)
  colnames(signs) <- pnames
  nzv <- apply(signs, 2, function(x) min(table(factor(x, levels = paste(0:1)))) > 1)
  if(all(!nzv)) return(NULL)
  signs <- signs[, nzv, drop = FALSE]
  if(!is.matrix(fitness)) fitness <- as.matrix(fitness)
  snr <- function(x, y) {
    apply(y, 2,
          function(outcome, ind)
            t.test(outcome[ind == 1], outcome[ind == 0])$statistic,
          ind = x)
  }
  melt(apply(signs, 2, snr, y = fitness))
}

#' @importFrom stats reshape
process_diffs <- function(x, pnames) {
  is_null_res <- vapply(x, is.null, logical(1))
  if (all(is_null_res)) {
    stop("Not enough results to compute differences")
  }
  x <- x[!is_null_res]
  x <- do.call("rbind", x)
  mean_diffs <- ddply(x, c("Var1", "Var2"),
                      function(x) c(mean = mean(x$value, na.rm = TRUE)))

  mean_diffs <- reshape(mean_diffs, direction = "wide",
                        v.names = "mean",
                        idvar = "Var2",
                        timevar = "Var1")
  names(mean_diffs) <- gsub("mean\\.", "", names(mean_diffs))
  names(mean_diffs)[1] <- "Variable"
  included <- pnames %in% as.character(mean_diffs$Variable)
  if(any(!included)) {
    extras <- data.frame(Variable = pnames[!included])
    for(i in 2:ncol(mean_diffs)) extras[, names(mean_diffs)[i]] <- NA_real_
    mean_diffs <- rbind(mean_diffs, extras)
  }
  mean_diffs
}


same_args <- function(a, b) {
  if(length(a) != length(b)) return(FALSE)
  if(!isTRUE(all.equal(sort(a), sort(b)))) return(FALSE)
  TRUE
}

getOper <- function(x) if(x)  `%dopar%` else  `%do%`

jack_sim <- function(a, b) {
  if(is.matrix(a) && nrow(a) > 1) a <- a[1,,drop=FALSE]
  if(is.matrix(a) && nrow(b) > 1) b <- b[1,,drop=FALSE]
  sum(a ==1 & b ==1)/(sum(a == 1 & b == 0)+sum(a == 0 & b == 1)+sum(a ==1 & b ==1))*100
}



#' Convert indicies to a binary vector
#'
#' The function performs the opposite of \code{which} converting a set of
#' integers to a binary vector
#'
#'
#' @param x a vector of integers
#' @param vars the number of possible locations
#' @param sign a lgical; when true the data are encoded as -1/+1, and 0/1
#' otherwise
#' @return a numeric vector
#' @author Max Kuhn
#' @examples
#'
#' index2vec(x = 1:2, vars = 5)
#' index2vec(x = 1:2, vars = 5, sign = TRUE)
#'
#' @export index2vec

index2vec <- function(x, vars, sign = FALSE) {
  bin <- rep(0, vars)
  bin[x] <- 1
  if(sign) bin <- ifelse(bin == 0, -1, 1)
  bin
}


change_text <- function(old, new, p, show_diff = TRUE) {
  a <- index2vec(new, p)
  b <- index2vec(old, p)
  size_diff <- length(new) - length(old)
  if(show_diff) {
    if(abs(size_diff) >= 0) {
      num_text <- if(size_diff >= 0) paste0(length(old), "+", size_diff) else paste0(length(old), size_diff)
    } else num_text <- paste(length(old))
  } else {
    old_len <- length(old)
    new_len <- length(new)
    num_text <- paste0(format(1:p)[old_len], "->", format(1:p)[new_len])
  }
  sim <- sprintf("%.1f", jack_sim(a, b))
  num_text <- paste0(" (", num_text, ", ", sim, "%)")
  num_text
}

#' @export
predictors.gafs <- function(x, ...) {
 x$best_vars
}

#' @export
predictors.safs <- function(x, ...) {
  x$best_vars
}

