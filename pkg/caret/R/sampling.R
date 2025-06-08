#' Down- and Up-Sampling Imbalanced Data
#'
#' \code{downSample} will randomly sample a data set so that all classes have
#' the same frequency as the minority class. \code{upSample} samples with
#' replacement to make the class distributions equal
#'
#' Simple random sampling is used to down-sample for the majority class(es).
#' Note that the minority class data are left intact and that the samples will
#' be re-ordered in the down-sampled version.
#'
#' For up-sampling, all the original data are left intact and additional
#' samples are added to the minority classes with replacement.
#'
#' @aliases downSample upSample
#' @param x a matrix or data frame of predictor variables
#' @param y a factor variable with the class memberships
#' @param list should the function return \code{list(x, y)} or bind \code{x}
#' and \code{y} together? If \code{FALSE}, the output will be coerced to a data
#' frame.
#' @param yname if \code{list = FALSE}, a label for the class column
#' @return Either a data frame or a list with elements \code{x} and \code{y}.
#' @author Max Kuhn
#' @keywords utilities
#' @examples
#'
#' ## A ridiculous example...
#' data(oil)
#' table(oilType)
#' downSample(fattyAcids, oilType)
#'
#' upSample(fattyAcids, oilType)
#'
#'
#' @importFrom dplyr %>% arrange slice_sample
#' @export downSample
downSample <- function(x, y, list = FALSE, yname = "Class") {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x, stringsAsFactors = TRUE)
  }
  if (!is.factor(y)) {
    warning(
      "Down-sampling requires a factor variable as the response. The original data was returned."
    )
    return(list(x = x, y = y))
  }

  minClass <- min(table(y))
  x$.outcome <- y

  x <- x %>%
    slice_sample(by = ".outcome", n = minClass) %>%
    arrange(.outcome)
  y <- x$.outcome
  x <- x[, !(colnames(x) %in% c("y", ".outcome")), drop = FALSE]
  if (list) {
    if (inherits(x, "matrix")) {
      x <- as.matrix(x)
    }
    out <- list(x = x, y = y)
  } else {
    out <- cbind(x, y)
    colnames(out)[ncol(out)] <- yname
  }
  out
}

upsample_indices <- function(n, target_size) {
  idx <- seq_len(n)

  if (target_size > n) {
    idx <- c(idx, sample(idx, target_size - n, replace = TRUE))
  }

  idx
}

#' @importFrom dplyr n slice
#' @export
upSample <- function(x, y, list = FALSE, yname = "Class") {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x, stringsAsFactors = TRUE)
  }
  if (!is.factor(y)) {
    warning(
      "Up-sampling requires a factor variable as the response. The original data was returned."
    )
    return(list(x = x, y = y))
  }

  maxClass <- max(table(y))
  x$.outcome <- y

  # h/t Davis Vaughan: https://github.com/tidyverse/dplyr/issues/7689#issuecomment-2950762795
  x <- x %>%
    slice(upsample_indices(n(), maxClass), .by = ".outcome")
  y <- x$.outcome
  x <- x[,!(colnames(x) %in% c("y", ".outcome")), drop = FALSE]
  if (list) {
    if (inherits(x, "matrix")) {
      x <- as.matrix(x)
    }
    out <- list(x = x, y = y)
  } else {
    out <- cbind(x, y)
    colnames(out)[ncol(out)] <- yname
  }
  out
}




