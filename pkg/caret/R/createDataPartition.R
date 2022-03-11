#' Data Splitting functions
#'
#' A series of test/training partitions are created using
#' \code{createDataPartition} while \code{createResample} creates one or more
#' bootstrap samples. \code{createFolds} splits the data into \code{k} groups
#' while \code{createTimeSlices} creates cross-validation split for series data.
#' \code{groupKFold} splits the data based on a grouping factor.
#'
#'
#' For bootstrap samples, simple random sampling is used.
#'
#' For other data splitting, the random sampling is done within the levels of
#' \code{y} when \code{y} is a factor in an attempt to balance the class
#' distributions within the splits.
#'
#' For numeric \code{y}, the sample is split into groups sections based on
#' percentiles and sampling is done within these subgroups. For
#' \code{createDataPartition}, the number of percentiles is set via the
#' \code{groups} argument. For \code{createFolds} and \code{createMultiFolds},
#' the number of groups is set dynamically based on the sample size and
#' \code{k}.  For smaller samples sizes, these two functions may not do
#' stratified splitting and, at most, will split the data into quartiles.
#'
#' Also, for \code{createDataPartition}, very small class sizes (<= 3) the
#' classes may not show up in both the training and test data
#'
#' For multiple k-fold cross-validation, completely independent folds are
#' created.  The names of the list objects will denote the fold membership
#' using the pattern "Foldi.Repj" meaning the ith section (of k) of the jth
#' cross-validation set (of \code{times}). Note that this function calls
#' \code{createFolds} with \code{list = TRUE} and \code{returnTrain = TRUE}.
#'
#' Hyndman and Athanasopoulos (2013)) discuss rolling forecasting origin
#' techniques that move the training and test sets in time.
#' \code{createTimeSlices} can create the indices for this type of splitting.
#'
#' For Group k-fold cross-validation, the data are split such that no group
#' is contained in both the modeling and holdout sets. One or more group
#' could be left out, depending on the value of \code{k}.
#'
#' @aliases createDataPartition createResample createFolds createMultiFolds
#'   createTimeSlices groupKFold
#' @param y a vector of outcomes. For \code{createTimeSlices}, these should be
#'   in chronological order.
#' @param group a vector of groups whose length matches the number of rows in
#' the overall data set.
#' @param times the number of partitions to create
#' @param p the percentage of data that goes to training
#' @param list logical - should the results be in a list (\code{TRUE}) or a
#'   matrix with the number of rows equal to \code{floor(p * length(y))} and
#'   \code{times} columns.
#' @param groups for numeric \code{y}, the number of breaks in the quantiles
#'   (see below)
#' @param k an integer for the number of folds.
#' @param returnTrain a logical. When true, the values returned are the sample
#'   positions corresponding to the data used during training. This argument
#'   only works in conjunction with \code{list = TRUE}
#' @param initialWindow The initial number of consecutive values in each
#'   training set sample
#' @param horizon the number of consecutive values in test set sample
#' @param fixedWindow logical, if \code{FALSE}, all training samples start at 1
#' @param skip integer, how many (if any) resamples to skip to thin the total
#'   amount
#' @return A list or matrix of row position integers corresponding to the
#'   training data. For \code{createTimeSlices} subsamples are named by the end
#'   index of each training subsample.
#' @author Max Kuhn, \code{createTimeSlices} by Tony Cooper
#' @references \url{http://topepo.github.io/caret/data-splitting.html}
#'
#' Hyndman and Athanasopoulos (2013), Forecasting: principles and practice.
#' \url{https://otexts.com/fpp2/}
#' @keywords utilities
#' @examples
#'
#' data(oil)
#' createDataPartition(oilType, 2)
#'
#' x <- rgamma(50, 3, .5)
#' inA <- createDataPartition(x, list = FALSE)
#'
#' plot(density(x[inA]))
#' rug(x[inA])
#'
#' points(density(x[-inA]), type = "l", col = 4)
#' rug(x[-inA], col = 4)
#'
#' createResample(oilType, 2)
#'
#' createFolds(oilType, 10)
#' createFolds(oilType, 5, FALSE)
#'
#' createFolds(rnorm(21))
#'
#' createTimeSlices(1:9, 5, 1, fixedWindow = FALSE)
#' createTimeSlices(1:9, 5, 1, fixedWindow = TRUE)
#' createTimeSlices(1:9, 5, 3, fixedWindow = TRUE)
#' createTimeSlices(1:9, 5, 3, fixedWindow = FALSE)
#'
#' createTimeSlices(1:15, 5, 3)
#' createTimeSlices(1:15, 5, 3, skip = 2)
#' createTimeSlices(1:15, 5, 3, skip = 3)
#'
#' set.seed(131)
#' groups <- sort(sample(letters[1:4], size = 20, replace = TRUE))
#' table(groups)
#' folds <- groupKFold(groups)
#' lapply(folds, function(x, y) table(y[x]), y = groups)
#' @export createDataPartition
createDataPartition <- function (y, times = 1, p = 0.5, list = TRUE, groups = min(5, length(y))){
  if(inherits(y, "Surv")) y <- y[,"time"]
  out <- vector(mode = "list", times)

  if(length(y) < 2) stop("y must have at least 2 data points")

  if(groups < 2) groups <- 2

  if(is.numeric(y)) {
    y <- cut(y,
             unique(quantile(y, probs = seq(0, 1, length = groups))),
             include.lowest = TRUE)
  } else {
    xtab <- table(y)
    if(any(xtab == 0)) {
      warning(paste("Some classes have no records (",
                    paste(names(xtab)[xtab  == 0], sep = "", collapse = ", "),
                    ") and these will be ignored"))
      y <- factor(as.character(y))
    }
    if(any(xtab == 1)) {
      warning(paste("Some classes have a single record (",
                    paste(names(xtab)[xtab  == 1], sep = "", collapse = ", "),
                    ") and these will be selected for the sample"))
    }
  }

  subsample <- function(dat, p) {
    if(nrow(dat) == 1) {
      out <- dat$index
    } else {
      num <- ceiling(nrow(dat) * p)
      out <- sample(dat$index, size = num)
    }
    out
  }

  for (j in 1:times) {
    tmp <- dlply(data.frame(y = y, index = seq(along = y)),
                 .(y), subsample, p = p)
    tmp <- sort(as.vector(unlist(tmp)))
    out[[j]] <- tmp
  }

  if (!list) {
    out <- matrix(unlist(out), ncol = times)
    colnames(out) <- prettySeq(1:ncol(out))
  } else {
    names(out) <- prettySeq(out)
  }
  out
}


#' @rdname createDataPartition
#' @importFrom stats quantile
#' @export
"createFolds" <-
  function(y, k = 10, list = TRUE, returnTrain = FALSE) {
    if(inherits(y, "Surv")) y <- y[,"time"]
    if(is.numeric(y)) {
      ## Group the numeric data based on their magnitudes
      ## and sample within those groups.

      ## When the number of samples is low, we may have
      ## issues further slicing the numeric data into
      ## groups. The number of groups will depend on the
      ## ratio of the number of folds to the sample size.
      ## At most, we will use quantiles. If the sample
      ## is too small, we just do regular unstratified
      ## CV
      cuts <- floor(length(y)/k)
      if(cuts < 2) cuts <- 2
      if(cuts > 5) cuts <- 5
      breaks <- unique(quantile(y, probs = seq(0, 1, length = cuts)))
      y <- cut(y, breaks, include.lowest = TRUE)
    }

    if(k < length(y)) {
      ## reset levels so that the possible levels and
      ## the levels in the vector are the same
      y <- factor(as.character(y))
      numInClass <- table(y)
      foldVector <- vector(mode = "integer", length(y))

      ## For each class, balance the fold allocation as far
      ## as possible, then resample the remainder.
      ## The final assignment of folds is also randomized.
      for(i in 1:length(numInClass)) {
        ## create a vector of integers from 1:k as many times as possible without
        ## going over the number of samples in the class. Note that if the number
        ## of samples in a class is less than k, nothing is produced here.
        min_reps <- numInClass[i] %/% k
        if(min_reps > 0) {
          spares <- numInClass[i] %% k
          seqVector <- rep(1:k, min_reps)
          ## add enough random integers to get  length(seqVector) == numInClass[i]
          if(spares > 0) seqVector <- c(seqVector, sample(1:k, spares))
          ## shuffle the integers for fold assignment and assign to this classes's data
          foldVector[which(y == names(numInClass)[i])] <- sample(seqVector)
        } else {
          ## Here there are less records in the class than unique folds so
          ## randomly sprinkle them into folds.
          foldVector[which(y == names(numInClass)[i])] <- sample(1:k, size = numInClass[i])
        }
      }
    } else foldVector <- seq(along = y)

    if(list) {
      out <- split(seq(along = y), foldVector)
      names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))), sep = "")
      if(returnTrain) out <- lapply(out, function(data, y) y[-data], y = seq(along = y))
    } else out <- foldVector
    out
  }

#' @rdname createDataPartition
#' @export
createMultiFolds <- function(y, k = 10, times = 5) {
  if(inherits(y, "Surv")) y <- y[,"time"]
  prettyNums <- paste("Rep", gsub(" ", "0", format(1:times)), sep = "")
  for(i in 1:times) {
    tmp <- createFolds(y, k = k, list = TRUE, returnTrain = TRUE)
    names(tmp) <- paste("Fold",
                        gsub(" ", "0", format(seq(along = tmp))),
                        ".",
                        prettyNums[i],
                        sep = "")
    out <- if(i == 1) tmp else c(out, tmp)

  }
  out
}

## From Tony Cooper <tonyc@iconz.co.nz> on 1/9/13
#' @rdname createDataPartition
#' @export
createTimeSlices <- function(y, initialWindow, horizon = 1, fixedWindow = TRUE, skip = 0) {
  ## initialwindow = initial number of consecutive values in each training set sample
  ## horizon = number of consecutive values in test set sample
  ## fixedwindow = FALSE if we use the maximum possible length for the training set
  ## Ensure that initialwindow + horizon <= length(y)

  stops <- seq(initialWindow, (length(y) - horizon), by = skip + 1)

  if (fixedWindow) {
    starts <- stops - initialWindow + 1
  } else {
    starts <- rep(1, length(stops)) # all start at 1
  }

  train <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  test <- mapply(seq, stops+1, stops+horizon, SIMPLIFY = FALSE)
  nums <- gsub(" ", "0", format(stops))
  names(train) <- paste("Training", nums, sep = "")
  names(test) <- paste("Testing", nums, sep = "")

  out <- list(train = train, test = test)

  out
}

#' @rdname createDataPartition
#' @export
groupKFold <- function(group, k = length(unique(group))) {
  g_unique <- unique(group)
  m <- length(g_unique)
  if (k > m) {
    stop("`k` should be less than ", m)
  }
  g_folds <- sample(k, size = m, replace = TRUE)
  # Distribute obs to hold-out
  out <- split(seq_along(group), g_folds[match(group, g_unique)])
  names(out) <- paste0("Fold", gsub(" ", "0", format(seq_along(out))))
  # Switch from hold-out to fold
  lapply(out, function(z) seq_along(group)[-z])
}

make_resamples <- function(ctrl_obj, outcome) {
  n <- length(outcome)
  if(is.null(ctrl_obj$index)) {
    if(ctrl_obj$method == "custom")
      stop("'custom' resampling is appropriate when the `trControl` argument `index` is used", call. = FALSE)
    ctrl_obj$index <-
      switch(tolower(ctrl_obj$method),
             oob = NULL,
             none = list(seq(along = outcome)),
             apparent = list(all = seq(along = outcome)),
             alt_cv =, cv = createFolds(outcome, ctrl_obj$number, returnTrain = TRUE),
             repeatedcv =, adaptive_cv = createMultiFolds(outcome, ctrl_obj$number, ctrl_obj$repeats),
             loocv = createFolds(outcome, n, returnTrain = TRUE),
             boot =, boot632 =, optimism_boot =, boot_all =,
             adaptive_boot = createResample(outcome, ctrl_obj$number),
             test = createDataPartition(outcome, 1, ctrl_obj$p),
             adaptive_lgocv =, lgocv = createDataPartition(outcome, ctrl_obj$number, ctrl_obj$p),
             timeslice = createTimeSlices(seq(along = outcome),
                                          initialWindow = ctrl_obj$initialWindow,
                                          horizon = ctrl_obj$horizon,
                                          fixedWindow = ctrl_obj$fixedWindow,
                                          skip = ctrl_obj$skip)$train,
             stop("Not a recognized resampling method.", call. = FALSE))
  } else {
    index_types <- unlist(lapply(ctrl_obj$index, is.integer))
    if(!isTRUE(all(index_types)))
      stop("`index` should be lists of integers.", call. = FALSE)
    if(!is.null(ctrl_obj$indexOut)) {
      index_types <- unlist(lapply(ctrl_obj$indexOut, is.integer))
      if(!isTRUE(all(index_types)))
        stop("`indexOut` should be lists of integers.", call. = FALSE)
    }
  }

  if(ctrl_obj$method == "apparent")
    ctrl_obj$indexOut <- list(all = seq(along = outcome))

  ## Create holdout indices
  if(is.null(ctrl_obj$indexOut) && ctrl_obj$method != "oob"){
    if(tolower(ctrl_obj$method) != "timeslice") {
      y_index <-
        if (inherits(outcome, "Surv"))
          1:nrow(outcome)
      else
        seq(along = outcome)
      ctrl_obj$indexOut <-
        lapply(ctrl_obj$index, function(training)
          setdiff(y_index, training))
      if(ctrl_obj$method %in% c("optimism_boot", "boot_all")) {
        ctrl_obj$indexExtra <- lapply(ctrl_obj$index, function(training) {
          list(origIndex = y_index, bootIndex = training)
        })
      }
      names(ctrl_obj$indexOut) <- prettySeq(ctrl_obj$indexOut)
    } else {
      ctrl_obj$indexOut <-
        createTimeSlices(seq(along = outcome),
                         initialWindow = ctrl_obj$initialWindow,
                         horizon = ctrl_obj$horizon,
                         fixedWindow = ctrl_obj$fixedWindow,
                         skip = ctrl_obj$skip)$test
    }
  }

  if(ctrl_obj$method != "oob" & is.null(ctrl_obj$index))
    names(ctrl_obj$index) <- prettySeq(ctrl_obj$index)
  if(ctrl_obj$method != "oob" & is.null(names(ctrl_obj$index)))
    names(ctrl_obj$index)    <- prettySeq(ctrl_obj$index)
  if(ctrl_obj$method != "oob" & is.null(names(ctrl_obj$indexOut)))
    names(ctrl_obj$indexOut) <- prettySeq(ctrl_obj$indexOut)
  ctrl_obj
}




