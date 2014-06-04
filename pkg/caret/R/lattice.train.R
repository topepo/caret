densityplot.train <- function(x,
                              data = NULL,
                              metric = x$metric,
                              ...)
  {

    if (!is.null(match.call()$data))
        warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")
    
    resamp <- x$resample
    tNames <- gsub("^\\.", "", names(x$bestTune))

    # adapt formula to work with muliple metrics
    mName <- names(resamp)[names(resamp) %in% metric][1]
    
    # Look for constant tuning parameters and remove them
    numVals <- unlist(
                      lapply(resamp,
                             function(u) length(unique(u))))
    if(any(numVals == 1))
      {
        # make sure that these are tuning parameters

        resamp <- resamp[, numVals > 1, drop = FALSE]
        tNames <- tNames[tNames %in% names(numVals)[numVals > 1]]
      }

    # Create the formula based on the data
    formText <- paste("~", mName)
    if(any(tNames %in% colnames(resamp)))
      {
        formText <- paste(formText,
                          "|",
                          paste(
                                tNames,
                                collapse = "*"))
      }

    form <- as.formula(formText)
    
    densityplot(form, data = resamp, ...)
}

histogram.train <- function(x,
                              data = NULL,
                              metric = x$metric,
                              ...)
  {

    if (!is.null(match.call()$data))
        warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")
    
    resamp <- x$resample
    tNames <- gsub("^\\.", "", names(x$bestTune))

    # adapt formula to work with muliple metrics
    mName <- names(resamp)[names(resamp) %in% metric][1]
    
    # Look for constant tuning parameters and remove them
    numVals <- unlist(
                      lapply(resamp,
                             function(u) length(unique(u))))
    if(any(numVals == 1))
      {
        # make sure that these are tuning parameters

        resamp <- resamp[, numVals > 1, drop = FALSE]
        tNames <- tNames[tNames %in% names(numVals)[numVals > 1]]
      }

    # Create the formula based on the data
    formText <- paste("~", mName)
    if(any(tNames %in% colnames(resamp)))
      {
        formText <- paste(formText,
                          "|",
                          paste(
                                tNames,
                                collapse = "*"))
      }

    form <- as.formula(formText)
    
    histogram(form, data = resamp, ...)
}


stripplot.train <- function(x,
                              data = NULL,
                              metric = x$metric,
                              ...)
  {
    if (!is.null(match.call()$data))
        warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")
    
    resamp <- x$resample
    tNames <- gsub("^\\.", "", names(x$bestTune))

    # adapt formula to work with muliple metrics
    mName <- names(resamp)[names(resamp) %in% metric][1]
    
    # Look for constant tuning parameters and remove them
    numVals <- unlist(
                      lapply(resamp,
                             function(u) length(unique(u))))
    if(any(numVals == 1))
      {
        # make sure that these are tuning parameters

        resamp <- resamp[, numVals > 1, drop = FALSE]
        tNames <- tNames[tNames %in% names(numVals)[numVals > 1]]
      }

    # determine which tuning parameter has the most values
    tNames1 <- names(which.max(numVals[names(numVals) %in% tNames]))
    tNames2 <- tNames[!(tNames %in% tNames1)]

    # The variable in tNames1 will be converted to a factor, so
    # we will make sure that numeric data gets changed correctly

    resamp[,tNames1] <- factor(
                               as.character(resamp[,tNames1]),
                               levels = paste(
                                 sort(unique(resamp[,tNames1]))))



    
    # Create the formula based on the data 
    if(any(tNames %in% colnames(resamp)))
      {
        theDots <- list(...)
        if(any(names(theDots) == "horizontal"))
           {
             formText <- if(theDots$horizontal) paste(tNames1, "~", mName) else paste(mName, "~", tNames1)
           } else  formText <- paste(tNames1, "~", mName)
        
        if(length(tNames2) > 0)
          {
            formText <- paste(formText,
                              "|",
                              paste(
                                    tNames2,
                                    collapse = "*"))
          }
        
      } else formText <- paste("~", mName)

    form <- as.formula(formText)
    
    stripplot(form, data = resamp, ...)
}


xyplot.train <- function(x,
                              data = NULL,
                              metric = x$metric,
                              ...)
  {
    if (!is.null(match.call()$data))
        warning("explicit 'data' specification ignored")

    if(x$control$method %in%  c("oob", "LOOCV"))
      stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")
    
    resamp <- x$resample
    tNames <- gsub("^\\.", "", names(x$bestTune))

    # adapt formula to work with muliple metrics
    mName <- names(resamp)[names(resamp) %in% metric][1]
    
    # Look for constant tuning parameters and remove them
    numVals <- unlist(
                      lapply(resamp,
                             function(u) length(unique(u))))
    if(any(numVals == 1))
      {
        # make sure that these are tuning parameters

        resamp <- resamp[, numVals > 1, drop = FALSE]
        tNames <- tNames[tNames %in% names(numVals)[numVals > 1]]
      }

    # determine which tuning parameter has the most values
    tNames1 <- names(which.max(numVals[names(numVals) %in% tNames]))
    tNames2 <- tNames[!(tNames %in% tNames1)]

   
    # Create the formula based on the data 
    if(any(tNames %in% colnames(resamp)))
      {
        formText <-  paste(mName, "~", tNames1)
        
        if(length(tNames2) > 0)
          {
            formText <- paste(formText,
                              "|",
                              paste(
                                    tNames2,
                                    collapse = "*"))
          }
        
      } else stop("there must be at least one tuning parameter for a scatter plot")

    form <- as.formula(formText)
    
    xyplot(form, data = resamp, ...)
}
