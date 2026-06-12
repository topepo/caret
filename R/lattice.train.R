#' @importFrom stats as.formula
#' @export
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



#' Lattice functions for plotting resampling results
#' 
#' A set of lattice functions are provided to plot the resampled performance
#' estimates (e.g. classification accuracy, RMSE) over tuning parameters (if
#' any).
#' 
#' By default, only the resampling results for the optimal model are saved in
#' the \code{train} object. The function \code{\link{trainControl}} can be used
#' to save all the results (see the example below).
#' 
#' If leave-one-out or out-of-bag resampling was specified, plots cannot be
#' produced (see the \code{method} argument of \code{\link{trainControl}})
#' 
#' For \code{xyplot} and \code{stripplot}, the tuning parameter with the most
#' unique values will be plotted on the x-axis. The remaining parameters (if
#' any) will be used as conditioning variables. For \code{densityplot} and
#' \code{histogram}, all tuning parameters are used for conditioning.
#' 
#' Using \code{horizontal = FALSE} in \code{stripplot} works.
#' 
#' @aliases stripplot.train xyplot.train densityplot.train histogram.train
#' @param x An object produced by \code{\link{train}}
#' @param data This argument is not used
#' @param metric A character string specifying the single performance metric
#' that will be plotted
#' @param \dots arguments to pass to either
#' \code{\link[lattice:histogram]{histogram}},
#' \code{\link[lattice:histogram]{densityplot}},
#' \code{\link[lattice:xyplot]{xyplot}} or
#' \code{\link[lattice:xyplot]{stripplot}}
#' @return A lattice plot object
#' @author Max Kuhn
#' @seealso \code{\link{train}}, \code{\link{trainControl}},
#' \code{\link[lattice:histogram]{histogram}},
#' \code{\link[lattice:histogram]{densityplot}},
#' \code{\link[lattice:xyplot]{xyplot}},
#' \code{\link[lattice:xyplot]{stripplot}}
#' @keywords hplot
#' @examples
#' 
#' \dontrun{
#' 
#' library(mlbench)
#' data(BostonHousing)
#' 
#' library(rpart)
#' rpartFit <- train(medv ~ .,
#'                   data = BostonHousing,
#'                   "rpart", 
#'                   tuneLength = 9,
#'                   trControl = trainControl(
#'                     method = "boot", 
#'                     returnResamp = "all"))
#' 
#' densityplot(rpartFit,
#'             adjust = 1.25)
#' 
#' xyplot(rpartFit,
#'        metric = "Rsquared",
#'        type = c("p", "a"))
#' 
#' stripplot(rpartFit,
#'           horizontal = FALSE,
#'           jitter = TRUE)
#' 
#' }
#' @export

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

#' @importFrom stats as.formula
#' @export
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

#' @importFrom stats as.formula
#' @export
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
