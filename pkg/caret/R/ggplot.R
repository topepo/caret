ggplot.train <- function(data = NULL, metric = data$metric[1], plotType = "scatter", output = "layered", ...) {
  if(!(output %in% c("data", "layered", "ggplot"))) stop("'outout' should be either 'data', 'ggplot' or 'layered'")
  params <- data$modelInfo$parameters$parameter
  paramData <- data$modelInfo$parameters
  
  if(grepl("adapt", data$control$method)) 
    warning("When using adaptive resampling, this plot may not accurately capture the relationship between the tuning parameters and model performance.")
  
  
  plotIt <- "yes"
  if(all(params == "parameter"))
  {
    plotIt <- "There are no tuning parameters for this model."
  } else {
    dat <- data$results
    
    ## Some exceptions for pretty printing
    if(data$method == "nb") dat$usekernel <- factor(ifelse(dat$usekernel, "Nonparametric", "Gaussian"))
    if(data$method == "gam") dat$select <- factor(ifelse(dat$select, "Feature Selection", "No Feature Selection"))
    if(data$method == "qrnn") dat$bag <- factor(ifelse(dat$bag, "Bagging", "No Bagging"))
    if(data$method == "C5.0") dat$winnow <- factor(ifelse(dat$winnow, "Winnowing", "No Winnowing"))
    if(data$method == "M5") dat$rules <- factor(ifelse(dat$rules == "Yes", "Rules", "Trees"))
    
    ## Check to see which tuning parameters were varied        
    paramValues <- apply(dat[,params,drop = FALSE],
                         2,
                         function(x) length(unique(x)))
    if(any(paramValues > 1))
    {
      params <- names(paramValues)[paramValues > 1]
      paramData <- subset(paramData, parameter %in% params)
    } else plotIt <- "There are no tuning parameters with more than 1 value."         
  }
  if(plotIt == "yes")
  {
    p <- length(params)
    dat <- dat[, c(metric, params)]
    
    resampText <- resampName(data, FALSE)
    resampText <- paste(metric, resampText)
  }  else stop(plotIt)
  p <- ncol(dat) - 1
  if(p > 1) {
    numUnique <- unlist(lapply(dat[, -1], function(x) length(unique(x))))
    numUnique <- sort(numUnique,  decreasing = TRUE)
    dat <- dat[, c(metric, names(numUnique))]
  }  
  if(output == "data") return(dat)
  
  if(plotType == "scatter") {
    dnm <- names(dat)
    if(p > 1 && is.numeric(dat[, 3])) dat[, 3] <- factor(format(dat[, 3]))
    out <- ggplot(dat, aes_string(x = dnm[2], y = dnm[1])) 
    out <- out + ylab(resampText) 
    out <- out + xlab(paramData$label[paramData$parameter == names(dat)[2]]) 
    
    if(output == "layered") {
      if(p >= 2) {
        out <- out + geom_point(aes_string(color = dnm[3]))
        out <- out + geom_line(aes_string(color = dnm[3]))
        out <- out + scale_colour_discrete(name = paramData$label[paramData$parameter == names(dat)[3]])
      } else out <- out + geom_point() + geom_line()
      ## TODO change legend (to blank when needed)
      if(p == 3) {
        out <- out + facet_wrap(as.formula(paste("~", names(dat)[4])))
      }
      if(p == 4) {
        out <- out + facet_grid(as.formula(paste(names(dat)[4], "~", names(dat)[5])))
      } 
      if(p > 4) stop("The function can only handle <= 4 tuning parameters for scatter plots. Use output = 'ggplot' to create your own")
    }
  }
  if(plotType == "level") {
    if(p == 1) stop("Two tuning parameters are required for a level plot")
    dnm <- names(dat)
    if(is.numeric(dat[,2])) dat[,2] <- factor(format(dat[,2]))
    if(is.numeric(dat[,3])) dat[,3] <- factor(format(dat[,3]))    
    ## TODO: use factor(format(x)) to make a solid block of colors?
    out <- ggplot(dat, aes_string(x = dnm[2], y = dnm[3], fill = metric)) 
    out <- out + ylab(paramData$label[paramData$parameter == names(dat)[3]]) 
    out <- out + xlab(paramData$label[paramData$parameter == names(dat)[2]]) 
    if(output == "layered") {
      out <- out + geom_tile()
      if(p == 3) {
        out <- out + facet_wrap(as.formula(paste("~", names(dat)[4])))
      }
      if(p == 4) {
        out <- out + facet_wrap(as.formula(paste("~", names(dat)[5])))
      }
      if(p == 5) {
        out <- out + facet_grid(as.formula(paste(names(dat)[5], "~", names(dat)[6])))
      } 
      if(p > 6) stop("The function can only handle <= 6 tuning parameters for level plots. Use output = 'ggplot' to create your own")
      
    }
  }  
  out
}


ggplot.rfe <- function(data = NULL, metric = data$metric[1], 
                       output = "layered", ...)
{
  if(!(output %in% c("data", "layered", "ggplot"))) 
    stop("'outout' should be either 'data', 'ggplot' or 'layered'")
  resampText <- resampName(data, FALSE)
  resampText <- paste(metric, resampText)
  if(output == "data") return(data$results)  
  notBest <- subset(data$results, Variables != data$bestSubset)
  best <- subset(data$results, Variables == data$bestSubset)  
  
  out <- ggplot(data$results, aes_string(x = "Variables", y = metric))
  if(output == "ggplot") return(out)
  out <- out + geom_line()
  out <- out + ylab(resampText)
  out <- out + geom_point(data = notBest, aes_string(x = "Variables", y = metric))
  
  out <- out + geom_point(data=best, aes_string(x = "Variables", y = metric),
                          size = 3, colour="blue")
  out
}
