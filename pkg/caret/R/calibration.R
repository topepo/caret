calibration <- function(x, ...) UseMethod("calibration")

calibration.default <- function(x, ...) stop("'x' should be a formula")

calibration.formula <- function(x, data = NULL, class = NULL, cuts = 11, subset = TRUE, lattice.options = NULL, ...)
{
  
  if (!is.null(lattice.options)) {
    oopt <- lattice.options(lattice.options)
    on.exit(lattice.options(oopt), add = TRUE)
  }
  
  formula <- x
  groups  <- NULL
  subset <- eval(substitute(subset), data, environment(x))
  
  form <- latticeParseFormula(formula, data, subset = subset, 
                              groups = groups, multiple = TRUE, outer = TRUE, 
                              subscripts = TRUE, drop = TRUE)
  calibData <- data.frame(prob = form$y)
  probNames <- strsplit(form$right.name, " + ", fixed = TRUE)[[1]]
  
  calibData <- data.frame(calibClassVar = rep(form$left, length(probNames)),
                          calibProbVar = form$right)
  calibData$calibModelVar <- if(length(probNames) > 1) form$condition[[length(form$condition)]] else probNames
  
  if(length(form$condition) > 0 && any(names(form$condition) != ""))
  {
    ind <- sum(names(form$condition) != "")
    tmp <- as.data.frame(form$condition[1:ind])
    calibData <- cbind(calibData, tmp)
  }
  if(!is.factor(calibData$calibClassVar)) stop("the left-hand side of the formula must be a factor of classes")
  
  splitVars <- names(calibData)[!(names(calibData) %in% c("calibClassVar", "calibProbVar"))]
  
  if(is.null(class)) class <- levels(calibData$calibClassVar)[1]
  plotData <- ddply(calibData, splitVars, calibCalc, class = class, cuts = cuts)    
  out <- list(data = plotData, cuts = cuts, class = class, probNames = probNames,
              call = match.call())
  class(out) <- "calibration"
  out
}


print.calibration <- function(x, ...)
{
  printCall(x$call)
  cat("Models:", paste(unique(x$data$calibModelVar), collapse = ", "), "\n")
  cat("Event: ", x$class, "\n")      
  cat("Cuts:", x$cuts, "\n")
  invisible(x)
}


calibCalc <- function(x, class = levels(obs)[1], cuts = 11) {
  if(length(cuts) == 1) {
    num_cuts <- cuts
    cuts <- (0:num_cuts)/num_cuts 
  } else {
    cuts <- unique(c(0, cuts, 1))
    num_cuts <- length(cuts)    
  }
  binData <-  data.frame(prob = x$calibProbVar,
                         bin = cut(x$calibProbVar, cuts, include.lowest = TRUE),
                         class = x$calibClassVar)
  
  dataPoints <- ddply(binData,
                      .(bin),
                      function(x, cls) c(Percent = mean(x$class == cls)*100,
                                         Count = sum(x$class == cls)),
                      cls = class,
                      .drop = FALSE)
  dataPoints$midpoint <- NA
  for(i in 2:length(cuts)) 
    dataPoints$midpoint[i-1] <- .5*(cuts[i] + cuts[i-1]) * 100
  dataPoints$Percent <- ifelse(dataPoints$Count == 0, 0, dataPoints$Percent)
  dataPoints
}

plot.calibration <- function(x, y = NULL, ...) 
  xyplot(x = x, data = NULL, ...)

xyplot.calibration <- function(x, data = NULL, ...)
{
  lFormula <- "Percent ~ midpoint"
  defaults <- c("calibModelVar", "bin", "Percent", "Count", "midpoint")
  extras <- names(x$data)[!(names(x$data) %in% defaults)]
  if(length(extras) > 0) lFormula <- paste(lFormula, paste(extras, collapse = "*"), sep = "|")
  
  rng <- extendrange(c(0, 100))
  
  opts <- list(...)
  if(!any(names(opts) == "xlab")) opts$xlab <- "Bin Midpoint"
  if(!any(names(opts) == "ylab")) opts$ylab <- "Observed Event Percentage"
  if(!any(names(opts) == "type")) opts$type <- "o"
  if(!any(names(opts) == "ylim")) opts$ylim <- rng   
  if(!any(names(opts) == "xlim")) opts$xlim <- rng
  if(!any(names(opts) == "panel")) opts$panel <- panel.calibration
  
  args <- list(x = as.formula(lFormula),
               data = x$data)
  if(length(x$probNames) > 1) args$groups <- x$data$calibModelVar
  
  args <- c(args, opts)    
  do.call("xyplot", args)    
}

panel.calibration <- function(...)
{
  panel.abline(0, 1,
               col = trellis.par.get("reference.line")$col,
               lwd = trellis.par.get("reference.line")$lwd,
               lty = trellis.par.get("reference.line")$lty)    
  panel.xyplot(...)
}
