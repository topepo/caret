"resamples" <- function(x, ...) UseMethod("resamples")

resamples.default <- function(x, modelNames = names(x), ...) {
  
  ## Do a lot of checkin of the object types and make sure
  ## that they actually used the samples samples in the resamples
  if(length(x) < 2) stop("at least two train objects are needed")
  classes <- unlist(lapply(x, function(x) class(x)[1]))
  #     if(!all(classes %in% c("sbf", "rfe", "train"))) stop("all objects in x must of class train, sbf or rfe")
  
  if(is.null(modelNames)){
    modelNames <- well_numbered("Model", length(x)) 
    
  } else {
    if(any(modelNames == "")) {
      no_name <- which(modelNames == "")
      modelNames[no_name] <- well_numbered("Model", length(x))[no_name] 
    }
  }
  
  numResamp <- unlist(lapply(x, function(x) length(x$control$index)))
  if(length(unique(numResamp)) > 1) stop("There are different numbers of resamples in each model")
  
  
  for(i in 1:numResamp[1]){
    indices <- lapply(x, function(x, i) sort(x$control$index[[1]]), i = i)
    uniqueIndex <- length(table(table(unlist(indices))))
    if(length(uniqueIndex) > 1) stop("The samples indices are not equal across resamples")
  }
  
  getTimes <- function(x){
    out <- rep(NA, 3)
    if(all(names(x) != "times")) return(out)
    if(any(names(x$times) == "everything")) out[1] <- x$time$everything[3]
    if(any(names(x$times) == "final")) out[2] <- x$time$final[3]
    if(any(names(x$times) == "prediction")) out[3] <- x$time$prediction[3]
    out
  }
  
  rs_values <- vector(mode = "list", length = length(x))
  for(i in seq(along = x)) {
    if(class(x[[i]])[1] == "rfe" && x[[i]]$control$returnResamp == "all"){ 
      warning(paste0("'", modelNames[i], "' did not have 'returnResamp=\"final\"; the optimal subset is used"))
    }
    if(class(x[[i]])[1] == "train" && x[[i]]$control$returnResamp == "all"){
      warning(paste0("'", modelNames[i], "' did not have 'returnResamp=\"final\"; the optimal tuning parameters are used"))
    }        
    if(class(x[[i]])[1] == "sbf" && x[[i]]$control$returnResamp == "all"){
      warning(paste0("'", modelNames[i], "' did not have 'returnResamp=\"final\"; the optimal subset is used"))
    }
    rs_values[[i]] <- get_resample_perf(x[[i]])         
  }
  all_names <- lapply(rs_values,
                      function(x) names(x)[names(x) != "Resample"])
  all_names <- table(unlist(all_names))
  
  if(length(all_names) == 0 || any(all_names == 0)) {
    warning("Could not find performance measures")
  }
  
  if(any(all_names < length(x))) {
    warning(paste("Some performance measures were not computed for each model:",
                  paste(names(all_names)[all_names < length(x)], collapse = ", ")))
  }
  pNames <- names(all_names)[all_names == length(x)]
  rs_values <- lapply(rs_values, 
                      function(x, n) x[,n,drop = FALSE], 
                      n = c(pNames, "Resample"))
  for(mod in seq(along = modelNames)) {
    names(rs_values[[mod]])[names(rs_values[[mod]]) %in% pNames] <- 
      paste(modelNames[mod], names(rs_values[[mod]])[names(rs_values[[mod]]) %in% pNames], sep = "~")
    out <- if(mod == 1) rs_values[[mod]] else merge(out, rs_values[[mod]])   
  }

  timings <- do.call("rbind", lapply(x, getTimes))
  colnames(timings) <- c("Everything", "FinalModel", "Prediction")
    
  out <- structure(
    list(
      call = match.call(),
      values = out,
      models = modelNames,
      metrics = pNames,
      timings = as.data.frame(timings),
      methods = unlist(lapply(x, function(x) x$method))),
    class = "resamples")

  out
}


prcomp.resamples <- function(x, metric = x$metrics[1],  ...)
{
  
  if(length(metric) != 1) stop("exactly one metric must be given")
  
  tmpData <- x$values[, grep(paste("~", metric, sep = ""),
                             names(x$value),
                             fixed = TRUE),
                      drop = FALSE]
  names(tmpData) <- gsub(paste("~", metric, sep = ""),
                         "",
                         names(tmpData),
                         fixed = TRUE)
  
  tmpData <- as.data.frame(t(tmpData))
  colnames(tmpData) <- paste("Resample", 
                             gsub(" ", "0", format(1:ncol(tmpData))), 
                             sep = "")
  out <- prcomp(~., data = tmpData, ...)
  out$metric <- metric
  out$call <- match.call()
  class(out) <- c("prcomp.resamples", "prcomp")
  out
}


"cluster" <- function(x, ...) UseMethod("cluster")

cluster.default <- function(x, ...) stop("only implemented for resamples objects")

cluster.resamples <- function(x, metric = x$metrics[1],  ...)
{
  
  if(length(metric) != 1) stop("exactly one metric must be given")
  
  tmpData <- x$values[, grep(paste("~", metric, sep = ""),
                             names(x$value),
                             fixed = TRUE),
                      drop = FALSE]
  names(tmpData) <- gsub(paste("~", metric, sep = ""),
                         "",
                         names(tmpData),
                         fixed = TRUE)
  
  tmpData <- t(tmpData)
  dt <- dist(tmpData)
  out <- hclust(dt, ...)
  out$metric <- metric
  out$call <- match.call()
  class(out) <- c("cluster.resamples", "hclust")
  out
}


plot.prcomp.resamples <- function(x, what = "scree", dims = max(2, ncol(x$rotation)), ...)
{
  if(length(what) > 1) stop("one plot at a time please")
  switch(what,
         scree =
{
  barchart(x$sdev ~ paste("PC", 
                          gsub(" ", "0", format(seq(along = x$sdev))), 
                          sep = ""),
           ylab = "Standard Deviation", ...)
},
cumulative =
{
  barchart(cumsum(x$sdev^2)/sum(x$sdev^2) ~ paste("PC", 
                                                  gsub(" ", "0", format(seq(along = x$sdev))), 
                                                  sep = ""),
           ylab = "Culmulative Percent of Variance", ...)
},
loadings =
{
  
  panelRange <- extendrange(x$rotation[, 1:dims,drop = FALSE])
  if(dims > 2)
  {
    
    splom(~x$rotation[, 1:dims,drop = FALSE],
          main = useMathSymbols(x$metric),
          prepanel.limits = function(x) panelRange,
          type = c("p", "g"),
          ...)
  } else {
    xyplot(PC2~PC1, data = as.data.frame(x$rotation),
           main = useMathSymbols(x$metric),
           xlim = panelRange,
           ylim = panelRange,
           type = c("p", "g"),
           ...)
  }
  
},
components =
{
  
  panelRange <- extendrange(x$x[, 1:dims,drop = FALSE])
  if(dims > 2)
  {
    
    splom(~x$x[, 1:dims,drop = FALSE],
          main = useMathSymbols(x$metric),
          prepanel.limits = function(x) panelRange,
          groups = rownames(x$x),
          type = c("p", "g"),
          ...)
  } else {
    xyplot(PC2~PC1, data = as.data.frame(x$x),
           main = useMathSymbols(x$metric),
           xlim = panelRange,
           ylim = panelRange,
           
           groups = rownames(x$x),
           type = c("p", "g"),
           ...)
  }
  
})
} 


print.prcomp.resamples <- function (x, digits = max(3, getOption("digits") - 3), print.x = FALSE, ...) 
{
  printCall(x$call)
  cat("Metric:", x$metric, "\n")
  
  
  sds <- rbind(x$sdev, cumsum(x$sdev^2)/sum(x$sdev^2))
  rownames(sds) <- c("Std. Dev. ", "Cum. Percent Var. ")
  colnames(sds) <- rep("", ncol(sds))
  
  print(sds, digits = digits, ...)
  cat("\nRotation:\n")
  print(x$rotation, digits = digits, ...)
  if (print.x && length(x$x)) {
    cat("\nRotated variables:\n")
    print(x$x, digits = digits, ...)
  }
  invisible(x)
}

print.resamples <- function(x, ...)
{
  printCall(x$call)
  cat("Models:", paste(x$models, collapse = ", "), "\n")
  cat("Number of resamples:", nrow(x$values), "\n")
  cat("Performance metrics:",  paste(x$metrics, collapse = ", "), "\n")
  
  hasTimes <- apply(x$timing, 2, function(a) !all(is.na(a)))
  if(any(hasTimes))
  {
    names(hasTimes) <- gsub("FinalModel", "final model fit", names(hasTimes))
    cat("Time estimates for:",  paste(tolower(names(hasTimes))[hasTimes], collapse = ", "), "\n")
  }
  invisible(x)
}

summary.resamples <- function(object, metric = object$metrics, ...){
  vals <- object$values[, names(object$values) != "Resample", drop = FALSE]
  out <- vector(mode = "list", length = length(metric))
  for(i in seq(along = metric)) {
    tmpData <- vals[, grep(paste("~", metric[i], sep = ""), names(vals), fixed = TRUE), drop = FALSE]
    
    out[[i]] <- do.call("rbind", lapply(tmpData, function(x) summary(x)[1:6]))
    naSum <- matrix(unlist(lapply(tmpData, function(x) sum(is.na(x)))), ncol = 1)
    colnames(naSum) <- "NA's"
    out[[i]] <- cbind(out[[i]], naSum)
    rownames(out[[i]]) <- gsub(paste("~", metric[i], sep = ""),
                               "",
                               rownames(out[[i]]),
                               fixed = TRUE)
  }
  
  names(out) <- metric
  out <- structure(
    list(values = vals,
         call = match.call(),
         statistics = out,
         models = object$models,
         metrics = metric,
         methods = object$methods),
    class = "summary.resamples")
  out
}


print.summary.resamples <- function(x, ...)
{
  printCall(x$call)
  cat("Models:", paste(x$models, collapse = ", "), "\n")
  cat("Number of resamples:", nrow(x$values), "\n")
  
  cat("\n")
  
  for(i in seq(along = x$statistics))
  {
    cat(names(x$statistics)[i], "\n")
    print(x$statistics[[i]])
    cat("\n")
  }
  invisible(x)
}



xyplot.resamples <- function (x, data = NULL, what = "scatter", models = NULL, metric = x$metric[1], units = "min", ...) 
{
  if(!(units %in% c("min", "sec", "hour"))) stop("units should be 'sec', 'min' or 'hour'")
  if(!(what %in% c("scatter", "BlandAltman", "tTime", "mTime", "pTime"))) stop("the what arg should be 'scatter', 'BlandAltman', 'tTime', 'mTime' or 'pTime'")
  
  if(is.null(models)) models <- if(what %in% c("tTime", "mTime", "pTime")) x$models else x$models[1:2]
  if(length(metric) != 1) stop("exactly one metric must be given")
  if(what == "BlandAltman" & length(models) != 2) stop("exactly two model names must be given")
  if(what == "BlandAltman")
  {
    tmpData <- x$values[, paste(models, metric, sep ="~")]
    tmpData$Difference <- tmpData[,1] - tmpData[,2]
    tmpData$Average <- (tmpData[,1] + tmpData[,2])/2
    ylm <- extendrange(c(tmpData$Difference, 0))
    out <- xyplot(Difference ~ Average,
                  data = tmpData,
                  ylab = paste(models, collapse = " - "),
                  ylim = ylm,
                  main = useMathSymbols(metric),
                  panel = function(x, y, ...)
                  {
                    panel.abline(h = 0, col = "darkgrey", lty = 2)
                    panel.xyplot(x, y, ...)
                  })
    
  }
  if(what == "scatter")
  {
    tmpData <- x$values[, paste(models, metric, sep ="~")]
    colnames(tmpData) <- gsub(paste("~", metric, sep = ""), "", colnames(tmpData))
    
    ylm <- extendrange(c(tmpData[,1], tmpData[,2]))
    out <- xyplot(tmpData[,1] ~ tmpData[,2],
                  ylab = colnames(tmpData)[1],
                  xlab = colnames(tmpData)[2],
                  xlim = ylm, ylim = ylm,
                  main = useMathSymbols(metric),
                  panel = function(x, y, ...)
                  {
                    panel.abline(0, 1, col = "darkgrey", lty = 2)
                    panel.xyplot(x, y, ...)
                  })
  }
  if(what %in% c("tTime", "mTime", "pTime"))
  {
    ## the following is based on
    ## file.show(system.file("demo/intervals.R", package = "lattice")
    prepanel.ci <- function(x, y, lx, ux, subscripts, ...)
    {
      x <- as.numeric(x)
      lx <- as.numeric(lx[subscripts])
      ux <- as.numeric(ux[subscripts])
      list(xlim = extendrange(c(x, ux, lx)),
           ylim = extendrange(y))
    }
    
    panel.ci <- function(x, y, lx, ux, groups, subscripts, pch = 16, ...)
    {
      theme <- trellis.par.get()
      x <- as.numeric(x)
      y <- as.numeric(y)
      lx <- as.numeric(lx[subscripts])
      ux <- as.numeric(ux[subscripts])
      gps <- unique(groups)
      for(i in seq(along = gps))
      {
        panel.arrows(lx[groups == gps[i]],
                     y[groups == gps[i]],
                     ux[groups == gps[i]],
                     y[groups == gps[i]],
                     col = theme$superpose.line$col[i],
                     length = .01, unit = "npc",
                     angle = 90, code = 3)
        panel.xyplot(x[groups == gps[i]],
                     y[groups == gps[i]],
                     type = "p",
                     col = theme$superpose.symbol$col[i],
                     cex = theme$superpose.symbol$cex[i],
                     pch = theme$superpose.symbol$pch[i],
                     , ...)              
      }
    }
    tmpData <- apply(x$values[,-1], 2,
                     function(x)
                     {
                       tmp <- t.test(x)
                       c(tmp$conf.int[1], tmp$estimate, tmp$conf.int[2])
                     })
    rownames(tmpData) <- c("lower", "point", "upper")
    
    tmpData <- tmpData[, paste(models, metric, sep ="~")]
    colnames(tmpData) <- gsub(paste("~", metric, sep = ""), "", colnames(tmpData))
    tmpData <- data.frame(t(tmpData))
    tmpData$Model <- rownames(tmpData)
    
    tm <- switch(what,
                 tTime = x$timings[,"Everything"],
                 mTime = x$timings[,"FinalModel"],
                 pTime = x$timings[,"Prediction"])
    lab <- switch(what,
                  tTime = "Total Time (",
                  mTime = "Final Model Time (",
                  pTime = "Prediction Time (")
    lab <- paste(lab, units, ")", sep = "")
    
    times <- data.frame(Time = tm,
                        Model = rownames(x$timings))
    plotData <- merge(times, tmpData)
    
    if(units == "min") plotData$Time <- plotData$Time/60
    if(units == "hour") plotData$Time <- plotData$Time/60/60
    
    out <- with(plotData,
                xyplot(Time ~ point,
                       lx = lower, ux = upper,
                       xlab = useMathSymbols(metric),
                       ylab = lab,
                       prepanel = prepanel.ci,
                       panel = panel.ci, groups = Model,
                       ...))
  }  
  out
}

parallelplot.resamples <- function (x, data = NULL, models = x$models, metric = x$metric[1], ...) 
{
  if(length(metric) != 1) stop("exactly one metric must be given")
  
  tmpData <- x$values[, grep(paste("~", metric, sep = ""),
                             names(x$value),
                             fixed = TRUE),
                      drop = FALSE]
  names(tmpData) <- gsub(paste("~", metric, sep = ""),
                         "",
                         names(tmpData),
                         fixed = TRUE)
  rng <- range(unlist(lapply(lapply(tmpData, as.numeric), range, na.rm = TRUE)))
  prng <- pretty(rng)
  
  reord <- order(apply(tmpData, 2, median, na.rm = TRUE))
  tmpData <- tmpData[, reord]
  
  parallelplot(~tmpData,
               common.scale = TRUE,
               scales = list(x = list(at = (prng-min(rng))/diff(rng), labels = prng)),
               xlab = useMathSymbols(metric),
               ...)
  
}

splom.resamples <- function (x, data = NULL, variables = "models",
                             models = x$models,
                             metric = NULL,
                             panelRange = NULL,
                             ...) 
{
  
  if(variables == "models")
  {
    
    if(is.null(metric)) metric <- x$metric[1]
    if(length(metric) != 1) stop("exactly one metric must be given")
    
    tmpData <- x$values[, grep(paste("~", metric, sep = ""),
                               names(x$value),
                               fixed = TRUE),
                        drop = FALSE]
    names(tmpData) <- gsub(paste("~", metric, sep = ""),
                           "",
                           names(tmpData),
                           fixed = TRUE)
    if(is.null(panelRange)) panelRange <- extendrange(tmpData)
    splom(~tmpData,
          panel = function(x, y)
          {
            panel.splom(x, y, ...)
            panel.abline(0, 1, lty = 2, col = "darkgrey")
            
          },
          main = useMathSymbols(metric),
          prepanel.limits = function(x) panelRange,
          ...)
  } else{
    if(variables == "metrics")
    {
      if(is.null(metric)) metric <- x$metric
      if(length(metric) < 2) stop("There should be at least two metrics")
      plotData <- melt(x$values, id.vars = "Resample")
      tmp <- strsplit(as.character(plotData$variable), "~", fixed = TRUE)
      plotData$Model <- unlist(lapply(tmp, function(x) x[1]))
      plotData$Metric <- unlist(lapply(tmp, function(x) x[2]))
      plotData <- subset(plotData, Model %in% models & Metric  %in% metric)
      means <- dcast(plotData, Model ~ Metric, mean, na.rm = TRUE)
      splom(~means[, metric], groups = means$Model, ...)
      
    } else stop ("'variables' should be either 'models' or 'metrics'")
    
  }
  
}


densityplot.resamples <- function (x, data = NULL, models = x$models, metric = x$metric, ...) 
{
  plotData <- melt(x$values, id.vars = "Resample")
  tmp <- strsplit(as.character(plotData$variable), "~", fixed = TRUE)
  plotData$Model <- unlist(lapply(tmp, function(x) x[1]))
  plotData$Metric <- unlist(lapply(tmp, function(x) x[2]))
  plotData <- subset(plotData, Model %in% models & Metric  %in% metric)
  
  metricVals <- unique(plotData$Metric)
  plotForm <- if(length(metricVals) > 1) as.formula(~value|Metric) else as.formula(~value)
  densityplot(plotForm, data = plotData, groups = Model,
              xlab = if(length(unique(plotData$Metric)) > 1) "" else metricVals, ...)
  
}



bwplot.resamples <- function (x, data = NULL, models = x$models, metric = x$metric, ...) 
{
  plotData <- melt(x$values, id.vars = "Resample")
  tmp <- strsplit(as.character(plotData$variable), "~", fixed = TRUE)
  plotData$Model <- unlist(lapply(tmp, function(x) x[1]))
  plotData$Metric <- unlist(lapply(tmp, function(x) x[2]))
  plotData <- subset(plotData, Model %in% models & Metric  %in% metric)
  avPerf <- ddply(subset(plotData, Metric == metric[1]),
                  .(Model),
                  function(x) c(Median = median(x$value, na.rm = TRUE)))
  avPerf <- avPerf[order(avPerf$Median),]
  plotData$Model <- factor(as.character(plotData$Model),
                           levels = avPerf$Model)
  metricVals <- unique(plotData$Metric)
  plotForm <- if(length(metricVals) > 1) as.formula(Model~value|Metric) else as.formula(Model~value)
  bwplot(plotForm, data = plotData,
         xlab = if(length(unique(plotData$Metric)) > 1) "" else metricVals, ...)
  
}



dotplot.resamples <- function (x, data = NULL, models = x$models, metric = x$metric, conf.level = 0.95, ...) 
{
  plotData <- melt(x$values, id.vars = "Resample")
  tmp <- strsplit(as.character(plotData$variable), "~", fixed = TRUE)
  plotData$Model <- unlist(lapply(tmp, function(x) x[1]))
  plotData$Metric <- unlist(lapply(tmp, function(x) x[2]))
  plotData <- subset(plotData, Model %in% models & Metric  %in% metric)
  plotData$variable <- factor(as.character(plotData$variable))
  
  plotData <- split(plotData, plotData$variable)
  results <- lapply(plotData,
                    function(x, cl)
                    {
                      ttest <- try(t.test(x$value, conf.level = cl),
                                   silent = TRUE)
                      if(class(ttest)[1] == "htest")
                      {
                        out <- c(ttest$conf.int, ttest$estimate)
                        names(out) <- c("LowerLimit", "UpperLimit", "Estimate")
                      } else out <- rep(NA, 3)
                      out
                    },
                    cl = conf.level)
  results <- do.call("rbind", results)
  results <- melt(results)
  results <- results[!is.nan(results$value),]
  tmp <- strsplit(as.character(results$Var1), "~", fixed = TRUE)
  results$Model <- unlist(lapply(tmp, function(x) x[1]))
  results$Metric <- unlist(lapply(tmp, function(x) x[2]))
  ## to avoid "no visible binding for global variable 'Var2'"
  Var2 <- NULL
  avPerf <- ddply(subset(results, Metric == metric[1] & Var2 == "Estimate"),
                  .(Model),
                  function(x) c(Median = median(x$value, na.rm = TRUE)))
  avPerf <- avPerf[order(avPerf$Median),]
  results$Model <- factor(as.character(results$Model),
                          levels = avPerf$Model)
  metricVals <- unique(results$Metric)
  plotForm <- if(length(metricVals) > 1) as.formula(Model~value|Metric) else as.formula(Model~value)
  dotplot(plotForm,
          data = results,
          xlab = if(length(unique(plotData$Metric)) > 1) "" else metricVals,
          panel = function(x, y)
          {
            plotTheme <- trellis.par.get()
            y <- as.numeric(y)
            vals <- aggregate(x, list(group = y), function(x) c(min = min(x), mid = median(x), max = max(x)))
            
            panel.dotplot(vals$x[,"mid"], vals$group,
                          pch = plotTheme$plot.symbol$pch[1],
                          col = plotTheme$plot.symbol$col[1],
                          cex = plotTheme$plot.symbol$cex[1])
            panel.segments(vals$x[,"min"], vals$group, 
                           vals$x[,"max"], vals$group, 
                           lty = plotTheme$plot.line$lty[1],
                           col = plotTheme$plot.line$col[1],
                           lwd = plotTheme$plot.line$lwd[1])
            len <- .03
            panel.segments(vals$x[,"min"], vals$group+len, 
                           vals$x[,"min"], vals$group-len, 
                           lty = plotTheme$plot.line$lty[1],
                           col = plotTheme$plot.line$col[1],
                           lwd = plotTheme$plot.line$lwd[1])
            panel.segments(vals$x[,"max"], vals$group+len, 
                           vals$x[,"max"], vals$group-len, 
                           lty = plotTheme$plot.line$lty[1],
                           col = plotTheme$plot.line$col[1],
                           lwd = plotTheme$plot.line$lwd[1])           
            
          },
          sub = paste("Confidence Level:", conf.level),
          ...)
  
}


diff.resamples <- function(x,
                           models = x$models,
                           metric = x$metrics,
                           test = t.test,
                           confLevel = 0.95,
                           adjustment = "bonferroni",
                           ...)
{
  
  allDif <- vector(mode = "list", length = length(metric))
  names(allDif) <- metric
  
  x$models <- x$models[x$models %in% models]
  p <- length(x$models)
  ncomp <- choose(p, 2)
  if(adjustment == "bonferroni") confLevel <- 1 - ((1 - confLevel)/ncomp)
  allStats <- allDif
  
  for(h in seq(along = metric))
  {
    index <- 0
    dif <- matrix(NA,
                  nrow = nrow(x$values),
                  ncol = choose(length(models), 2))
    stat <- vector(mode = "list", length = choose(length(models), 2))
    
    colnames(dif) <- paste("tmp", 1:ncol(dif), sep = "")
    for(i in seq(along = models))
    {
      for(j in seq(along = models))
      {
        if(i < j)
        {
          index <- index + 1
          
          left <- paste(models[i], metric[h], sep = "~")
          right <- paste(models[j], metric[h], sep = "~")
          dif[,index] <- x$values[,left] - x$values[,right]
          colnames(dif)[index] <- paste(models[i], models[j], sep = ".diff.")
        }
      }
    }
    
    stats <- apply(dif, 2, function(x, tst, ...) tst(x, ...), tst = test, conf.level = confLevel, ...)
    
    allDif[[h]] <- dif
    allStats[[h]] <- stats
  }
  out <- structure(
    list(
      call = match.call(),
      difs = allDif,
      confLevel = confLevel,
      adjustment = adjustment,
      statistics = allStats,
      models = models,
      metric = metric),
    class = "diff.resamples")
  out
}


densityplot.diff.resamples <- function(x, data, metric = x$metric, ...)
{
  plotData <- lapply(x$difs,
                     function(x) stack(as.data.frame(x)))
  plotData <- do.call("rbind", plotData)
  plotData$Metric <- rep(x$metric, each = length(x$difs[[1]]))
  plotData$ind <- gsub(".diff.", " - ", plotData$ind, fixed = TRUE)
  plotData <- subset(plotData, Metric %in% metric)
  metricVals <- unique(plotData$Metric)
  plotForm <- if(length(metricVals) > 1) as.formula(~values|Metric) else as.formula(~values)
  
  densityplot(plotForm, data = plotData, groups = ind,
              xlab = if(length(unique(plotData$Metric)) > 1) "" else metricVals, ...)
  
}


bwplot.diff.resamples <- function(x, data, metric = x$metric, ...)
{
  plotData <- lapply(x$difs,
                     function(x) stack(as.data.frame(x)))
  plotData <-  do.call("rbind", plotData)
  plotData$Metric <- rep(x$metric, each = length(x$difs[[1]]))
  plotData$ind <- gsub(".diff.", " - ", plotData$ind, fixed = TRUE)
  plotData <- subset(plotData, Metric %in% metric)
  metricVals <- unique(plotData$Metric)
  plotForm <- if(length(metricVals) > 1) as.formula(ind ~ values|Metric) else as.formula(ind ~ values)
  
  bwplot(plotForm,
         data = plotData,
         xlab = if(length(unique(plotData$Metric)) > 1) "" else metricVals,
         ...)
  
}

print.diff.resamples <- function(x, ...)
{
  printCall(x$call)
  cat("Models:", paste(x$models, collapse = ", "), "\n")
  cat("Metrics:", paste(x$metric, collapse = ", "), "\n")
  cat("Number of differences:",  ncol(x$difs[[1]]), "\n")
  cat("p-value adjustment:",  x$adjustment, "\n")    
  invisible(x)
}


summary.diff.resamples <- function(object, digits = max(3, getOption("digits") - 3), ...)
{
  all <- vector(mode = "list", length = length(object$metric))
  names(all) <- object$metric
  
  for(h in seq(along = object$metric))
  {
    pvals <- matrix(NA, nrow = length(object$models), ncol = length(object$models))
    meanDiff <- pvals
    index <- 0
    for(i in seq(along = object$models)) {
      for(j in seq(along = object$models)) {
        if(i < j) {
          index <- index + 1
          meanDiff[i, j] <- object$statistics[[h]][index][[1]]$estimate
        }
      }
    }
    
    index <- 0
    for(i in seq(along = object$models)) {
      for(j in seq(along = object$models)) {
        if(i < j) {
          index <- index + 1
          pvals[j, i] <- object$statistics[[h]][index][[1]]$p.value
          
        }
      }
    }
    pvals[lower.tri(pvals)] <- p.adjust(pvals[lower.tri(pvals)], method = object$adjustment)
    asText <- matrix("", nrow = length(object$models), ncol = length( object$models))
    meanDiff2 <- format(meanDiff, digits = digits)
    pvals2 <- matrix(format.pval(pvals, digits = digits), nrow = length( object$models))
    asText[upper.tri(asText)] <- meanDiff2[upper.tri(meanDiff2)]
    asText[lower.tri(asText)] <- pvals2[lower.tri(pvals2)]
    diag(asText) <- ""
    colnames(asText) <- object$models
    rownames(asText) <- object$models
    all[[h]] <- asText
  }  
  
  out <- structure(
    list(
      call = match.call(),
      adjustment = object$adjustment,
      table = all),
    class = "summary.diff.resamples")
  out
}


levelplot.diff.resamples <- function(x, data = NULL, metric = x$metric[1], what = "pvalues", ...)
{
  comps <- ncol(x$difs[[1]])
  if(length(metric) != 1) stop("exactly one metric must be given")
  
  all <- vector(mode = "list", length = length(x$metric))
  names(all) <- x$metric
  
  for(h in seq(along = x$metric))
  {
    temp <- matrix(NA, nrow = length(x$models), ncol = length( x$models))
    index <- 0
    for(i in seq(along = x$models))
    {
      for(j in seq(along = x$models))
      {
        
        if(i < j)
        {
          index <- index + 1
          temp[i, j] <-  if(what == "pvalues")
          {
            x$statistics[[h]][index][[1]]$p.value
          } else x$statistics[[h]][index][[1]]$estimate
          temp[j, i] <- temp[i, j] 
        }
      }
    }
    colnames(temp) <- x$models
    temp  <- as.data.frame(temp)
    temp$A <- x$models
    temp$Metric <- x$metric[h]
    all[[h]] <- temp
  }
  all <- do.call("rbind", all)
  all <- melt(all, measure.vars = x$models)
  names(all)[names(all) == "variable"] <- "B"
  all$A <- factor(all$A, levels = x$models)
  all$B <- factor(as.character(all$B), levels = x$models)
  
  all <- all[complete.cases(all),]
  levelplot(value ~ A + B|Metric,
            data = all,
            subset = Metric %in% metric,
            xlab = "",
            ylab = "",
            sub = ifelse(what == "pvalues", "p-values", "difference = (x axis - y axis)"),
            ...)
}




print.summary.diff.resamples <- function(x, ...)
{
  printCall(x$call)
  
  if(x$adjustment != "none")
    cat("p-value adjustment:", x$adjustment, "\n")
  
  
  cat("Upper diagonal: estimates of the difference\n",
      "Lower diagonal: p-value for H0: difference = 0\n\n",
      sep = "")
  
  for(i in seq(along = x$table))
  {
    cat(names(x$table)[i], "\n")
    print(x$table[[i]], quote = FALSE)
    cat("\n")
  }
  invisible(x)
}


dotplot.diff.resamples <- function(x, data = NULL, metric = x$metric[1], ...)
{
  if(length(metric) > 1)
  {
    metric <- metric[1]
    warning("Sorry Dave, only one value of metric is allowed right now. I'll use the first value")
    
  }
  h <- which(x$metric == metric)
  plotData <- as.data.frame(matrix(NA, ncol = 3, nrow = ncol(x$difs[[metric]])))
  ## Get point and interval estimates on the differences
  index <- 0
  for(i in seq(along = x$models))
  {
    for(j in seq(along = x$models))
    {
      
      if(i < j)
      {
        index <- index + 1
        plotData[index, 1] <- x$statistics[[h]][index][[1]]$estimate
        plotData[index, 2:3] <- x$statistics[[h]][index][[1]]$conf.int
        
      }
    }
  }
  names(plotData)[1:3] <- c("Estimate", "LowerLimit", "UpperLimit")
  plotData$Difference <- gsub(".diff.", " - ", colnames(x$difs[[metric]]), fixed = TRUE)
  plotData <- melt(plotData, id.vars = "Difference")
  xText <- paste("Difference in",
                 useMathSymbols(metric),
                 "\nConfidence Level",
                 round(x$confLevel, 3),
                 ifelse(x$adjustment == "bonferroni",
                        " (multiplicity adjusted)",
                        " (no multiplicity adjustment)"))
  dotplot(Difference ~ value,
          data = plotData,
          xlab = xText,
          panel = function(x, y)
          {
            plotTheme <- trellis.par.get()
            
            
            middle <- aggregate(x, list(mod = y), median)
            upper <- aggregate(x, list(mod = as.numeric(y)), max)
            lower <- aggregate(x, list(mod = as.numeric(y)), min)
            panel.dotplot(middle$x, middle$mod,
                          col = plotTheme$plot.symbol$col[1],
                          pch = plotTheme$plot.symbol$pch[1],
                          cex = plotTheme$plot.symbol$cex[1])
            panel.abline(v = 0,
                         col = plotTheme$reference.line$col[1],
                         lty = plotTheme$reference.line$lty[1],
                         lwd = plotTheme$reference.line$lwd[1])
            for(i in seq(along = upper$mod))
            {
              panel.segments(upper$x[i], upper$mod[i], lower$x[i], lower$mod[i],
                             col = plotTheme$plot.line$col[1],
                             lwd = plotTheme$plot.line$lwd[1],
                             lty = plotTheme$plot.line$lty[1])
              len <- .03
              panel.segments(lower$x[i], upper$mod[i]+len, 
                             lower$x[i], lower$mod[i]-len, 
                             lty = plotTheme$plot.line$lty[1],
                             col = plotTheme$plot.line$col[1],
                             lwd = plotTheme$plot.line$lwd[1])
              panel.segments(upper$x[i],upper$mod[i]+len, 
                             upper$x[i], lower$mod[i]-len, 
                             lty = plotTheme$plot.line$lty[1],
                             col = plotTheme$plot.line$col[1],
                             lwd = plotTheme$plot.line$lwd[1])
            }
          },
          ...)
}


modelCor <- function(x, metric = x$metric[1], ...)
{
  dat <- x$values[, grep(paste("~", metric[1], sep = ""), names(x$values))]
  colnames(dat) <- gsub(paste("~", metric[1], sep = ""), "", colnames(dat))
  cor(dat, ...)
}

sort.resamples <- function(x, decreasing = FALSE, metric = x$metric[1], FUN = mean, ...) 
{
  dat <- x$values[, grep(paste("~", metric[1], sep = ""), names(x$values))]
  colnames(dat) <- gsub(paste("~", metric[1], sep = ""), "", colnames(dat))
  stats <- apply(dat, 2, FUN, ...)
  names(sort(stats, decreasing = decreasing))
}

compare_models <- function(a, b, metric = a$metric[1]) {
  mods <- list(a, b)
  rs <- resamples(mods)
  diffs <- diff(rs, metric = metric[1])
  diffs$statistics[[1]][[1]]
}



