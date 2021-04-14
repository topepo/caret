#' @rdname plot.train
#' @importFrom stats as.formula
#' @export
ggplot.train <- function(data = NULL, mapping = NULL, metric = data$metric[1], plotType = "scatter", output = "layered",
               nameInStrip = FALSE, highlight = FALSE, ..., environment = NULL) {
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
    #       params is a factor, so just using params does not work properly when model metric is not the first column in dat
    #           e.g. oob resampling
    paramValues <- apply(dat[,as.character(params),drop = FALSE],
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
  if(data$control$search == "random") return(random_search_plot(data, metric = metric))

  if(plotType == "scatter") {
    # To highlight bestTune parameters in the plot
    if (highlight) {
      bstRes <- data$results
      for (par in as.character(params))
        bstRes <- bstRes[which(bstRes[, par] == data$bestTune[, par]), ]
      if (nrow(bstRes) > 1)
        stop("problem in extracting model$bestTune row from model$results")
    }

    dnm <- names(dat)
    if(p > 1 && is.numeric(dat[, 3])) dat[, 3] <- factor(format(dat[, 3]))
    if(p > 2 & nameInStrip) {
      strip_vars <- names(dat)[-(1:3)]
      strip_lab <- as.character(subset(data$modelInfo$parameters, parameter %in% strip_vars)$label)
      for(i in seq_along(strip_vars))
        dat[, strip_vars[i]] <- factor(paste(strip_lab[i], dat[, strip_vars[i]], sep = ": "))
    }

    # If a parameter is assigned to a facet panel, it needs to be converted to a factor
    #   otherwise, highlighting the bestTune parameters in a facet creates an extraneous panel
    #   potentially, a bug in ggplot ?
    if (p >= 3)
      for (col in 1:(p-2)) {
        lvls <- as.character(unique(dat[, dnm[col+3]]))
        dat[, dnm[col+3]] <- factor(dat[, dnm[col+3]], levels = lvls)
        if (highlight)
          bstRes[, dnm[col+3]] <- factor(bstRes[, dnm[col+3]], levels = lvls)
      }

    out <- ggplot(dat, aes_string(x = dnm[2], y = dnm[1]))
    out <- out + ylab(resampText)

    # names(dat)[.] changed to dnm[.] to make the code more readable & (marginally) efficient
    out <- out + xlab(paramData$label[paramData$parameter == dnm[2]])
    if (highlight)
      out <- out + geom_point(data = bstRes,
                              aes_string(x = dnm[2], y = dnm[1]),
                              size = 4, shape = 5)

    if(output == "layered") {
      if(p >= 2) {
        leg_name <- paramData$label[paramData$parameter == dnm[3]]
        out <- out + geom_point(aes_string(color = dnm[3], shape = dnm[3]))
        out <- out + geom_line(aes_string(color = dnm[3]))
        out <- out + scale_colour_discrete(name = leg_name) +
          scale_shape_discrete(name = leg_name)
      } else out <- out + geom_point() + geom_line()

      if(p == 3)
        out <- out + facet_wrap(as.formula(paste("~", dnm[4])))
      if(p == 4)
        out <- out + facet_grid(as.formula(paste(names(dat)[4], "~", names(dat)[5])))
      if(p > 4) stop("The function can only handle <= 4 tuning parameters for scatter plots. Use output = 'ggplot' to create your own")
    }
  }

  if(plotType == "level") {
    if(p == 1) stop("Two tuning parameters are required for a level plot")
    dnm <- names(dat)
    if(is.numeric(dat[,2])) dat[,2] <- factor(format(dat[,2]), levels = format(sort(unique(dat[,2]))))
    if(is.numeric(dat[,3])) dat[,3] <- factor(format(dat[,3]), levels = format(sort(unique(dat[,3]))))
    if(p > 2 & nameInStrip) {
      strip_vars <- names(dat)[-(1:3)]
      strip_lab <- as.character(subset(data$modelInfo$parameters, parameter %in% strip_vars)$label)
      for(i in seq_along(strip_vars))
        dat[, strip_vars[i]] <- factor(
          paste(strip_lab[i], format(dat[, strip_vars[i]]), sep = ": "),
          levels = paste(strip_lab[i], format(sort(unique(dat[, strip_vars[i]]))), sep = ": ")
        )
    }
    ## TODO: use factor(format(x)) to make a solid block of colors?
    out <- ggplot(dat, aes_string(x = dnm[2], y = dnm[3], fill = metric))
    out <- out + ylab(paramData$label[paramData$parameter == dnm[3]])
    out <- out + xlab(paramData$label[paramData$parameter == dnm[2]])
    if(output == "layered") {
      out <- out + geom_tile()
      if(p == 3)
        out <- out + facet_wrap(as.formula(paste("~", dnm[4])))

      # incorrect facet_wrap call for p == 4 ? fixed errors for p >= 4
      if(p == 4)
        out <- out + facet_grid(as.formula(paste(dnm[4], "~", dnm[5])))
      if(p > 4) stop("The function can only handle <= 4 tuning parameters for level plots. Use output = 'ggplot' to create your own")

    }
  }
  out
}

#' @rdname plot.rfe
#' @export
ggplot.rfe <- function(data = NULL, mapping = NULL, metric = data$metric[1],
                       output = "layered", ..., environment = NULL) {
  if(!(output %in% c("data", "layered", "ggplot")))
    stop("'outout' should be either 'data', 'ggplot' or 'layered'")
  resampText <- resampName(data, FALSE)
  resampText <- paste(metric, resampText)
  if(output == "data") return(data$results)

    if(any(names(data$results) == "Num_Resamples")) {
      data$results <- 
        subset(data$results, Num_Resamples >= floor(.5 * length(data$control$index)))
  }
  
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

#' @importFrom stats complete.cases
random_search_plot <- function(x, metric = x$metric[1]) {

  params <- x$modelInfo$parameters
  p_names <- as.character(params$parameter)

  exclude <- NULL
  for(i in seq(along = p_names)) {
    if(all(is.na(x$results[, p_names[i]])))
      exclude <- c(exclude, i)
  }
  if(length(exclude) > 0) p_names <- p_names[-exclude]
  x$results <- x$results[, c(metric, p_names)]
  res <- x$results[complete.cases(x$results),]
  combos <- res[, p_names, drop = FALSE]

  nvals <- unlist(lapply(combos, function(x) length(unique(x))))
  p_names <- p_names[which(nvals > 1)]

  if(nrow(combos) == 1 | length(p_names) == 0)
    stop("Can't plot results with a single tuning parameter combination")
  combos <- combos[, p_names, drop = FALSE]
  nvals <- sort(nvals[p_names], decreasing = TRUE)

  is_num <- unlist(lapply(combos, function(x) is.numeric(x) | is.integer(x)))
  num_cols <- names(is_num)[is_num]
  other_cols <- names(is_num)[!is_num]

  num_num <- sum(is_num)
  num_other <- length(p_names) - num_num
  if(num_other == 0) {
    if(num_num == 1) {
      out <- ggplot(res, aes_string(x = num_cols[1], y = metric)) +
        geom_point() + xlab(as.character(params$label[params$parameter == num_cols[1]]))
    } else {
      if(num_num == 2) {
        out <- ggplot(res, aes_string(x = num_cols[1], y = num_cols[2], size = metric)) +
          geom_point() +
          xlab(as.character(params$label[params$parameter == num_cols[1]])) +
          ylab(as.character(params$label[params$parameter == num_cols[2]]))
      } else {
        ## feature plot
        vert <- melt(res[, c(metric, num_cols)], id.vars = metric, variable.name = "parameter")
        vert <- merge(vert, params)
        names(vert)[names(vert) == "label"] <- "Parameter"
        out <- ggplot(vert, aes_string(x = "value", y = metric)) +
          geom_point() + facet_wrap(~Parameter, scales = "free_x") + xlab("")
      }
    }
  } else {
    if(num_other == length(p_names)) {
      ## do an interaction plot
      if(num_other == 1) {
        out <- ggplot(res, aes_string(x = other_cols[1], y = metric)) +
          geom_point() +
          xlab(as.character(params$label[params$parameter == other_cols[1]]))
      } else {
        if(num_other == 2) {
          out <- ggplot(res, aes_string(x = other_cols[1], shape = other_cols[2],  y = metric)) +
            geom_point() + geom_line(aes_string(group = other_cols[2])) +
            xlab(as.character(params$label[params$parameter == other_cols[1]]))
        } else {
          if(num_other == 3) {
            pname <- as.character(params$label[params$parameter == other_cols[3]])
            res[,other_cols[3]] <- paste0(pname, ": ", res[,other_cols[3]])
            out <- ggplot(res, aes_string(x = other_cols[1], shape = other_cols[2],  y = metric)) +
              geom_point() + geom_line(aes_string(group = other_cols[2])) +
              facet_grid(paste0(".~", other_cols[3])) +
              xlab(as.character(params$label[params$parameter == other_cols[1]]))
          } else {
            stop(paste("There are",
                       num_other, "non-numeric variables; I don't have code for",
                       "that Dave"))
          }
        }
      }
    } else {
      ## feature plot with colors and or shapes
      vert <- melt(res[, c(metric, num_cols, other_cols)],
                   id.vars = c(metric, other_cols),
                   variable.name = "parameter")
      vert <- merge(vert, params)
      names(vert)[names(vert) == "label"] <- "Parameter"
      if(num_other == 1) {
        out <- ggplot(vert, aes_string(x = "value", y = metric, color = other_cols)) +
          geom_point() + facet_wrap(~Parameter, scales = "free_x") + xlab("")
      } else {
        stop(paste("There are", num_num, "numeric tuning variables and",
                   num_other, "non-numeric variables; I don't have code for",
                   "that Dave"))
      }
    }
  }
  out

}
