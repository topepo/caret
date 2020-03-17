#' Lift Plot
#'
#' For classification models, this function creates a 'lift plot' that
#' describes how well a model ranks samples for one class
#'
#' \code{lift.formula} is used to process the data and \code{xyplot.lift} is
#' used to create the plot.
#'
#' To construct data for the the lift and gain plots, the following steps are
#' used for each model:
#'
#' \enumerate{ \item The data are ordered by the numeric model prediction used
#' on the right-hand side of the model formula \item Each unique value of the
#' score is treated as a cut point \item The number of samples with true
#' results equal to \code{class} are determined \item The lift is calculated as
#' the ratio of the percentage of samples in each split corresponding to
#' \code{class} over the same percentage in the entire data set} \code{lift}
#' with \code{plot = "gain"} produces a plot of the cumulative lift values by
#' the percentage of samples evaluated while \code{plot = "lift"} shows the cut
#' point value versus the lift statistic.
#'
#' This implementation uses the \pkg{lattice} function
#' \code{\link[lattice:xyplot]{xyplot}}, so plot elements can be changed via
#' panel functions, \code{\link[lattice:trellis.par.get]{trellis.par.set}} or
#' other means. \code{lift} uses the panel function \code{\link{panel.lift2}}
#' by default, but it can be changes using
#' \code{\link[lattice:update.trellis]{update.trellis}} (see the examples in
#' \code{\link{panel.lift2}}).
#'
#' The following elements are set by default in the plot but can be changed by
#' passing new values into \code{xyplot.lift}: \code{xlab = "\% Samples
#' Tested"}, \code{ylab = "\% Samples Found"}, \code{type = "S"}, \code{ylim =
#' extendrange(c(0, 100))} and \code{xlim = extendrange(c(0, 100))}.
#'
#' @aliases lift lift.formula lift.default xyplot.lift
#' @param x a \code{lattice} formula (see \code{\link[lattice:xyplot]{xyplot}}
#' for syntax) where the left-hand side of the formula is a factor class
#' variable of the observed outcome and the right-hand side specifies one or
#' model columns corresponding to a numeric ranking variable for a model (e.g.
#' class probabilities). The classification variable should have two levels.
#' @param data For \code{lift.formula}, a data frame (or more precisely,
#' anything that is a valid \code{envir} argument in \code{eval}, e.g., a list
#' or an environment) containing values for any variables in the formula, as
#' well as \code{groups} and \code{subset} if applicable. If not found in
#' \code{data}, or if \code{data} is unspecified, the variables are looked for
#' in the environment of the formula. This argument is not used for
#' \code{xyplot.lift} or \code{ggplot.lift}.
#' @param class a character string for the class of interest
#' @param subset An expression that evaluates to a logical or integer indexing
#' vector. It is evaluated in \code{data}. Only the resulting rows of
#' \code{data} are used for the plot.
#' @param lattice.options A list that could be supplied to
#' \code{\link[lattice:lattice.options]{lattice.options}}
#' @param cuts If a single value is given, a sequence of values between 0 and 1
#' are created with length \code{cuts}. If a vector, these values are used as
#' the cuts. If \code{NULL}, each unique value of the model prediction is used.
#' This is helpful when the data set is large.
#' @param labels A named list of labels for keys. The list should have an
#' element for each term on the right-hand side of the formula and the names
#' should match the names of the models.
#' @param plot Either "gain" (the default) or "lift". The former plots the
#' number of samples called events versus the event rate while the latter shows
#' the event cut-off versus the lift statistic.
#' @param values A vector of numbers between 0 and 100 specifying reference
#' values for the percentage of samples found (i.e. the y-axis). Corresponding
#' points on the x-axis are found via interpolation and line segments are shown
#' to indicate how many samples must be tested before these percentages are
#' found. The lines use either the \code{plot.line} or \code{superpose.line}
#' component of the current lattice theme to draw the lines (depending on
#' whether groups were used. These values are only used when \code{type =
#' "gain"}.
#' @param \dots options to pass through to \code{\link[lattice:xyplot]{xyplot}}
#' or the panel function (not used in \code{lift.formula}).
#' @return \code{lift.formula} returns a list with elements: \item{data}{the
#' data used for plotting} \item{cuts}{the number of cuts} \item{class}{the
#' event class} \item{probNames}{the names of the model probabilities}
#' \item{pct}{the baseline event rate}
#'
#' \code{xyplot.lift} returns a \pkg{lattice} object
#' @author Max Kuhn, some \pkg{lattice} code and documentation by Deepayan
#' Sarkar
#' @seealso \code{\link[lattice:xyplot]{xyplot}},
#' \code{\link[lattice:trellis.par.get]{trellis.par.set}}
#' @keywords hplot
#' @examples
#'
#' set.seed(1)
#' simulated <- data.frame(obs = factor(rep(letters[1:2], each = 100)),
#'                         perfect = sort(runif(200), decreasing = TRUE),
#'                         random = runif(200))
#'
#' lift1 <- lift(obs ~ random, data = simulated)
#' lift1
#' xyplot(lift1)
#'
#' lift2 <- lift(obs ~ random + perfect, data = simulated)
#' lift2
#' xyplot(lift2, auto.key = list(columns = 2))
#'
#' xyplot(lift2, auto.key = list(columns = 2), value = c(10, 30))
#'
#' xyplot(lift2, plot = "lift", auto.key = list(columns = 2))
#'
#' @export lift
lift <- function(x, ...) UseMethod("lift")

#' @rdname lift
#' @method lift default
#' @export
lift.default <- function(x, ...) stop("'x' should be a formula")

#' @rdname lift
#' @method lift formula
#' @export
lift.formula <- function(x, data = NULL,
                         class = NULL,
                         subset = TRUE,
                         lattice.options = NULL,
                         cuts = NULL,
                         labels = NULL, ...)
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
  liftData <- data.frame(prob = form$y)
  probNames <- strsplit(form$right.name, " + ", fixed = TRUE)[[1]]

  if(!is.null(labels)) {
    if(length(labels) != length(probNames))
      stop("labels should have an element for each term on the rhs of the formula")
    if(!all(probNames %in% names(labels)))
      stop(paste("labels should be a named vector or list with names:",
                 paste(probNames, collapse = ", ")))
  }

  liftData <- data.frame(liftClassVar = rep(form$left, length(probNames)),
                         liftProbVar = form$right)
  liftData$liftModelVar <- if(length(probNames) > 1) form$condition[[length(form$condition)]] else probNames

  if(length(form$condition) > 0 && any(names(form$condition) != "")) {
    ind <- sum(names(form$condition) != "")
    tmp <- as.data.frame(form$condition[1:ind], stringsAsFactors = TRUE)
    liftData <- cbind(liftData, tmp)
  }
  if(!is.factor(liftData$liftClassVar))
    stop("the left-hand side of the formula must be a factor of classes")

  splitVars <- names(liftData)[!(names(liftData) %in% c("liftClassVar", "liftProbVar"))]

  if(is.null(class)) class <- levels(liftData$liftClassVar)[1]
  plotData <- ddply(liftData, splitVars, liftCalc, class = class, cuts = cuts)
  if(!is.null(labels)) {
    plotData$originalName <- plotData$liftModelVar
    plotData$liftModelVar <- as.character(plotData$liftModelVar)
    for(i in seq(along = labels)) plotData$liftModelVar[plotData$liftModelVar == names(labels)[i]] <- labels[i]
    plotData$liftModelVar <- factor(plotData$liftModelVar,
                                    levels = labels)
  }
  out <- list(data = plotData, class = class, probNames = probNames,
              pct =  mean(liftData$liftClassVar == class)*100, call = match.call())
  class(out) <- "lift"
  out
}

#' @rdname lift
#' @method print lift
#' @export
print.lift <- function(x, ...) {
  printCall(x$call)
  cat("Models:", paste(unique(x$data$liftModelVar), collapse = ", "), "\n")
  cat("Event: ", x$class, " (", round( x$pct, 1), "%)\n", sep = "")
  invisible(x)
}

#' @method plot lift
#' @export
plot.lift <- function(x, y = NULL, ...) xyplot.lift(x = x, data = NULL, ...)

#' @rdname lift
#' @method xyplot lift
#' @importFrom stats as.formula
#' @importFrom grDevices extendrange
#' @export
xyplot.lift <- function(x, data = NULL, plot = "gain", values = NULL, ...){
  if(!(plot %in% c("lift", "gain")))
    stop("`plot`` should be either 'lift' or 'gain'", call. = FALSE)
  if(plot == "gain") {
    lFormula <- "CumEventPct ~ CumTestedPct"
    rng <- extendrange(c(0, 100))
    opts <- list(...)
    if(!any(names(opts) == "xlab")) opts$xlab <- "% Samples Tested"
    if(!any(names(opts) == "ylab")) opts$ylab <- "% Samples Found"
    if(!any(names(opts) == "type")) opts$type <- "l"
    if(!any(names(opts) == "ylim")) opts$ylim <- rng
    if(!any(names(opts) == "xlim")) opts$xlim <- rng
    if(!any(names(opts) == "panel")) opts$panel <- panel.lift2
  } else {
    lFormula <- "lift ~ cuts"
    x$data <- x$data[order(x$data$liftModelVar, x$data$cuts),]
    rng <- extendrange(c(0, 100))
    opts <- list(...)
    if(!any(names(opts) == "xlab")) opts$xlab <- "Cut-Off"
    if(!any(names(opts) == "ylab")) opts$ylab <- "Lift"
    if(!any(names(opts) == "type")) opts$type <- "l"
  }
  args <- list(x = as.formula(lFormula),
               data = x$data,
               pct = x$pc,
               values = values)
  if(length(x$probNames) > 1) args$groups <- x$data$liftModelVar

  args <- c(args, opts)
  do.call("xyplot", args)
}

#' @importFrom stats complete.cases
liftCalc <- function(x, class = levels(x$liftClassVar)[1], cuts = NULL) {
  x <- x[complete.cases(x),]
  lvl <- levels(x$liftClassVar)
  x <- x[order(x$liftProbVar, decreasing = TRUE),]

  nEvents <- sum(x$liftClassVar == class)
  baseline <- mean(x$liftClassVar == class)
  if(!is.null(cuts)) {
    if(length(cuts) == 1) {
      cuts <- rev(seq(0, 1, length = cuts))
    } else {
      cuts <- unique(c(1, sort(cuts, decreasing = TRUE), 0))
    }
  } else {
    cuts <- sort(unique(x$liftProbVar), decreasing = TRUE)
    cuts <- unique(c(1, sort(cuts, decreasing = TRUE), 0))
  }

  class2 <- levels(x$liftClassVar)
  class2 <- class2[class2 != class]
  tmp <- data.frame(cuts = cuts,
                    events = NA,
                    n = NA,
                    Sn = NA,
                    Sp = NA)
  for(i in seq(along = cuts)) {
    sub <- x$liftClassVar[x$liftProbVar >= tmp$cuts[i]]
    tmp$n[i] <- length(sub)
    tmp$events[i] <- sum(sub == class)
    prd <- factor(ifelse(x$liftProbVar >= tmp$cuts[i], class, class2),
                  levels = levels(x$liftClassVar))
    tmp$Sn[i] <- sensitivity(prd,
                             x$liftClassVar,
                             positive = class)
    tmp$Sp[i] <- specificity(prd,
                             x$liftClassVar,
                             negative = class2)
  }

  tmp$EventPct <- ifelse(tmp$n > 0, tmp$events/tmp$n*100, 0)
  tmp$CumEventPct <- tmp$events/nEvents*100
  tmp$lift <- tmp$events/tmp$n/baseline
  tmp$CumTestedPct <- tmp$n/nrow(x)*100
  tmp
}

#' @export
panel.lift <- function(x,  y, ...) {
  panel.xyplot(x, y, ...)
  panel.abline(0, 1, col = "black")
}



#' Lattice Panel Functions for Lift Plots
#'
#' Two panel functions that be used in conjunction with \code{\link{lift}}.
#'
#' \code{panel.lift} plots the data with a simple (black) 45 degree reference
#' line.
#'
#' \code{panel.lift2} is the default for \code{\link{lift}} and plots the data
#' points with a shaded region encompassing the space between to the random
#' model and perfect model trajectories. The color of the region is determined
#' by the lattice \code{reference.line} information (see example below).
#'
#' @aliases panel.lift panel.lift2
#' @param x the percentage of searched to be plotted in the scatterplot
#' @param y the percentage of events found to be plotted in the scatterplot
#' @param pct the baseline percentage of true events in the data
#' @param values A vector of numbers between 0 and 100 specifying reference
#' values for the percentage of samples found (i.e. the y-axis). Corresponding
#' points on the x-axis are found via interpolation and line segments are shown
#' to indicate how many samples must be tested before these percentages are
#' found. The lines use either the \code{plot.line} or \code{superpose.line}
#' component of the current lattice theme to draw the lines (depending on
#' whether groups were used
#' @param \dots options to pass to
#' \code{\link[lattice:panel.xyplot]{panel.xyplot}}
#' @author Max Kuhn
#' @seealso \code{\link{lift}},
#' \code{\link[lattice:panel.xyplot]{panel.xyplot}},
#' \code{\link[lattice:xyplot]{xyplot}},
#' \link[lattice:trellis.par.get]{trellis.par.set}
#' @keywords hplot
#' @examples
#'
#' set.seed(1)
#' simulated <- data.frame(obs = factor(rep(letters[1:2], each = 100)),
#'                         perfect = sort(runif(200), decreasing = TRUE),
#'                         random = runif(200))
#'
#' regionInfo <- trellis.par.get("reference.line")
#' regionInfo$col <- "lightblue"
#' trellis.par.set("reference.line", regionInfo)
#'
#' lift2 <- lift(obs ~ random + perfect, data = simulated)
#' lift2
#' xyplot(lift2, auto.key = list(columns = 2))
#'
#' ## use a different panel function
#' xyplot(lift2, panel = panel.lift)
#'
#' @export panel.lift2
panel.lift2 <- function (x, y, pct = 0, values = NULL, ...)  {
  polyx <- c(0, pct, 100, 0)
  polyy <- c(0, 100, 100, 0)
  regionStyle <- trellis.par.get("reference.line")

  panel.polygon(polyx, polyy,
                col = regionStyle$col,
                border = regionStyle$col)
  panel.xyplot(x, y, ...)
  if(!is.null(values)){
    theDots <- list(...)
    if(any(names(theDots) == "groups")) {
      dat <- data.frame(x = x, y = y, groups = theDots$groups)
      ung <- unique(dat$groups)
      for(i in seq(along = ung))  {
        dat0 <- subset(dat, groups == ung[i])
        plotRef(dat0$x, dat0$y, values, iter = i)
      }

    } else plotRef(x, y, values)

  }
}

#' @importFrom stats approx
plotRef <- function(x, y, v, iter = 0) {
  if(iter == 0) {
    lineStyle <- trellis.par.get("plot.line")
  } else {
    lineStyle <- trellis.par.get("superpose.line")
    lineStyle <- lapply(lineStyle, function(x, i) x[min(length(x), i)], i = iter)
  }
  tmp_dat <- data.frame(CumTestedPct = x,
                        CumEventPct = y)
  ref_values <- get_ref_point(tmp_dat, v = v)
  ref_values <- ref_values[!is.na(ref_values$CumTestedPct), ]
  if(nrow(ref_values) > 0) {
    for(i in 1:nrow(ref_values)) {
      panel.segments(x0 = ref_values$CumTestedPct[i],
                     x1 = ref_values$CumTestedPct[i],
                     y0 = 0,
                     y1 = ref_values$CumEventPct[i],
                     lty = lineStyle$lty, col = lineStyle$col,
                     alpha = lineStyle$alpha, lwd = lineStyle$lwd)
      panel.segments(x0 = 0,
                     x1 = ref_values$CumTestedPct[i],
                     y0 = ref_values$CumEventPct[i],
                     y1 = ref_values$CumEventPct[i],
                     lty = lineStyle$lty, col = lineStyle$col,
                     alpha = lineStyle$alpha, lwd = lineStyle$lwd)
    }
  }
}



utils::globalVariables(c("CumEventPct", "CumTestedPct",
                         "cuts", "x1", "x2", "y1", "y2"))
#' @rdname lift
#' @param mapping,environment  Not used (required for \code{ggplot} consistency).
#' @method ggplot lift
#' @export
ggplot.lift <- function (data = NULL, mapping = NULL, plot = "gain", values = NULL, ...,
                 environment = NULL) {
  if(!(plot %in% c("lift", "gain")))
    stop("`plot`` should be either 'lift' or 'gain'", call. = FALSE)
  names(data$data)[names(data$data) == "liftModelVar"] <- "Model"
  nmod <- length(unique(data$data$Model))
  if(plot == "gain") {
    lines1 <- data.frame(x1 = 0, x2 = 100, y1 = 0, y2 = 100)
    lines2 <- data.frame(x1 = 0, x2 = data$pct, y1 = 0, y2 = 100)
    lines3 <- data.frame(x1 = data$pct, x2 = 100, y1 = 100, y2 = 100)
    rng <- extendrange(c(0, 100))
    res <- ggplot(data$data, aes(x = CumTestedPct, y = CumEventPct)) +
      geom_segment(data = lines1,
                   aes(x = x1, y = y1, xend = x2, yend = y2),
                   alpha = .2, lty = 2) +
      geom_segment(data = lines2,
                   aes(x = x1, y = y1, xend = x2, yend = y2),
                   alpha = .2, lty = 2) +
      geom_segment(data = lines3,
                   aes(x = x1, y = y1, xend = x2, yend = y2),
                   alpha = .2, lty = 2) +
      xlab("% Samples Tested") + ylab("% Samples Found") +
      xlim(rng) + ylim(rng)
    res <- if(nmod == 1)
      res + geom_line()
    else
      res + geom_line(aes(col = Model))
    if(!is.null(values)) {
      ref_values <- ddply(data$data, .(Model), get_ref_point, v = values)
      ref_values <- ref_values[!is.na(ref_values$CumTestedPct),]
      if(nrow(ref_values) > 0) {
        if(nmod > 1) {
          res <- res +
            geom_segment(data = ref_values,
                         aes(x = CumTestedPct, y = CumEventPct,
                             xend = CumTestedPct, yend = 0,
                             color = Model))+
            geom_segment(data = ref_values,
                         aes(x = CumTestedPct, y = CumEventPct,
                             xend = 0, yend = CumEventPct,
                             color = Model))
        } else {
          res <- res +
            geom_segment(data = ref_values,
                         aes(x = CumTestedPct, y = CumEventPct,
                             xend = CumTestedPct, yend = 0)) +
            geom_segment(data = ref_values,
                         aes(x = CumTestedPct, y = CumEventPct,
                             xend = 0, yend = CumEventPct))
        }
      }
    }
  } else {
    data$data <- data$data[!is.na(data$data$lift),]
    res <- ggplot(data$data, aes(x = cuts, y = lift)) +
      xlab("Cut-Off") + ylab("Lift")
    res <- if(nmod == 1)
      res + geom_line()
    else
      res + geom_line(aes(col = Model))
  }
  res
}


get_ref_point <- function(dat, v, window = 5) {
  x <- dat$CumTestedPct
  y <- dat$CumEventPct
  erx <- extendrange(x)
  ery <- extendrange(y)

  res <- data.frame(CumEventPct = v,
                    CumTestedPct = NA)

  for(i in seq(along = v)) {
    nearest <- which.min((y - v[i])^2)
    index <- max(1, nearest - window):min(length(y), nearest + window)
    res$CumTestedPct[i] <-
      if (length(index) > 2)
        approx(y[index], x[index], xout = v[i])$y
    else
      NA
  }
  res
}

