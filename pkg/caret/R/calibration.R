#' Probability Calibration Plot
#'
#' @name calibration
#' @aliases calibration calibration.formula calibration.default xyplot.calibration ggplot.calibration
#' panel.calibration
#'
#' @description For classification models, this function creates a 'calibration plot' that describes
#' how consistent model probabilities are with observed event rates.
#'
#' @param x a \code{lattice} formula (see \code{\link[lattice:xyplot]{xyplot}} for syntax) where the left
#' -hand side of the formula is a factor class variable of the observed outcome and the right-hand side
#' specifies one or model columns corresponding to a numeric ranking variable for a model (e.g. class
#' probabilities). The classification variable should have two levels.
#'
#' @param data For \code{calibration.formula}, a data frame (or more precisely, anything that is a valid
#' \code{envir} argument in \code{eval}, e.g., a list or an environment) containing values for any
#' variables in the formula, as well as \code{groups} and \code{subset} if applicable. If not found in
#' \code{data}, or if \code{data} is unspecified, the variables are looked for in the environment of the
#' formula. This argument is not used for \code{xyplot.calibration}. For {ggplot.calibration}, \code{data}
#' should be an object of class "\code{calibration}"."
#'
#' @param class a character string for the class of interest
#'
#' @param cuts If a single number this indicates the number of splits of the data are used to create the
#' plot. By default, it uses as many cuts as there are rows in \code{data}. If a vector, these are the
#' actual cuts that will be used.
#'
#' @param subset An expression that evaluates to a logical or integer indexing vector. It is evaluated in
#' \code{data}. Only the resulting rows of \code{data} are used for the plot.
#'
#' @param lattice.options A list that could be supplied to \code{\link[lattice:lattice.options]{lattice.options}}
#'
#' @param bwidth,dwidth a numeric value for the confidence interval bar width and dodge width, respectively.
#' In the latter case, a dodge is only used when multiple models are specified in the formula.
#' @param \dots options to pass through to \code{\link[lattice:xyplot]{xyplot}} or the panel function (not
#' used in \code{calibration.formula}).
#'
#' @details
#' \code{calibration.formula} is used to process the data and \code{xyplot.calibration} is used to create the plot.
#'
#' To construct the calibration plot, the following steps are used for each model:
#'
#' \enumerate{
#'    \item The data are split into \code{cuts - 1} roughly equal groups by their class probabilities
#'    \item the number of samples with true results equal to \code{class} are determined
#'    \item the event rate is determined for each bin}
#' \code{xyplot.calibration} produces a plot of the observed event rate by the mid-point of the bins.
#'
#' This implementation uses the \pkg{lattice} function \code{\link[lattice:xyplot]{xyplot}}, so plot
#' elements can be changed via panel functions, \code{\link[lattice:trellis.par.get]{trellis.par.set}} or
#' other means. \code{calibration} uses the panel function \code{\link{panel.calibration}} by default, but
#' it can be changed by passing that argument into \code{xyplot.calibration}.
#'
#' The following elements are set by default in the plot but can be changed by passing new values into
#' \code{xyplot.calibration}: \code{xlab = "Bin Midpoint"}, \code{ylab = "Observed Event Percentage"},
#' \code{type = "o"}, \code{ylim = extendrange(c(0, 100))},\code{xlim = extendrange(c(0, 100))} and
#' \code{panel = panel.calibration}
#'
#' For the \code{ggplot} method, confidence intervals on the estimated proportions (from
#' \code{\link[stats]{binom.test}}) are also shown.
#'
#' @return
#' \code{calibration.formula} returns a list with elements:
#' \item{data}{the data used for plotting}
#' \item{cuts}{the number of cuts}
#' \item{class}{the event class}
#' \item{probNames}{the names of the model probabilities}
#'
#' \code{xyplot.calibration} returns a \pkg{lattice} object
#'
#' @author Max Kuhn, some \pkg{lattice} code and documentation by Deepayan Sarkar
#'
#' @seealso \code{\link[lattice:xyplot]{xyplot}}, \code{\link[lattice:trellis.par.get]{trellis.par.set}}
#'
#' @examples
#' \dontrun{
#' data(mdrr)
#' mdrrDescr <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
#' mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .5)]
#'
#'
#' inTrain <- createDataPartition(mdrrClass)
#' trainX <- mdrrDescr[inTrain[[1]], ]
#' trainY <- mdrrClass[inTrain[[1]]]
#' testX <- mdrrDescr[-inTrain[[1]], ]
#' testY <- mdrrClass[-inTrain[[1]]]
#'
#' library(MASS)
#'
#' ldaFit <- lda(trainX, trainY)
#' qdaFit <- qda(trainX, trainY)
#'
#' testProbs <- data.frame(obs = testY,
#'                         lda = predict(ldaFit, testX)$posterior[,1],
#'                         qda = predict(qdaFit, testX)$posterior[,1])
#'
#' calibration(obs ~ lda + qda, data = testProbs)
#'
#' calPlotData <- calibration(obs ~ lda + qda, data = testProbs)
#' calPlotData
#'
#' xyplot(calPlotData, auto.key = list(columns = 2))
#' }
#'
#' @keywords hplot
#'
#' @export
calibration <- function(x, ...) UseMethod("calibration")

#' @rdname calibration
#' @method calibration default
#' @export
calibration.default <- function(x, ...) stop("'x' should be a formula")

#' @rdname calibration
#' @method calibration formula
#' @export
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

  calibData <- data.frame(calibClassVar = form$left,
                          calibProbVar = form$right)
  calibData$calibModelVar <- if(length(probNames) > 1) form$condition[[length(form$condition)]] else probNames

  if(length(form$condition) > 0 && any(names(form$condition) != ""))
  {
    ind <- sum(names(form$condition) != "")
    tmp <- as.data.frame(form$condition[1:ind], stringsAsFactors = TRUE)
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

#' @rdname calibration
#' @method print calibration
#' @export
print.calibration <- function(x, ...)
{
  printCall(x$call)
  cat("Models:", paste(unique(x$data$calibModelVar), collapse = ", "), "\n")
  cat("Event: ", x$class, "\n")
  cat("Cuts:", x$cuts, "\n")
  invisible(x)
}

#' @importFrom stats binom.test
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
                      function(x, cls) {
                        if(nrow(x) > 0) {
                          tmp <- binom.test(x = sum(x$class == cls), n = nrow(x))
                          out <- c(Percent = mean(x$class == cls)*100,
                                   Lower  = tmp$conf.int[1]*100,
                                   Upper  = tmp$conf.int[2]*100,
                                   Count = sum(x$class == cls))
                        } else out <- c(Percent = NA, Lower  = NA,
                                        Upper  = NA, Count = 0)
                        out
                      },
                      cls = class,
                      .drop = FALSE)
  dataPoints$midpoint <- NA
  for(i in 2:length(cuts))
    dataPoints$midpoint[i-1] <- .5*(cuts[i] + cuts[i-1]) * 100
  dataPoints$Percent <- ifelse(dataPoints$Count == 0, 0, dataPoints$Percent)
  dataPoints
}

#' @method plot calibration
#' @export
plot.calibration <- function(x, y = NULL, ...)
  xyplot(x = x, data = NULL, ...)


#' @rdname calibration
#' @method xyplot calibration
#' @importFrom stats as.formula
#' @importFrom grDevices extendrange
#' @export
xyplot.calibration <- function(x, data = NULL, ...){
  lFormula <- "Percent ~ midpoint"
  defaults <- c("calibModelVar", "bin", "Percent", "Count", "Lower", "Upper", "midpoint")
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

#' @rdname calibration
#' @method ggplot calibration
#' @export
ggplot.calibration <- function(data, ..., bwidth = 2, dwidth = 3){
  data$data$Model <- data$data$calibModelVar
  mods <- length(unique(data$data$Model))
  if(mods == 1) {
    out <- ggplot(data$data, aes(x = midpoint, y = Percent)) +
      geom_abline(slope = 1, intercept = 0, col = "black", lty = 2, alpha = .3) +
      geom_point() +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = bwidth)
  } else {
    out <- ggplot(data$data, aes(x = midpoint, y = Percent,
                                 group = Model, color = Model)) +
      geom_abline(slope = 1, intercept = 0, col = "black", lty = 2, alpha = .3) +
      geom_point(position = position_dodge(width = dwidth)) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = bwidth,
                    position = position_dodge(width = dwidth))
  }
  out + xlab("Bin Midpoint") + ylab("Observed Event Percentage")
}

#' @export
panel.calibration <- function(...)
{
  panel.abline(0, 1,
               col = trellis.par.get("reference.line")$col,
               lwd = trellis.par.get("reference.line")$lwd,
               lty = trellis.par.get("reference.line")$lty)
  panel.xyplot(...)
}
