#' Plot Method for the train Class
#'
#' This function takes the output of a \code{\link{train}} object and creates a
#' line or level plot using the \pkg{lattice} or \pkg{ggplot2} libraries.
#'
#' If there are no tuning parameters, or none were varied, an error is
#' produced.
#'
#' If the model has one tuning parameter with multiple candidate values, a plot
#' is produced showing the profile of the results over the parameter. Also, a
#' plot can be produced if there are multiple tuning parameters but only one is
#' varied.
#'
#' If there are two tuning parameters with different values, a plot can be
#' produced where a different line is shown for each value of of the other
#' parameter. For three parameters, the same line plot is created within
#' conditioning panels/facets of the other parameter.
#'
#' Also, with two tuning parameters (with different values), a levelplot (i.e.
#' un-clustered heatmap) can be created. For more than two parameters, this
#' plot is created inside conditioning panels/facets.
#'
#' @aliases plot.train ggplot.train
#' @param x an object of class \code{\link{train}}.
#' @param metric What measure of performance to plot. Examples of possible
#' values are "RMSE", "Rsquared", "Accuracy" or "Kappa". Other values can be
#' used depending on what metrics have been calculated.
#' @param plotType a string describing the type of plot (\code{"scatter"},
#' \code{"level"} or \code{"line"} (\code{plot} only))
#' @param digits an integer specifying the number of significant digits used to
#' label the parameter value.
#' @param xTrans a function that will be used to scale the x-axis in scatter
#' plots.
#' @param data an object of class \code{\link{train}}.
#' @param output either "data", "ggplot" or "layered". The first returns a data
#' frame while the second returns a simple \code{ggplot} object with no layers.
#' The third value returns a plot with a set of layers.
#' @param nameInStrip a logical: if there are more than 2 tuning parameters,
#' should the name and value be included in the panel title?
#' @param highlight a logical: if \code{TRUE}, a diamond is placed around the
#' optimal parameter setting for models using grid search.
#' @param mapping,environment unused arguments to make consistent with
#' \pkg{ggplot2} generic method
#' @param \dots \code{plot} only: specifications to be passed to
#' \code{\link[lattice]{levelplot}}, \code{\link[lattice]{xyplot}},
#' \code{\link[lattice:xyplot]{stripplot}} (for line plots). The function
#' automatically sets some arguments (e.g. axis labels) but passing in values
#' here will over-ride the defaults
#' @author Max Kuhn
#' @seealso \code{\link{train}}, \code{\link[lattice]{levelplot}},
#' \code{\link[lattice]{xyplot}}, \code{\link[lattice:xyplot]{stripplot}},
#' \code{\link[ggplot2]{ggplot}}
#' @references Kuhn (2008), ``Building Predictive Models in R Using the caret''
#' (\doi{10.18637/jss.v028.i05})
#' @keywords hplot
#' @method plot train
#' @export
#' @examples
#'
#'
#' \dontrun{
#' library(klaR)
#' rdaFit <- train(Species ~ .,
#'                 data = iris,
#'                 method = "rda",
#'                 control = trainControl(method = "cv"))
#' plot(rdaFit)
#' plot(rdaFit, plotType = "level")
#'
#' ggplot(rdaFit) + theme_bw()
#'
#' }
#'
#' @export plot.train
"plot.train" <-  function(x,
                    plotType = "scatter",
                    metric = x$metric[1],
                    digits = getOption("digits") - 3,
                    xTrans = NULL,
                    nameInStrip = FALSE,
                    ...)
  {


    ## Error trap
    if(!(plotType %in% c("level", "scatter", "line"))) stop("plotType must be either level, scatter or line")

    cutpoints <- c(0, 1.9, 2.9, 3.9, Inf)

    ## define some functions
    prettyVal <- function(u, dig, Name = NULL)
      {
        if(is.numeric(u))
          {
            if(!is.null(Name)) u <- paste(gsub(".", " ", Name, fixed = TRUE),
                                          ": ",
                                          format(u, digits = dig), sep = "")
            return(factor(u))
          } else return(if(!is.factor(u)) as.factor(u) else u)
      }

    ## Get tuning parameter info

    params <- as.character(x$modelInfo$parameters$parameter)

    if(grepl("adapt", x$control$method))
      warning("When using adaptive resampling, this plot may not accurately capture the relationship between the tuning parameters and model performance.")

    plotIt <- "yes"
    if(all(params == "parameter"))
      {
        plotIt <- "There are no tuning parameters for this model."
      } else {
        dat <- x$results

        ## Some exceptions for pretty printing
        if(x$method == "nb") dat$usekernel <- factor(ifelse(dat$usekernel, "Nonparametric", "Gaussian"))
        if(x$method == "gam") dat$select <- factor(ifelse(dat$select, "Feature Selection", "No Feature Selection"))
        if(x$method == "qrnn") dat$bag <- factor(ifelse(dat$bag, "Bagging", "No Bagging"))
        if(x$method == "C5.0") dat$winnow <- factor(ifelse(dat$winnow, "Winnowing", "No Winnowing"))
##        if(x$method %in% c("M5Rules", "M5", "PART")) dat$pruned <- factor(ifelse(dat$pruned == "Yes", "Pruned", "Unpruned"))
##        if(x$method %in% c("M5Rules", "M5")) dat$smoothed <- factor(ifelse(dat$smoothed == "Yes", "Smoothed", "Unsmoothed"))
        if(x$method == "M5") dat$rules <- factor(ifelse(dat$rules == "Yes", "Rules", "Trees"))
##        if(x$method == "vbmpRadial") dat$estimateTheta <- factor(ifelse(dat$estimateTheta == "Yes", "Estimate Theta", "Do Not Estimate Theta"))


        ## Check to see which tuning parameters were varied

        paramValues <- apply(dat[,params,drop = FALSE],
                             2,
                             function(x) length(unique(x)))
        ##paramValues <- paramValues[order(paramValues)]
        if(any(paramValues > 1))
          {
            params <- names(paramValues)[paramValues > 1]
          } else plotIt <- "There are no tuning parameters with more than 1 value."
      }

    if(plotIt == "yes")
      {
        p <- length(params)
        dat <- dat[, c(metric, params)]
        if(p > 1) {
          numUnique <- unlist(lapply(dat[, -1], function(x) length(unique(x))))
          numUnique <- sort(numUnique,  decreasing = TRUE)
          dat <- dat[, c(metric, names(numUnique))]
          params <- names(numUnique)
        }
        ## The conveintion is that the first parameter (in
        ## position #2 of dat) is plotted on the x-axis,
        ## the second parameter is the grouping variable
        ## and the rest are conditioning variables
        if(!is.null(xTrans) & plotType == "scatter") dat[,2] <- xTrans(dat[,2])

        ## We need to pretty-up some of the values of grouping
        ## or conditioning variables


        resampText <- resampName(x, FALSE)

        if(plotType %in% c("line", "scatter"))
          {

            if(plotType == "scatter")
              {
                if(p >= 2) for(i in 3:ncol(dat))
                  dat[,i] <- prettyVal(dat[,i], dig = digits, Name = if(i > 3) params[i-1] else  NULL)
              } else {
                for(i in 2:ncol(dat))
                  dat[,i] <- prettyVal(dat[,i], dig = digits, Name = if(i > 3) params[i-1] else  NULL)
              }
            for(i in 2:ncol(dat)) if(is.logical(dat[,i])) dat[,i] <- factor(dat[,i])
            if(p > 2 & nameInStrip) {
              strip_vars <- params[-(1:2)]
              strip_lab <- subset(x$modelInfo$parameters, parameter %in% strip_vars)$label
              for(i in seq_along(strip_vars))
                dat[, strip_vars[i]] <- factor(paste(strip_lab[i], dat[, strip_vars[i]], sep = ": "))
            }
            ## make formula
            form <- if(p <= 2)
              {
                as.formula(
                           paste(metric, "~", params[1], sep = ""))
              } else as.formula(paste(metric, "~", params[1], "|",
                                      paste(params[-(1:2)], collapse = "*"),
                                      sep = ""))
            defaultArgs <- list(x = form,
                                data = dat,
                                groups = if(p > 1) dat[,params[2]] else NULL)
            if(length(list(...)) > 0) defaultArgs <- c(defaultArgs, list(...))
            lNames <- names(defaultArgs)
            if(!("ylab" %in% lNames))  defaultArgs$ylab <- paste(metric, resampText)

            if(!("type" %in% lNames) & plotType == "scatter") defaultArgs$type <- c("g", "o")
            if(!("type" %in% lNames) & plotType == "line") defaultArgs$type <- c("g", "o")
            if(p > 1)
              {
                ## I apologize in advance for the following 3 line kludge.
                groupCols <- 4
                if(length(unique(dat[,3])) < 4) groupCols <- length(unique(dat[,3]))
                if(length(unique(dat[,3])) %in% 5:6) groupCols <- 3

                groupCols <- as.numeric(
                                        cut(length(unique(dat[,3])),
                                            cutpoints,
                                            include.lowest = TRUE))

                if(!(any(c("key", "auto.key") %in% lNames)))
                  defaultArgs$auto.key <- list(columns = groupCols,
                                               lines = TRUE,
                                               title = as.character(x$modelInfo$parameter$label)[x$modelInfo$parameter$parameter == params[2]],
                                               cex.title = 1)
              }
            if(!("xlab" %in% lNames)) defaultArgs$xlab <- as.character(x$modelInfo$parameter$label)[x$modelInfo$parameter$parameter == params[1]]

            if(plotType == "scatter")
              {
                out <- do.call("xyplot", defaultArgs)
              } else {
                ## line plot #########################
                out <- do.call("stripplot", defaultArgs)
              }

          }

        if(plotType == "level")
          {
            if(p == 1) stop("There must be at least 2 tuning parameters with multiple values")

            for(i in 2:ncol(dat))
              dat[,i] <- prettyVal(dat[,i], dig = digits, Name = if(i > 3) params[i-1] else  NULL)
            if(p > 2 & nameInStrip) {
              strip_vars <- params[-(1:2)]
              strip_lab <- subset(x$modelInfo$parameters, parameter %in% strip_vars)$label
              for(i in seq_along(strip_vars))
                dat[, strip_vars[i]] <- factor(paste(strip_lab[i], dat[, strip_vars[i]], sep = ": "))
            }
            ## make formula
            form <- if(p <= 2)
              {
                as.formula(paste(metric, "~", params[1], "*", params[2], sep = ""))
              } else as.formula(paste(metric, "~", params[1], "*", params[2], "|",
                                      paste(params[-(1:2)], collapse = "*"),
                                      sep = ""))
            defaultArgs <- list(x = form, data = dat)
            if(length(list(...)) > 0) defaultArgs <- c(defaultArgs, list(...))
            lNames <- names(defaultArgs)
            if(!("sub" %in% lNames)) defaultArgs$sub <- paste(metric, resampText)

            if(!("xlab" %in% lNames)) defaultArgs$xlab <- as.character(x$modelInfo$parameter$label)[x$modelInfo$parameter$parameter == params[1]]
            if(!("ylab" %in% lNames)) defaultArgs$ylab <- as.character(x$modelInfo$parameter$label)[x$modelInfo$parameter$parameter == params[2]]



            out <- do.call("levelplot", defaultArgs)
          }

      } else stop(plotIt)

    out


  }


