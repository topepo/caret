#' Internal Functions
#' @name caret-internal
#' @aliases createModel resampleWrapper sortImp caretTheme progress hasTerms predictionFunction probFunction expandParameters flatTable MeanSD sbfIter gamFormula bagEarthStats cforestStats ipredStats rfStats well_numbered
#'
#' @description Internal functions
#'
#'
#' @author Max Kuhn, but \code{caretTheme} uses an expanded grid of the "Blues" palette designed by Cynthia Brewer and Mark Harrower
#'
#' @importFrom stats predict
#' @export
#' @keywords internal
"createModel" <-function(x, y, wts, method, tuneValue, obsLevels, pp = NULL, last = FALSE, sampling = NULL, classProbs, ...) {

  ## To get of warnings "some row.names duplicated: " when resampling with replacement
  if(is.data.frame(x) | is.matrix(x)) 
    rownames(x) <- make.names(rownames(x), unique = TRUE)

  if(!is.null(sampling) && sampling$first) {
    tmp <- sampling$func(x, y)
    x <- tmp$x
    y <- tmp$y
    rm(tmp)
  }

  if(!is.null(pp$options)) {
    pp$method <- pp$options
    pp$options <- NULL
    if("ica" %in% pp$method) pp$n.comp <- pp$ICAcomp
    pp$ICAcomp <- NULL
    pp$x <- x
    pp$outcome <- y
    ppObj <- do.call("preProcess", pp)
    ppObj$call <- "scrubed"
    x <- predict(ppObj, x)
    rm(pp)
  } else ppObj <- NULL

  if(!is.null(sampling) && !sampling$first) {
    tmp <- sampling$func(x, y)
    x <- tmp$x
    y <- tmp$y
    rm(tmp)
  }

  modelFit <- method$fit(x = x,
                         y = y, wts = wts,
                         param  = tuneValue, lev = obsLevels,
                         last = last,
                         classProbs = classProbs, ...)
  ## for models using S4 classes, you can't easily append data, so
  ## exclude these and we'll use other methods to get this information
  if(is.null(method$label)) method$label <- ""
  if(!isS4(modelFit) &
       !(method$label %in% c("Ensemble Partial Least Squares Regression",
                             "Ensemble Partial Least Squares Regression with Feature Selection"))) {
    modelFit$xNames <- colnames(x)
    modelFit$problemType <- if(is.factor(y)) "Classification" else "Regression"
    modelFit$tuneValue <- tuneValue
    modelFit$obsLevels <- obsLevels
    modelFit$param <- list(...)
  }

  list(fit = modelFit, preProc = ppObj)
}
