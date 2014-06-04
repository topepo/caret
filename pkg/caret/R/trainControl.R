trainControl <- function(method = "boot",
                         number = ifelse(grepl("cv", method), 10, 25),
                         repeats = ifelse(grepl("cv", method), 1, number),
                         p = .75,
                         initialWindow = NULL,
                         horizon = 1,
                         fixedWindow = TRUE,
                         verboseIter = FALSE,
                         returnData = TRUE,
                         returnResamp = "final",
                         savePredictions = FALSE,
                         classProbs = FALSE,
                         summaryFunction = defaultSummary,
                         selectionFunction = "best",
                         preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5),
                         index = NULL,
                         indexOut = NULL,
                         timingSamps = 0,
                         predictionBounds = rep(FALSE, 2),
                         seeds = NA,
                         adaptive = list(min = 5, alpha = 0.05, method = "gls", complete = TRUE),
                         allowParallel = TRUE)
{
  if(is.null(selectionFunction)) stop("null selectionFunction values not allowed")
  if(!(returnResamp %in% c("all", "final", "none"))) stop("incorrect value of returnResamp")
  if(length(predictionBounds) > 0 && length(predictionBounds) != 2) stop("'predictionBounds' should be a logical or numeric vector of length 2")
  if(any(names(preProcOptions) == "method")) stop("'method' cannot be specified here")
  if(any(names(preProcOptions) == "x")) stop("'x' cannot be specified here")

  if(!(adaptive$method %in% c("gls", "BT"))) stop("incorrect value of adaptive$method")
  if(adaptive$alpha < .0000001 | adaptive$alpha > 1) stop("incorrect value of adaptive$alpha")
  if(grepl("adapt", method)) {
    num <- if(method == "adaptive_cv") number*repeats else number
    if(adaptive$min >= num) stop(paste("adaptive$min should be less than", num))
    if(adaptive$min <= 1) stop("adaptive$min should be greater than 1")
  }
  
  list(method = method,
       number = number,
       repeats = repeats,
       p = p,
       initialWindow = initialWindow,
       horizon = horizon,
       fixedWindow = fixedWindow,
       verboseIter = verboseIter,
       returnData = returnData,
       returnResamp = returnResamp,
       savePredictions = savePredictions,
       classProbs = classProbs,
       summaryFunction = summaryFunction,
       selectionFunction = selectionFunction,
       preProcOptions = preProcOptions,
       index = index,
       indexOut = indexOut,
       timingSamps = timingSamps,
       predictionBounds = predictionBounds,
       seeds = seeds,
       adaptive = adaptive,
       allowParallel = allowParallel)
}



