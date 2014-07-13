###################################################################
##

## Todo: 
## 1. Make an argument for what to save ("prediction", "probabilities", "both")
## 2. Adapt code to average predictions/probabilities
## 3. Use train/rfe/sbf saved prediction function to make predictions
##    for the ensemble.
## 4. Change `nrow` to an average of `complete.cases`

heldout <- function (x, ...) UseMethod("heldout")

heldout.train <- function(x, best = TRUE, average = TRUE) {
  library(plyr)
  if(is.null(x$pred)) 
    stop("re-fit the model using 'trainControl(savePredictions=TRUE)'")
  prd <- x$pred
  pname <- as.character(x$modelInfo$parameters$parameter)
  if(best) { 
    prd <- merge(prd, x$bestTune)
    prd <- prd[, colnames(prd) != pname]
  } 
  bycol <- "rowIndex"
  if(!best) bycol <- c(bycol, pname)
  
  if(average) 
    prd <- ddply(prd, bycol,
                 function(x) c(colMeans(x[, c("pred", "obs")]), 
                               n = nrow(x)))
  
  prd
}

heldout.rfe <- function(x, best = TRUE, average = TRUE) {
  library(plyr)
  if(is.null(x$pred)) 
    stop("re-fit the model using 'rfeControl(saveDetails=TRUE)'")
  prd <- x$pred
  if(best) { 
    prd <- subset(prd, Variables == x$bestSubset)
    prd <- prd[, colnames(prd) != "Variables"]
  } 
  
  if(average) 
    prd <- ddply(prd, "rowIndex",
                 function(x) c(colMeans(x[, c("pred", "obs")]), 
                               n = nrow(x)))
  
  prd
}

heldout.sbf <- function(x, average = TRUE) {
  library(plyr)
  if(is.null(x$pred)) 
    stop("re-fit the model using 'rfeControl(saveDetails=TRUE)'")
  prd <- x$pred[names(x$pred) == "predictions"]
  prd <- rbind.fill(prd)
  if(average) 
    prd <- ddply(prd, "rowIndex",
                 function(x) c(colMeans(x[, c("pred", "obs")]), 
                               n = nrow(x)))
  
  prd
}

heldout.list <- function(x, direction = "long", ...) {
  hos <- lapply(x, heldout, ...)
  nms <- names(hos)
  if(is.null(nms)) nms <- paste("Model", gsub(" ", "0", format(1:length(hos))), sep = "")
  for(i in seq(along = nms)) hos[[i]]$.label <- nms[i]
  hos <- rbind.fill(hos)
  if(direction == "wide") {
    hos <- reshape(hos, direction = "wide", 
                   v.names = "pred", idvar = c("rowIndex", "obs", "n"),
                   timevar = ".label")
    colnames(hos) <- gsub("pred\\.", "", colnames(hos))
  }
  hos
}
