###################################################################
##

## Todo: 
## X. Make an argument for what to save ("prediction", "probabilities", "both")
## X. Adapt code to average predictions/probabilities
## 1. Use train/rfe/sbf saved prediction function to make predictions
##    for the ensemble.
## X. Change `nrow` to an average of `complete.cases`
## X. Detect class prob column names and drop off the last one
## X. Breakout the 'n' code so that it isn't converted to character
## X. Call it "oob" or "oob_pred"
## 2. Make `oob_pred.list` smaller and create a new function to
##    do the real work
## 3. TEST TEST TEST

###################################################################
##

oob_pred <- function (x, ...) UseMethod("oob_pred")

#' @export
oob_pred.train <- function(x, best = TRUE, average = TRUE) {

  if(is.null(x$pred)) 
    stop("re-fit the model using 'trainControl(savePredictions=TRUE)'")
  prd <- x$pred
  pname <- as.character(x$modelInfo$parameters$parameter)
  if(best) { 
    prd <- merge(prd, x$bestTune)
    prd <- prd[, !(colnames(prd) %in% pname)]
  } 
  bycol <- "rowIndex"
  if(!best) bycol <- c(bycol, pname)
  
  if(average) prd <- get_averages(x, prd, bycol)
  
  if(x$modelType == "Classification") {
    lev <- train_lev(x)
    if(!is.null(lev)) {
      if(is.character(prd$obs))  prd$obs  <- factor(prd$obs,  levels = lev)
      if(is.character(prd$pred))  prd$pred <- factor(prd$pred, levels = lev)    
    }
  }
  prd
}

#' @export
oob_pred.rfe <- function(x, best = TRUE, average = TRUE) {

  if(is.null(x$pred)) 
    stop("re-fit the model using 'rfeControl(saveDetails=TRUE)'")
  prd <- x$pred
  
  if(best) { 
    prd <- subset(prd, Variables == x$bestSubset)
    prd <- prd[, colnames(prd) != "Variables"]
    bycol <- "rowIndex"
  } else bycol <- c("rowIndex", "Variables")
  ## Temp kludge since I think there is an issue with
  ## how `rfe` saves the data
  prd <- prd[!duplicated(prd),]
  
  if(average)  prd <- get_averages(x, prd, bycol)
  
  if(is.character(prd$obs)  && !is.null(x$obsLevels))  
    prd$obs  <- factor(prd$obs,  levels = x$obsLevels)
  if(is.character(prd$pred) && !is.null(x$obsLevels)) 
    prd$pred <- factor(prd$pred, levels = x$obsLevels)  
  
  prd
}

#' @export
oob_pred.sbf <- function(x, average = TRUE) {

  if(is.null(x$pred)) 
    stop("re-fit the model using 'rfeControl(saveDetails=TRUE)'")
  prd <- x$pred[names(x$pred) == "predictions"]
  prd <- rbind.fill(prd)
  prd <- prd[!duplicated(prd),]
  
  if(average) prd <- get_averages(x, prd, bycol = "rowIndex")
  
  if(is.character(prd$obs)  && !is.null(x$obsLevels))  
    prd$obs  <- factor(prd$obs,  levels = x$obsLevels)
  if(is.character(prd$pred) && !is.null(x$obsLevels)) 
    prd$pred <- factor(prd$pred, levels = x$obsLevels)  
  
  prd
}

#' @importFrom stats reshape
#' @export
oob_pred.list <- function(x, direction = "wide", what = "both", ...) {
  num <- length(x)
  oob <- lapply(x, oob_pred, ...)
  
  ## check column names and get those common to everything
  cnames <- lapply(oob, colnames)
  cnames <- table(unlist(cnames))
  if(any(cnames < num)) {
    cnames <- names(cnames[cnames == num])
    for(i in 1:num) oob[[i]] <- oob[[i]][, cnames]
  }
  
  nms <- names(oob)
  if(is.null(nms)) nms <- well_numbered("Model", length(oob)) 
  for(i in seq(along = nms)) oob[[i]]$.label <- nms[i]
  oob <- rbind.fill(oob)
  if(length(table(table(oob$n))) > 1)
    stop("Some averages have different sample sizes than others")
  if(direction == "wide") {
    vert_names <- colnames(oob)
    vert_names <- vert_names[!(vert_names %in% c("rowIndex", "n", "obs", ".label"))]
    
    oob <- reshape(oob, direction = "wide", 
                   v.names = vert_names, 
                   idvar = c("rowIndex", "obs", "n"),
                   timevar = ".label")
    vert_names <- vert_names[vert_names != "pred"]
    exclude <- vert_names[length(vert_names)]
    oob <- oob[, !grepl(paste0("^", exclude, "\\."), names(oob))]
    
    if(!is.null(what) && !("both" %in% what)) {
      if(!is.null(what) && !("pred" %in% what))
        oob <- oob[, !grepl("^pred\\.", names(oob))]
      if(!is.null(what) && !("prob" %in% what)) {
        for(i in vert_names) 
          oob <- oob[, !grepl(paste0("^", i, "\\."), names(oob))]
      }
    }
  }
  oob
}

###################################################################
##

get_averages <- function (x, ...) UseMethod("get_averages")

#' @importFrom stats complete.cases
get_averages.train <- function(x, prd, bycol = "rowIndex") {
  if("Regression" %in% x$modelType) {
    out <- ddply(prd, bycol,
                 function(x) c(colMeans(x[, c("pred", "obs")])))
  } else {
    out <- ddply(prd, bycol,
                 function(x) c(pred = char_mode(x$pred), 
                               obs = as.character(x$obs)[1]))
    if(x$control$classProbs) {
      lev <- train_lev(x)
      cprobs <- ddply(prd, bycol,
                      function(x, lev) c(colMeans(x[, lev])),
                      lev = lev)      
      out <- merge(out, cprobs)
    }
  }
  n <- ddply(prd, bycol, function(x) c(n = sum(complete.cases(x))))
  out <- merge(out, n)
  out
}

#' @importFrom stats complete.cases
get_averages.rfe <- function(x, prd, bycol = "rowIndex") {
  if(is.null(x$obsLevels)) {
    out <- ddply(prd, bycol,
                 function(x) c(colMeans(x[, c("pred", "obs")])))
  } else {
    out <- ddply(prd, bycol,
                 function(x) c(pred = char_mode(x$pred), 
                               obs = as.character(x$obs)[1]))
    if(all(x$obsLevels %in% colnames(prd))) {
      lev <- x$obsLevels
      cprobs <- ddply(prd, bycol,
                      function(x, lev) c(colMeans(x[, lev])),
                      lev = lev)      
      out <- merge(out, cprobs)
    }
  }
  n <- ddply(prd, bycol, function(x) c(n = sum(complete.cases(x))))
  out <- merge(out, n)
  out
}

#' @importFrom stats complete.cases
get_averages.sbf <- function(x, prd, bycol = "rowIndex") {
  if(is.null(x$obsLevels)) {
    out <- ddply(prd, bycol,
                 function(x) c(colMeans(x[, c("pred", "obs")])))
  } else {
    out <- ddply(prd, bycol,
                 function(x) c(pred = char_mode(x$pred), 
                               obs = as.character(x$obs)[1]))
    if(all(x$obsLevels %in% colnames(prd))) {
      lev <- x$obsLevels
      cprobs <- ddply(prd, bycol,
                      function(x, lev) c(colMeans(x[, lev])),
                      lev = lev)      
      out <- merge(out, cprobs)
    }
  }
  n <- ddply(prd, bycol, function(x) c(n = sum(complete.cases(x))))
  out <- merge(out, n)
  out
}

###################################################################
##

#' @importFrom stats complete.cases
char_mode <- function(x, random = TRUE, na.rm = FALSE) {
  if(na.rm) x <- x[complete.cases(x)]
  tab <- table(x)
  tab <- tab[tab == max(tab)]
  tab <- if(length(tab) > 1 & random) sample(tab, 1) else tab[1]
  as.vector(names(tab))
}

train_lev <- function(x) {
  if(x$modelType == "Classification") {
    if(!is.null(x$modelInfo$levels)) {
      lev <- x$modelInfo$levels(x$finalModel)
    } else {
      lev <- if(!isS4(x)) x$finalModel$obsLevel else unique(x$pred$obs)
    }
  } else lev <- NULL
  lev
}


#' @importFrom stats cor
corr_mat <- function (object, metric = object$metrics, 
                       ...) {
  dat <- object$values[, grepl(paste0("~", metric[1]), 
                               colnames(object$values))]
  colnames(dat) <- gsub(paste0("~", metric[1]), "", colnames(dat))
  dat <- cor(dat, ...)
  dat
}



