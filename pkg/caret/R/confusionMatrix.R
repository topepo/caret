confusionMatrix <- 
  function(data, ...){
    UseMethod("confusionMatrix")
  }

confusionMatrix.default <- function(data, reference,
                                    positive = NULL,
                                    dnn = c("Prediction", "Reference"),
                                    prevalence = NULL,
                                    ...)
{
  if(!is.factor(data)) data <- factor(data)
  if(!is.factor(reference)) reference <- factor(reference)
  if(!is.character(positive) & !is.null(positive)) stop("positive argument must be character")
  
  if(length(levels(data)) > length(levels(reference)))
    stop("the data cannot have more levels than the reference")
  
  if(!any(levels(data) %in% levels(reference))){
    stop("The data must contain some levels that overlap the reference.")
  } 
  
  if(!all(levels(data) %in% levels(reference))){
    badLevel <- levels(data)[!levels(data) %in% levels(reference)]
    if(sum(table(data)[badLevel]) > 0){
      stop("The data contain levels not found in the data.")
    } else{
      warning("The data contains levels not found in the data, but they are empty and will be dropped.")
      data <- factor(as.character(data))
    }
  }
  
  if(any(levels(reference) != levels(data))) {
    warning("Levels are not in the same order for reference and data. Refactoring data to match.")
    data <- as.character(data)
    data <- factor(data, levels = levels(reference))
  }
  classLevels <- levels(data)
  numLevels <- length(classLevels)
  if(numLevels < 2) 
    stop("there must be at least 2 factors levels in the data")
  
  if(numLevels == 2 & is.null(positive))  positive <- levels(reference)[1]
  
  classTable <- table(data, reference, dnn = dnn, ...)
  
  getFromNamespace("confusionMatrix.table", "caret")(classTable, positive, prevalence = prevalence)
}

confusionMatrix.table <- function(data, positive = NULL, prevalence = NULL, ...)
{
  requireNamespaceQuietStop("e1071")
  
  if(length(dim(data)) != 2) stop("the table must have two dimensions")
  if(!all.equal(nrow(data), ncol(data))) stop("the table must nrow = ncol")
  if(!all.equal(rownames(data), colnames(data))) stop("the table must the same classes in the same order")
  if(!is.character(positive) & !is.null(positive)) stop("positive argument must be character")
  
  classLevels <- rownames(data)
  numLevels <- length(classLevels)
  if(numLevels < 2) 
    stop("there must be at least 2 factors levels in the data")
  
  if(numLevels == 2 & is.null(positive))  positive <- rownames(data)[1]
  
  
  if(numLevels == 2 & !is.null(prevalence) && length(prevalence) != 1)
    stop("with two levels, one prevalence probability must be specified")
  
  if(numLevels > 2 & !is.null(prevalence) && length(prevalence) != numLevels)
    stop("the number of prevalence probability must be the same as the number of levels")
  
  if(numLevels > 2 & !is.null(prevalence) && is.null(names(prevalence)))
    stop("with >2 classes, the prevalence vector must have names")
  
  propCI <- function(x)
  {
    binom.test(sum(diag(x)), sum(x))$conf.int
  }
  
  propTest <- function(x)
  {
    out <- binom.test(sum(diag(x)),
                      sum(x),
                      p = max(apply(x, 2, sum)/sum(x)),
                      alternative = "greater")
    unlist(out[c("null.value", "p.value")])
    
  }
  
  overall <- c(
    unlist(e1071::classAgreement(data))[c("diag", "kappa")],
    propCI(data),
    propTest(data),
    mcnemar.test(data)$p.value)
  
  
  names(overall) <- c("Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper", "AccuracyNull", "AccuracyPValue", "McnemarPValue")  
  
  if(numLevels == 2)
  {
    if(is.null(prevalence)) prevalence <- sum(data[, positive])/sum(data)
    negative <- classLevels[!(classLevels %in% positive)]
    tableStats <- c(sensitivity.table(data, positive),
                    specificity.table(data, negative),
                    posPredValue.table(data, positive, prevalence = prevalence),
                    negPredValue.table(data, negative, prevalence = prevalence),
                    prevalence,
                    sum(data[positive, positive])/sum(data),
                    sum(data[positive, ])/sum(data))
    names(tableStats) <- c("Sensitivity", "Specificity",
                           "Pos Pred Value", "Neg Pred Value",
                           "Prevalence", "Detection Rate",
                           "Detection Prevalence")   
    tableStats["Balanced Accuracy"] <- (tableStats["Sensitivity"]+tableStats["Specificity"])/2
    
  } else {
    
    tableStats <- matrix(NA, nrow = length(classLevels), ncol = 8)
    
    for(i in seq(along = classLevels))
    {
      pos <- classLevels[i]
      neg <- classLevels[!(classLevels %in% classLevels[i])]
      prev <- if(is.null(prevalence)) sum(data[, pos])/sum(data) else prevalence[pos]
      tableStats[i,] <- c(sensitivity.table(data, pos),
                          specificity.table(data, neg),
                          posPredValue.table(data, pos, prevalence = prev),
                          negPredValue.table(data, neg, prevalence = prev),
                          prev,
                          sum(data[pos, pos])/sum(data),
                          sum(data[pos, ])/sum(data), NA)          
      tableStats[i,8] <- (tableStats[i,1] + tableStats[i,2])/2
    }
    rownames(tableStats) <- paste("Class:", classLevels)
    colnames(tableStats) <- c("Sensitivity", "Specificity",
                              "Pos Pred Value", "Neg Pred Value",
                              "Prevalence", "Detection Rate",
                              "Detection Prevalence", "Balanced Accuracy")  
  }
  
  structure(list(
    positive = positive,
    table = data, 
    overall = overall, 
    byClass = tableStats,
    dots = list(...)), 
    class = "confusionMatrix")
}


as.matrix.confusionMatrix <- function(x, what = "xtabs", ...)
{
  if(!(what %in% c("xtabs", "overall", "classes")))
    stop("what must be either xtabs, overall or classes")
  out <- switch(what,
                xtabs = matrix(as.vector(x$table),
                               nrow = length(colnames(x$table)),
                               dimnames = list(rownames(x$table), colnames(x$table))),
                overall = as.matrix(x$overall),
                classes = as.matrix(x$byClass))
  if(what == "classes")
  {
    if(length(colnames(x$table)) > 2)
    {
      out <- t(out)
      colnames(out) <- gsub("Class: ", "", colnames(out), fixed = TRUE)
    }
  }
  out
}

sbf_resampledCM <- function(x) {
  lev <- x$obsLevels
  if("pred" %in% names(x) && !is.null(x$pred)) {
    resampledCM <- do.call("rbind", x$pred[names(x$pred) == "predictions"])
    resampledCM <- ddply(resampledCM, .(Resample), function(y) flatTable(pred  = y$pred, obs = y$obs))
  } else stop(paste("When there are 50+ classes, the function does not automatically pre-compute the",
                    "resampled confusion matrices. You can get them when the option",
                    "`saveDetails = TRUE`."))
  resampledCM
}
rfe_resampledCM <- function(x) {
  lev <- x$obsLevels
  if("resample" %in% names(x) && 
     !is.null(x$resample) && 
     sum(grepl("\\.cell[1-9]", names(x$resample))) > 3) {
    resampledCM <- subset(x$resample, Variables == x$optsize)
    resampledCM <- resampledCM[,grepl("\\.cell[1-9]", names(resampledCM))]
  } else {
    if(!is.null(x$pred)) {
      resampledCM <- ddply(x$pred, .(Resample), function(y) flatTable(pred  = y$pred, obs = y$obs))
    } else {
      if(length(lev) > 50)
        stop(paste("When there are 50+ classes, `the function does not automatically pre-compute the",
                   "resampled confusion matrices. You can get them when the object",
                   "has a `pred` element."))
    }
  }
  resampledCM
}
train_resampledCM <- function(x) {
  if(x$modelType == "Regression")
    stop("confusion matrices are only valid for classification models")
  
  lev <- levels(x)
  
  ## For problems with large numbers of classes, `train`, `rfe`, and `sbf` do not pre-compute the 
  ## the resampled matrices. If the predictions have been saved, we can get them from there. 
  
  if("resampledCM" %in% names(x) && !is.null(x$resampledCM)) {
    ## get only best tune
    names(x$bestTune) <- gsub("^\\.", "", names(x$bestTune))
    resampledCM <- merge(x$bestTune, x$resampledCM)
  } else {
    if(!is.null(x$pred)) {
      resampledCM <- ddply(merge(x$pred, x$bestTune), .(Resample), function(y) flatTable(pred  = y$pred, obs = y$obs))
    } else {
      if(length(lev) > 50)
        stop(paste("When there are 50+ classes, `train` does not automatically pre-compute the",
                   "resampled confusion matrices. You can get them from this function",
                   "using a value of `savePredictions` other than FALSE."))
    }
  }
  resampledCM
}

as.table.confusionMatrix <- function(x, ...)  x$table

confusionMatrix.train <- function(data, norm = "overall", dnn = c("Prediction", "Reference"), ...)
{
  if(data$control$method %in% c("oob", "LOOCV", "none"))
    stop("cannot compute confusion matrices for leave-one-out, out-of-bag resampling, or no resampling")
  
  if (inherits(data, "train")) {
    if(data$modelType == "Regression")
      stop("confusion matrices are only valid for classification models")
    lev <- levels(data)
    ## For problems with large numbers of classes, `train`, `rfe`, and `sbf` do not pre-compute the 
    ## the resampled matrices. If the predictions have been saved, we can get them from there. 
    resampledCM <- train_resampledCM(data)
    } else {
    lev <- data$obsLevels
    if (inherits(data, "rfe")) resampledCM <- rfe_resampledCM(data)
    if (inherits(data, "sbf")) resampledCM <- sbf_resampledCM(data)    
  }

  if(!is.null(data$control$index)) {
    resampleN <- unlist(lapply(data$control$index, length))
    numResamp <- length(resampleN)
    resampText <- resampName(data)
  } else {
    resampText <- ""
    numResamp <- 0
  }
  
  counts <- as.matrix(resampledCM[ , grep("^\\.?cell", colnames(resampledCM))])
  
  ## normalize?
  norm <- match.arg(norm, c("none", "overall", "average"))
  
  if(norm == "none") counts <- matrix(apply(counts, 2, sum), nrow = length(lev))
  else counts <- matrix(apply(counts, 2, mean), nrow = length(lev))
  
  if(norm == "overall") counts <- counts / sum(counts) * 100
  
  ## names
  rownames(counts) <- colnames(counts) <- lev
  names(dimnames(counts)) <- dnn
  
  ## out
  out <- list(table = as.table(counts),
              norm = norm,
              B = length(data$control$index),
              text = paste(resampText, "Confusion Matrix"))
  class(out) <- paste0("confusionMatrix.", class(data))
  out
}

confusionMatrix.rfe <- confusionMatrix.train
confusionMatrix.sbf <- confusionMatrix.train

print.confusionMatrix.train <- function(x, digits = 1, ...)
{
  cat(x$text, "\n")
  normText <- switch(x$norm,
                     none = "\n(entries are un-normalized aggregated counts)\n",
                     average = "\n(entries are average cell counts across resamples)\n",
                     overall = "\n(entries are percentual average cell counts across resamples)\n",
                     "")
  cat(normText, "\n")
  if(x$norm == "none" & x$B == 1) {
    print(getFromNamespace("confusionMatrix.table", "caret")(x$table)) 
  } else {
    print(round(x$table, digits))
    
    out <- cbind("Accuracy (average)", ":", formatC(sum(diag(x$table) / sum(x$table))))
    
    dimnames(out) <- list(rep("", nrow(out)), rep("", ncol(out)))
    print(out, quote = FALSE)
    cat("\n")
  }
  invisible(x)
}

print.confusionMatrix.rfe <- print.confusionMatrix.train
print.confusionMatrix.sbf <- print.confusionMatrix.train

resampName <- function(x, numbers = TRUE)
{
  if(!("control" %in% names(x))) return("")
  if(numbers) {
    resampleN <- unlist(lapply(x$control$index, length))
    numResamp <- length(resampleN)
    out <- switch(tolower(x$control$method),
                  none = "None",
                  apparent = "Apparent",
                  custom = paste("Custom Resampling (", numResamp, " reps)", sep = ""),
                  timeslice = paste("Rolling Forecasting Origin Resampling (",
                                    x$control$horizon, " held-out with",
                                    ifelse(x$control$fixedWindow, " a ", " no "),
                                    "fixed window)", sep = ""),
                  oob = "Out of Bag Resampling",
                  boot =, boot632 = paste("Bootstrapped (", numResamp, " reps)", sep = ""),
                  cv = paste("Cross-Validated (", x$control$number, " fold)", sep = ""),
                  repeatedcv = paste("Cross-Validated (", x$control$number, " fold, repeated ",
                                     x$control$repeats, " times)", sep = ""),
                  lgocv = paste("Repeated Train/Test Splits Estimated (", numResamp, " reps, ",
                                round(x$control$p*100, 1), "%)", sep = ""),
                  loocv = "Leave-One-Out Cross-Validation",
                  adaptive_boot = paste("Adaptively Bootstrapped (", numResamp, " reps)", sep = ""),
                  adaptive_cv = paste("Adaptively Cross-Validated (", x$control$number, " fold, repeated ",
                                      x$control$repeats, " times)", sep = ""),
                  adaptive_lgocv = paste("Adaptive Repeated Train/Test Splits Estimated (", numResamp, " reps, ",
                                         round(x$control$p, 2), "%)", sep = "")
    )
  } else {
    out <- switch(tolower(x$control$method),
                  none = "None",
                  apparent = "(Apparent)", 
                  custom = "Custom Resampling",
                  timeslice = "Rolling Forecasting Origin Resampling",
                  oob = "Out of Bag Resampling",
                  boot = "(Bootstrap)",
                  boot632 = "(Bootstrap 632 Rule)",
                  cv = "(Cross-Validation)",
                  repeatedcv = "(Repeated Cross-Validation)",
                  loocv = "Leave-One-Out Cross-Validation",
                  lgocv = "(Repeated Train/Test Splits)")
    
  }
  out
}


mcc <- function(tab, pos = colnames(tab)[1])
{
  if(nrow(tab) != 2 | ncol(tab) != 2) stop("A 2x2 table is needed")
  neg <- colnames(tab)[colnames(tab) != pos]
  tp <- tab[pos, pos]
  tn <- tab[neg, neg]
  fp <- tab[pos,neg]
  fn <- tab[neg, pos]
  d1 <- tp + fp
  d2 <- tp + fn
  d3 <- tn + fp
  d4 <- tn + fn
  if(d1 == 0 | d2 == 0 | d3 == 0 | d4 == 0) return(0)
  ((tp * tn) - (fp * fn))/sqrt(d1*d2*d3*d4)  
}

get_sbf_rs <- function(obj) {
  
}
