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
  
  if(length(levels(data)) != length(levels(reference)))
    stop("the data and reference factors must have the same number of levels")
  
  if(any(levels(data) != levels(reference)))
    stop("the data and reference values must have exactly the same levels")
  
  classLevels <- levels(data)
  numLevels <- length(classLevels)
  if(numLevels < 2) 
    stop("there must be at least 2 factors levels in the data")
  
  if(numLevels == 2 & is.null(positive))  positive <- levels(reference)[1]
  
  classTable <- table(data, reference, dnn = dnn, ...)
  
  confusionMatrix(classTable, positive, prevalence = prevalence)
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

as.table.confusionMatrix <- function(x, ...)  x$table



confusionMatrix.train <- function(data, norm = "overall", dnn = c("Prediction", "Reference"), ...)
{
  if(data$modelType == "Regression") stop("confusion matrices are only valid for classification models")
  if(!norm %in% c("none", "overall", "average")) stop("values for norm should be 'none', 'overall', 'byClass' or 'average'")
  if(data$control$method %in% c("oob", "LOOCV", "none")) stop("cannot compute confusion matrices for leave-one-out, out-of-bag resampling or no resampling")
  if(!is.null(data$control$index))
  {
    resampleN <- unlist(lapply(data$control$index, length))
    numResamp <- length(resampleN)
    resampText <- resampName(data)
  } else {
    resampText <- ""
    numResamp <- 0
  }
  
  lev <- levels(data)
  ## get only best tune
  names(data$bestTune) <- gsub("^\\.", "", names(data$bestTune))
  resampledCM <- merge(data$bestTune, data$resampledCM)
  counts <- as.matrix(resampledCM[,grep("^cell", colnames(resampledCM))])
  ## normalize by true class?
  
  if(norm == "overall") counts <- t(apply(counts, 1, function(x)x/sum(x)))
  if(norm == "average") counts <- counts/numResamp
  overall <- matrix(apply(counts, 2, mean), nrow = length(lev))
  rownames(overall) <- colnames(overall) <- lev
  if(norm != "none") overall <- overall*100
  names(dimnames(overall)) <- dnn
  
  
  out <- list(table = as.table(overall),
              norm = norm,
              B = length(data$control$index),
              text = paste(resampText, "Confusion Matrix"))
  class(out) <- "confusionMatrix.train"
  out
}


print.confusionMatrix.train <- function(x, digits = 1, ...)
{
  cat(x$text, "\n")
  normText <- switch(x$norm,
                     none = "\n(entries are un-normalized counts)\n",
                     average = "\n(entries are cell counts per resample)\n",
                     overall = "\n(entries are percentages of table totals)\n",
                     byClass = "\n(entries are percentages within the reference class)\n",
                     "")
  cat(normText, "\n")
  if(x$norm == "none" & x$B == 1) print(confusionMatrix(x$table)) else print(round(x$table, digits))
  cat("\n")
  invisible(x)
}

confusionMatrix.rfe <- function(data, norm = "overall", dnn = c("Prediction", "Reference"), ...)
{
  if(is.null(data$resampledCM)) stop("resampled confusion matrices are not availible")
  if(!norm %in% c("none", "overall", "average")) stop("values for norm should be 'none', 'overall', 'byClass' or 'average'")
  if(data$control$method %in% c("oob", "LOOCV")) stop("cannot compute confusion matrices for leave-one-out and out-of-bag resampling")
  if(!is.null(data$control$index))
  {
    resampleN <- unlist(lapply(data$control$index, length))
    numResamp <- length(resampleN)
    resampText <- resampName(data)
  } else {
    resampText <- ""
    numResamp <- 0
  }
  
  
  resampledCM <- data$resampledCM
  counts <- as.matrix(resampledCM[,grep("^\\.cell", colnames(resampledCM))])
  ## normalize by true class?
  
  if(norm == "overall") counts <- t(apply(counts, 1, function(x)x/sum(x)))
  if(norm == "average") counts <- counts/numResamp
  overall <- matrix(apply(counts, 2, mean), nrow = length(data$obsLevels))
  rownames(overall) <- colnames(overall) <- data$obsLevels
  overall <- overall*100
  names(dimnames(overall)) <- dnn
  
  
  out <- list(table = overall,
              norm = norm,
              B = numResamp, 
              text = paste(resampText, "Confusion Matrix"))
  class(out) <- "confusionMatrix.rfe"
  out
}


resampName <- function(x, numbers = TRUE)
{
  if(!("control" %in% names(x))) return("")
  if(numbers) {
    resampleN <- unlist(lapply(x$control$index, length))
    numResamp <- length(resampleN)
    out <- switch(tolower(x$control$method),
                  none = "None",
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
                                round(x$control$p, 2), "%)", sep = ""),
                  
                  adaptive_boot = paste("Adaptively Bootstrapped (", numResamp, " reps)", sep = ""),
                  adaptive_cv = paste("Adaptively Cross-Validated (", x$control$number, " fold, repeated ",
                                     x$control$repeats, " times)", sep = ""),
                  adaptive_lgocv = paste("Adaptive Repeated Train/Test Splits Estimated (", numResamp, " reps, ",
                                round(x$control$p, 2), "%)", sep = "")
                  )
  } else {
    out <- switch(tolower(x$control$method),
                  none = "None", 
                  timeslice = "Rolling Forecasting Origin Resampling",
                  oob = "Out of Bag Resampling",
                  boot = "(Bootstrap)",
                  boot632 = "(Bootstrap 632 Rule)",
                  cv = "(Cross-Validation)",
                  repeatedcv = "(Repeated Cross-Validation)",
                  lgocv = "(Repeated Train/Test Splits)")
    
  }
  out
}

confusionMatrix.sbf <- function(data, norm = "overall", dnn = c("Prediction", "Reference"), ...)
{
  if(is.null(data$resampledCM)) stop("resampled confusion matrices are not availible")
  if(!norm %in% c("none", "overall", "average")) stop("values for norm should be 'none', 'overall', 'byClass' or 'average'")
  if(data$control$method %in% c("oob", "LOOCV")) stop("cannot compute confusion matrices for leave-one-out and out-of-bag resampling")
  if(!is.null(data$control$index))
  {
    resampleN <- unlist(lapply(data$control$index, length))
    numResamp <- length(resampleN)
    resampText <- resampName(data)
  } else {
    resampText <- ""
    numResamp <- 0
  }
  
  resampledCM <- data$resampledCM
  counts <- as.matrix(resampledCM[,grep("^\\.cell", colnames(resampledCM))])
  ## normalize by true class?
  
  if(norm == "overall") counts <- t(apply(counts, 1, function(x)x/sum(x)))
  if(norm == "average") counts <- counts/numResamp
  overall <- matrix(apply(counts, 2, mean), nrow = length(data$obsLevels))
  rownames(overall) <- colnames(overall) <- data$obsLevels
  overall <- overall*100
  names(dimnames(overall)) <- dnn
  
  
  out <- list(table = overall,
              norm = norm,
              B = numResamp,
              text = paste(resampText, "Confusion Matrix"))
  class(out) <- "confusionMatrix.sbf"
  out
}

print.confusionMatrix.rfe <- print.confusionMatrix.train
print.confusionMatrix.sbf <- print.confusionMatrix.train



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
