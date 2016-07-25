recall <- function(data, ...) UseMethod("recall")

"recall.table" <- function(data, relevant = rownames(data)[1], ...){
  if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
  if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")
  
  if(nrow(data) > 2) {
    tmp <- data
    data <- matrix(NA, 2, 2)
    
    colnames(data) <- rownames(data) <- c("rel", "irrel")
    irrelCol <- which(!(colnames(tmp) %in% relevant))
    relCol <- which(colnames(tmp) %in% relevant)
    
    data[1, 1] <- sum(tmp[relCol, relCol])
    data[1, 2] <- sum(tmp[relCol, irrelCol])
    data[2, 1] <- sum(tmp[irrelCol, relCol])      
    data[2, 2] <- sum(tmp[irrelCol, irrelCol])
    data <- as.table(data)
    relevant <- "rel"
    rm(tmp)
  }
  numer <- data[relevant, relevant]
  denom <- sum(data[, relevant])
  rec <- ifelse(denom > 0, numer / denom, NA)  
  rec
}

recall.default <- function(data, reference, relevant = levels(reference)[1], 
                           na.rm = TRUE, ...) {
  if (!is.factor(reference) | !is.factor(data)) 
    stop("input data must be a factor")
  if (length(unique(c(levels(reference), levels(data)))) != 2) 
    stop("input data must have the same two levels")
  if (na.rm) {
    cc <- complete.cases(data) & complete.cases(reference)
    if (any(!cc)) {
      data <- data[cc]
      reference <- reference[cc]
    }
  }
  xtab <- table(data, reference)
  recall.table(xtab, relevant = relevant)
}

precision <- function(data, ...) UseMethod("precision")

precision.default <- function(data, reference, relevant = levels(reference)[1], 
                              na.rm = TRUE, ...) {
  if (!is.factor(reference) | !is.factor(data)) 
    stop("input data must be a factor")
  if (length(unique(c(levels(reference), levels(data)))) != 2) 
    stop("input data must have the same two levels")
  if (na.rm) {
    cc <- complete.cases(data) & complete.cases(reference)
    if (any(!cc)) {
      data <- data[cc]
      reference <- reference[cc]
    }
  }
  xtab <- table(data, reference)
  precision.table(xtab, relevant = relevant)
}

precision.table <- function (data, relevant = rownames(data)[1], ...) {
  if (!all.equal(nrow(data), ncol(data))) 
    stop("the table must have nrow = ncol")
  if (!all.equal(rownames(data), colnames(data))) 
    stop("the table must the same groups in the same order")
  if (nrow(data) > 2) {
    tmp <- data
    data <- matrix(NA, 2, 2)
    colnames(data) <- rownames(data) <- c("rel", "irrel")
    irrelCol <- which(!(colnames(tmp) %in% relevant))
    relCol <- which(colnames(tmp) %in% relevant)
    data[1, 1] <- sum(tmp[relCol, relCol])
    data[1, 2] <- sum(tmp[relCol, irrelCol])
    data[2, 1] <- sum(tmp[irrelCol, relCol])
    data[2, 2] <- sum(tmp[irrelCol, irrelCol])
    data <- as.table(data)
    relevant <- "rel"
    relevant
    rm(tmp)
  }
  numer <- data[relevant, relevant]
  denom <- sum(data[relevant, ])
  spec <- ifelse(denom > 0, numer/denom, NA)
  spec
}

F_meas <- function(data, ...) UseMethod("F_meas")

F_meas.default <- function(data, reference, relevant = levels(reference)[1], 
                           beta = 1,  na.rm = TRUE, ...) {
  if (!is.factor(reference) | !is.factor(data)) 
    stop("input data must be a factor")
  if (length(unique(c(levels(reference), levels(data)))) != 2) 
    stop("input data must have the same two levels")
  if (na.rm) {
    cc <- complete.cases(data) & complete.cases(reference)
    if (any(!cc)) {
      data <- data[cc]
      reference <- reference[cc]
    }
  }
  xtab <- table(data, reference)
  F_meas.table(xtab, relevant = relevant, beta = beta)
}

F_meas.table <- function (data, relevant = rownames(data)[1], beta = 1, ...) {
  prec <- precision.table(data, relevant = relevant)
  rec <- recall.table(data, relevant = relevant)
  (1+beta^2)*prec*rec/((beta^2 * prec)+rec)
}

prSummary <- function (data, lev = NULL, model = NULL)  {
  
  requireNamespaceQuietStop("MLmetrics")
  if (length(levels(data$obs)) > 2) 
    stop(paste("Your outcome has", length(levels(data$obs)), 
               "levels. The prSummary() function isn't appropriate."))
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")

  c(AUC = MLmetrics::PRAUC(y_pred = data[, lev[1]], y_true = ifelse(data$obs == lev[1], 1, 0)),
    Precision = precision.default(data = data$pred, reference = data$obs, relevant = lev[1]),
    Recall = recall.default(data = data$pred, reference = data$obs, relevant = lev[1]),
    F = F_meas.default(data = data$pred, reference = data$obs, relevant = lev[1]))
}

