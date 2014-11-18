sensitivity <- 
  function(data, ...){
    UseMethod("sensitivity")
  }

"sensitivity.default" <-
  function(data, reference, positive = levels(reference)[1], na.rm = TRUE, ...)
{
  if(!is.factor(reference) | !is.factor(data)) 
    stop("inputs must be factors")

  ## todo: relax the =2 constraint and let ngative length be > 2
  if(length(unique(c(levels(reference), levels(data)))) != 2)
    stop("input data must have the same two levels")
  if(na.rm)
    {
      cc <- complete.cases(data) & complete.cases(reference)
      if(any(!cc))
        {
          data <- data[cc]
          reference <- reference[cc]
        }
    }
  numer <- sum(data %in% positive & reference %in% positive)
  denom <- sum(reference %in% positive)
  sens <- ifelse(denom > 0, numer / denom, NA)
  sens
}

"sensitivity.table" <-
  function(data, positive = rownames(data)[1], ...)
{
  ## "truth" in columns, predictions in rows
  if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
  if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")

  if(nrow(data) > 2)
    {
      tmp <- data
      data <- matrix(NA, 2, 2)
      
      colnames(data) <- rownames(data) <- c("pos", "neg")
      posCol <- which(colnames(tmp) %in% positive)
      negCol <- which(!(colnames(tmp) %in% positive))
      
      data[1, 1] <- sum(tmp[posCol, posCol])
      data[1, 2] <- sum(tmp[posCol, negCol])
      data[2, 1] <- sum(tmp[negCol, posCol])      
      data[2, 2] <- sum(tmp[negCol, negCol])
      data <- as.table(data)
      positive <- "pos"
      rm(tmp)
    }

  numer <- sum(data[positive, positive])
  denom <- sum(data[, positive])
  sens <- ifelse(denom > 0, numer / denom, NA)
  sens
}

"sensitivity.matrix" <-
  function(data, positive = rownames(data)[1], ...)
{
  data <- as.table(data)
  sensitivity.table(data)
}
