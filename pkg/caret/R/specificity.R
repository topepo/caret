#' @export
#' @rdname sensitivity
specificity <-
  function(data, ...){
    UseMethod("specificity")
  }

#' @importFrom stats complete.cases
#' @export
#' @rdname sensitivity
"specificity.default" <-
function(data, reference, negative = levels(reference)[-1], na.rm = TRUE, ...)
{
   if(!is.factor(reference) | !is.factor(data))
      stop("input data must be a factor")

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
   numer <- sum(data %in% negative & reference %in% negative)
   denom <- sum(reference %in% negative)
   spec <- ifelse(denom > 0, numer / denom, NA)
   spec
}

#' @export
#' @rdname sensitivity
"specificity.table" <-
  function(data, negative = rownames(data)[-1], ...)
{
  ## "truth" in columns, predictions in rows
  if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
  if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")

  if(nrow(data) > 2)
    {
      tmp <- data
      data <- matrix(NA, 2, 2)

      colnames(data) <- rownames(data) <- c("pos", "neg")
      negCol <- which(colnames(tmp) %in% negative)
      posCol <- which(!(colnames(tmp) %in% negative))

      data[1, 1] <- sum(tmp[posCol, posCol])
      data[1, 2] <- sum(tmp[posCol, negCol])
      data[2, 1] <- sum(tmp[negCol, posCol])
      data[2, 2] <- sum(tmp[negCol, negCol])
      data <- as.table(data)
      negative <- "neg"
      rm(tmp)
    }

  numer <- sum(data[negative, negative])
  denom <- sum(data[, negative])
  spec <- ifelse(denom > 0, numer / denom, NA)
  spec
}

"specificity.matrix" <-
  function(data, negative = rownames(data)[-1], ...)
{
  data <- as.table(data)
  specificity.table(data)
}
