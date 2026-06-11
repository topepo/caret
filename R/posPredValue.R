#' @rdname sensitivity
#' @export
posPredValue <- 
  function(data, ...){
    UseMethod("posPredValue")
  }

#' @rdname sensitivity
#' @export
"posPredValue.default" <-
  function(data, reference, positive = levels(reference)[1], prevalence = NULL, ...)
{
  if(!is.factor(reference) | !is.factor(data)) 
    stop("inputs must be factors")
  
  if(length(unique(c(levels(reference), levels(data)))) != 2)
    stop("input data must have the same two levels")
  
  lvls <- levels(data) 
  if(is.null(prevalence)) prevalence <- mean(reference == positive)
  sens <- sensitivity(data, reference, positive)
  spec <- specificity(data, reference, lvls[lvls != positive])
  (sens * prevalence)/((sens*prevalence) + ((1-spec)*(1-prevalence)))

}

#' @rdname sensitivity
#' @export
"posPredValue.table" <-
  function(data, positive = rownames(data)[1], prevalence = NULL, ...)
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

  negative <- colnames(data)[colnames(data) != positive]
  if(is.null(prevalence)) prevalence <- sum(data[, positive])/sum(data)
  
  sens <- sensitivity(data, positive)
  spec <- specificity(data, negative)
    (sens * prevalence)/((sens*prevalence) + ((1-spec)*(1-prevalence)))

}

#' @rdname sensitivity
#' @export
"posPredValue.matrix" <-
  function(data, positive = rownames(data)[1], prevalence = NULL, ...)
{
  data <- as.table(data)
  posPredValue.table(data, prevalence = prevalence)
}

