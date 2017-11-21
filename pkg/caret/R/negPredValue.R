#' @rdname sensitivity
#' @export
negPredValue <- 
  function(data, ...){
    UseMethod("negPredValue")
  }

#' @rdname sensitivity
#' @export
"negPredValue.default" <-
function(data, reference, negative = levels(reference)[2], prevalence = NULL, ...)
{
   if(!is.factor(reference) | !is.factor(data)) 
      stop("input data must be a factor")
   
   if(length(unique(c(levels(reference), levels(data)))) != 2)
      stop("input data must have the same two levels")   

   lvls <- levels(data) 
   if(is.null(prevalence)) prevalence <- mean(reference == lvls[lvls != negative])
   sens <- sensitivity(data, reference, lvls[lvls != negative])
   spec <- specificity(data, reference, negative)
   (spec * (1-prevalence))/(((1-sens)*prevalence) + ((spec)*(1-prevalence)))
}

#' @rdname sensitivity
#' @export
"negPredValue.table" <-
  function(data, negative = rownames(data)[-1], prevalence = NULL, ...)
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

  positive <- colnames(data)[colnames(data) != negative]
  if(is.null(prevalence)) prevalence <- sum(data[, positive])/sum(data)
  
  sens <- sensitivity(data, positive)
  spec <- specificity(data, negative)
  (spec * (1-prevalence))/(((1-sens)*prevalence) + ((spec)*(1-prevalence)))

}

#' @rdname sensitivity
#' @export
"negPredValue.matrix" <-
  function(data, negative = rownames(data)[-1], prevalence = NULL, ...)
{
  data <- as.table(data)
  negPredValue.table(data, prevalence = prevalence)
}

