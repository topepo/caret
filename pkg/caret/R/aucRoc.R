aucRoc <- function(object)
{
    warning("This function is deprecated a of 1/3/12. The computations now utilize the pROC package. This function will be removed in a few releases.")

   sens <- object[, "sensitivity"]
   omspec <- 1 - object[, "specificity"]
   newOrder <- order(omspec)
   sens <- sens[newOrder]
   omspec <- omspec[newOrder]
   
   rocArea <- sum(.5 *diff(omspec) * (sens[-1] + sens[-length(sens)]))
   rocArea <- max(rocArea, 1 - rocArea)
   rocArea
}

