basic2x2Stats <- function(x, y, pos, neg)
{
   out <- vector(length = 4, mode = "numeric")
   out[1] <- sensitivity(x, y, pos)
   out[2] <- specificity(x, y, neg)
   out[3] <- posPredValue(x, y, pos)
   out[4] <- negPredValue(x, y, neg)  
   names(out) <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value")
   out
}

