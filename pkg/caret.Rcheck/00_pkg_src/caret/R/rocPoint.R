rocPoint <- function(cutoff, x, y, positive)
{
  warning("This function is deprecated a of 1/3/12. The computations now utilize the pROC package. This function will be removed in a few releases.")
  classLevels <- levels(y)
  negative <- classLevels[positive != classLevels]
  newClass <- factor(
                     ifelse(
                            x <= cutoff, 
                            negative,
                            positive), 
                     levels = classLevels)
  out <- c(
           cutoff,
           sensitivity(newClass, y, positive), 
           specificity(newClass, y, negative))
  names(out) <- c("cutoff", "sensitivity", "specificity")
  out
}

