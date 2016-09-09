#' Print method for confusionMatrix
#' 
#' a print method for \code{confusionMatrix}
#' 
#' 
#' @param x an object of class \code{confusionMatrix}
#' @param mode a single character string either "sens_spec", "prec_recall", or
#' "everything"
#' @param digits number of significant digits when printed
#' @param printStats a logical: if \code{TRUE} then table statistics are also
#' printed
#' @param \dots optional arguments to pass to \code{print.table}
#' @return \code{x} is invisibly returned
#' @author Max Kuhn
#' @seealso \code{\link{confusionMatrix}}
#' @keywords utilities
print.confusionMatrix <- function(x, mode = x$mode, digits = max(3, getOption("digits") - 3), printStats = TRUE, ...){
  if(is.null(mode)) mode <- "sens_spec"
  if(!(mode %in% c("sens_spec", "prec_recall", "everything")))
    stop("`mode` should be either 'sens_spec', 'prec_recall', or 'everything'")
  cat("Confusion Matrix and Statistics\n\n") 
  print(x$table, ...)
  
  if(printStats) {
    tmp <- round(x$overall, digits = digits)
    pIndex <- grep("PValue", names(x$overall))
    tmp[pIndex] <- format.pval(x$overall[pIndex], digits = digits)
    overall <- tmp
    
    accCI <- paste("(",
                   paste(
                     overall[ c("AccuracyLower", "AccuracyUpper")],
                     collapse = ", "),
                   ")",
                   sep = "")      
    
    overallText <- c(paste(overall["Accuracy"]),
                     accCI,
                     paste(overall[c("AccuracyNull", "AccuracyPValue")]),
                     "",
                     paste(overall["Kappa"]),
                     paste(overall["McnemarPValue"]))
    
    overallNames <- c("Accuracy", "95% CI",
                      "No Information Rate",
                      "P-Value [Acc > NIR]",
                      "",
                      "Kappa",
                      "Mcnemar's Test P-Value")
    
    if(dim(x$table)[1] > 2){
      cat("\nOverall Statistics\n")
      overallNames <- ifelse(overallNames == "",
                             "",
                             paste(overallNames, ":"))
      out <- cbind(format(overallNames, justify = "right"), overallText)
      colnames(out) <- rep("", ncol(out))
      rownames(out) <- rep("", nrow(out))
      
      print(out, quote = FALSE)
      
      cat("\nStatistics by Class:\n\n")
      if(mode == "prec_recall")
        x$byClass <- x$byClass[,!grepl("(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)", 
                                     colnames(x$byClass))]
      if(mode == "sens_spec")
        x$byClass <- x$byClass[,!grepl("(Precision)|(Recall)|(F1)", colnames(x$byClass))]      
      print(t(x$byClass), digits = digits)
      
    } else {
      if(mode == "prec_recall")
        x$byClass <- x$byClass[!grepl("(Sensitivity)|(Specificity)|(Pos Pred Value)|(Neg Pred Value)", 
                                       names(x$byClass))]
      if(mode == "sens_spec")
        x$byClass <- x$byClass[!grepl("(Precision)|(Recall)|(F1)", names(x$byClass))]  
      
      overallText <- c(overallText,
                       "",
                       format(x$byClass, digits = digits))
      overallNames <- c(overallNames, "", names(x$byClass))
      overallNames <- ifelse(overallNames == "", "", paste(overallNames, ":"))
      
      overallNames <- c(overallNames, "", "'Positive' Class :")
      overallText <- c(overallText, "", x$positive)
      
      out <- cbind(format(overallNames, justify = "right"), overallText)
      colnames(out) <- rep("", ncol(out))
      rownames(out) <- rep("", nrow(out))
      
      out <- rbind(out, rep("", 2))
      
      print(out, quote = FALSE)
    }
  }
  invisible(x)   
}
