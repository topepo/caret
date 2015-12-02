print.confusionMatrix <- function(x, digits = max(3, getOption("digits") - 3), printStats = TRUE, ...)
{
   cat("Confusion Matrix and Statistics\n\n") 
   print(x$table, ...)
      
#   cat("\n(columns are reference results, rows are predictions)\n")

   if(printStats)
   {
 
      tmp <- round(x$overall, digits = digits)
      pIndex <- grep("PValue", names(x$overall))
      tmp[pIndex] <- format.pval(x$overall[pIndex], digits = digits)
      overall <- tmp
        
      accCI <- paste(
                     "(",
                     paste(
                           overall[ c("AccuracyLower", "AccuracyUpper")],
                           collapse = ", "),
                     ")",
                     sep = "")      
   
      overallText <- c(
                       paste(overall["Accuracy"]),
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
                        
      if(dim(x$table)[1] > 2)
      {
         cat("\nOverall Statistics\n")
         overallNames <- ifelse(
                                overallNames == "",
                                "",
                                paste(overallNames, ":"))
         out <- cbind(
                      format(overallNames, justify = "right"),
                      overallText)
         colnames(out) <- rep("", ncol(out))
         rownames(out) <- rep("", nrow(out))
         
         print(out, quote = FALSE)
         
         cat("\nStatistics by Class:\n\n")
         print(t(x$byClass), digits = digits)
         
      } else {

         overallText <- c(
                      overallText,
                      "",
                      format(x$byClass, digits = digits))
         overallNames <- c(
                           overallNames,
                           "",
                           names(x$byClass))
         overallNames <- ifelse(
                                overallNames == "",
                                "",
                                paste(overallNames, ":"))

         overallNames <- c(overallNames,
                           "",
                           "'Positive' Class :")
         overallText <- c(overallText,
                          "",
                          x$positive)
         
         out <- cbind(
                      format(overallNames, justify = "right"),
                      overallText)
         colnames(out) <- rep("", ncol(out))
         rownames(out) <- rep("", nrow(out))


         out <- rbind(out, rep("", 2))
         
         print(out, quote = FALSE)

      }

        
   }
   invisible(x)   
}
