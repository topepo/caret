plotClassProbs <- function(object,
                           plotType = "histogram",
                           useObjects = FALSE,
                           ...)
{
  obsLevels <- levels(object$obs)


  stackProbs <- melt(object, id.vars = c("obs", "model", "object", "dataType"),
                     measure.vars = if(length(obsLevels) == 2) obsLevels[1] else obsLevels)
  names(stackProbs)[names(stackProbs) == "variable"] <- "Class"
  names(stackProbs)[names(stackProbs) == "value"] <- "Probability"
  names(stackProbs)[names(stackProbs) == "obs"] <- "Observed"
  stackProbs$Observed <- paste("Data:", as.character(stackProbs$Observed))
  stackProbs$Class <- paste("Prob:", as.character(stackProbs$Class))
  
  keepVars <- "Observed"
  if(length(unique(stackProbs$dataType)) > 1) keepVars <- c(keepVars, "dataType")
  if(length(unique(stackProbs$model)) > 1) keepVars <- c(keepVars, "model")     

  if(any(names(object) == "object") & useObjects)
    {
      if(length(unique(stackProbs$object)) > 1) keepVars <- c(keepVars, "object")
    }

  if(plotType == "histogram")
    {
      form <- if(length(obsLevels) == 2)
        {
          form <- if(length(keepVars) > 0) paste("~ Probability|", paste(keepVars, collapse = "*")) else "~ Probability"
          form <- as.formula(form)
          out <- histogram(form,
                           data = stackProbs,
                           xlab = paste("Probability of", obsLevels[1]),
                           ...)
          
        } else {
          form <- if(length(keepVars) > 0) paste("~ Probability|Class*", paste(keepVars, collapse = "*")) else "~ Probability|Class"
                    form <- as.formula(form)
          out <- histogram(form,
                           data = stackProbs,
                           ...)
        }
      

      
    } else {
      keepVars <- keepVars[keepVars != "Observed"]
      form  <- if(length(keepVars) > 0) paste("~ Probability|", paste(keepVars, collapse = "*")) else "~ Probability"
      form <- as.formula(form)

      out <- densityplot(form, data = stackProbs, groups = Observed, ...)
      
    }
  out
}

