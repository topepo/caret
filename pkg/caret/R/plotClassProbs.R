#' Plot Predicted Probabilities in Classification Models
#' 
#' This function takes an object (preferably from the function
#' \code{\link{extractProb}}) and creates a lattice plot.
#' 
#' If the call to \code{\link{extractProb}} included test data, these data are
#' shown, but if unknowns were also included, these are not plotted
#' 
#' 
#' @param object an object (preferably from the function
#' \code{\link{extractProb}}. There should be columns for each level of the
#' class factor and columns named \code{obs}, \code{pred}, \code{model} (e.g.
#' "rpart", "nnet" etc), \code{dataType} (e.g. "Training", "Test" etc) and
#' optionally \code{objects} (for giving names to objects with the same model
#' type).
#' @param plotType either "histogram" or "densityplot"
#' @param useObjects a logical; should the object name (if any) be used as a
#' conditioning variable?
#' @param \dots parameters to pass to \code{\link[lattice]{histogram}} or
#' \code{\link[lattice]{densityplot}}
#' @return A lattice object. Note that the plot has to be printed to be
#' displayed (especially in a loop).
#' @author Max Kuhn
#' @keywords hplot
#' @examples
#' 
#' \dontrun{
#' data(mdrr)
#' set.seed(90)
#' inTrain <- createDataPartition(mdrrClass, p = .5)[[1]]
#' 
#' trainData <- mdrrDescr[inTrain,1:20]
#' testData <- mdrrDescr[-inTrain,1:20]
#' 
#' trainY <- mdrrClass[inTrain]
#' testY <- mdrrClass[-inTrain]
#' 
#' ctrl <- trainControl(method = "cv")
#' 
#' nbFit1 <- train(trainData, trainY, "nb",
#'                 trControl = ctrl,
#'                 tuneGrid = data.frame(usekernel = TRUE, fL = 0))
#' 
#' nbFit2 <- train(trainData, trainY, "nb",
#'                 trControl = ctrl,
#'                 tuneGrid = data.frame(usekernel = FALSE, fL = 0))
#' 
#' 
#' models <- list(para = nbFit2, nonpara = nbFit1)
#' 
#' predProbs <- extractProb(models, testX = testData,  testY = testY)
#' 
#' plotClassProbs(predProbs, useObjects = TRUE)
#' plotClassProbs(predProbs,
#'                subset = object == "para" & dataType == "Test")
#' plotClassProbs(predProbs,
#'                useObjects = TRUE,
#'                plotType = "densityplot",
#'                auto.key = list(columns = 2))
#' }
#' 
#' 
#' 
#' @export plotClassProbs
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

