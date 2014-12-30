stringFunc <- function (x)  {
  if (!is.character(x)) x <- format(x)
  numElements <- length(x)
  out <- if (length(x) > 0) {
    switch(min(numElements, 3), x, paste(x, collapse = " and "), {
      x <- paste0(x, c(rep(",", numElements - 2), " and", ""))
      paste(x, collapse = " ")
    })
  } else ""
  out
} 

"print.train" <-
  function(x,
           printCall = FALSE,
           details = FALSE,
           selectCol = FALSE,
           ...) {
  
    if(!is.null(x$modelInfo$label)) cat(x$modelInfo$label, "\n\n")
    if(printCall) printCall(x$call)
    
    if(!is.null(x$trainingData)) {
      chDim <- dim(x$trainingData)
      chDim[2] <- chDim[2] - 1
      if(x$modelType == "Classification") {
        lev <- levels(x)
        if(is.character(lev)) chDim <- c(chDim, length(lev))
      } else lev <- NULL      
      chDim <- format(chDim)
      cat(chDim[1], 
          " samples\n", 
          chDim[2],
          " predictor", ifelse(chDim[2] > 1, "s\n", "\n"),
          sep = "")
      if(is.character(lev)){
        cat(chDim[3], 
            "classes:",
            paste("'", lev, "'", sep = "", collapse = ", "),
            "\n")
      }
      cat("\n")
    }
    
    if(!is.null(x$preProc)){
      ## Make things look a little nicer:
      pp <- x$preProc$method
      pp <- gsub("BoxCox", "Box-Cox transformation", pp)  
      pp <- gsub("YeoJohnson", "Yeo-Johnson transformation", pp)  
      pp <- gsub("expoTrans", "exponential transformation", pp) 
      pp <- gsub("scale", "scaled", pp)
      pp <- gsub("center", "centered", pp)
      pp <- gsub("pca", "principal component signal extraction", pp)
      pp <- gsub("ica", "independent component signal extraction", pp)
      pp <- gsub("spatialSign", "spatial sign transformation", pp)
      pp <- gsub("knnImpute", paste(x$k, "nearest neighbor imputation"), pp)
      pp <- gsub("bagImpute", "bagged tree imputation", pp)
      pp <- gsub("medianImpute", "median imputation", pp)
      pp <- gsub("range", "re-scaling to [0, 1]", pp)  
      
      if(length(pp) == 0) pp <- "None"
      
      ppText <- paste("Pre-processing:", paste(pp, collapse = ", "))
      cat(truncateText(ppText), "\n")
    } else cat("No pre-processing\n")
    
    if(!is.null(x$control$index)) {
      resampleN <- unlist(lapply(x$control$index, length))
      numResamp <- length(resampleN)
      
      resampText <- resampName(x)
      
      cat("Resampling:", resampText, "\n\n")   
      if(x$control$method != "none") {
        outLabel <- x$metric
        
        resampleN <- as.character(resampleN)
        if(numResamp > 5) resampleN <- c(resampleN[1:6], "...")
        cat("Summary of sample sizes:", paste(resampleN, collapse = ", "), "\n\n")
      }
    }
    
    if(x$control$method != "none") {
      
      tuneAcc <- x$results 
      tuneAcc <- tuneAcc[, names(tuneAcc) != "parameter"]
      
      cat("Resampling results")
      if(dim(tuneAcc)[1] > 1) cat(" across tuning parameters:\n") else cat("\n")
      cat("\n")
      
      if(dim(tuneAcc)[1] > 1) {
        numParam <- length(x$bestTune)
        
        finalTune <- x$bestTune
        
        optValues <- paste(names(finalTune), "=", format(finalTune, ...))
        optString <- paste0("The final ",
                            ifelse(numParam > 1, "values", "value"),
                            " used for the model ",
                            ifelse(numParam > 1, "were ", "was "),
                            stringFunc(optValues),
                            ".")
        
        
        finalTune$Selected <- "*"
        
        tuneAcc <- merge(tuneAcc, finalTune, all.x = TRUE)
        tuneAcc$Selected[is.na(tuneAcc$Selected)] <- ""
        
      } else optString <- ""
      
      sdCols <- grep("SD$", colnames(tuneAcc))
      sdCheck <- unlist(lapply(tuneAcc[, sdCols, drop = FALSE],
                               function(u) all(is.na(u))))
      if(any(sdCheck)) {
        rmCols <- names(sdCheck)[sdCheck]
        tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% rmCols)]  
      }
      
      params <- names(x$bestTune)
      
      if(!all(params == "parameter")){
        numVals <- apply(tuneAcc[, params, drop = FALSE], 2, function(x) length(unique(x)))
        if(any(numVals < 2)) {
          constString <- NULL
          for(i in seq(along = numVals)) {
            if(numVals[i] == 1)
              constString <- c(constString,
                               paste0("Tuning parameter '",
                                      names(numVals)[i],
                                      "' was held constant at a value of ",
                                      stringFunc(tuneAcc[1,names(numVals)[i]])))
          }
          discard <- names(numVals)[which(numVals == 1)]
          tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% discard), drop = FALSE]
          
        } else constString <- NULL
      } else constString <- NULL
      
      tuneAcc <- tuneAcc[,!grepl("Apparent$", names(tuneAcc)),drop = FALSE]
      
      theDots <- list(...)
      theDots$x <- tuneAcc
#       if(!(any(names(theDots) == "digits"))) theDots$digits <- min(3, getOption("digits"))
      printMat <- do.call("format.data.frame", theDots)
      printMat <- as.matrix(printMat)
      rownames(printMat) <- rep("", dim(printMat)[1])
      colnames(printMat) <- gsub("SD", " SD", colnames(printMat))
      
      if(!selectCol) printMat <- printMat[, colnames(printMat) != "Selected", drop = FALSE]
      
      print(printMat, quote = FALSE, print.gap = 2)
      cat("\n")
      
      if(!is.null(constString)){
        cat(truncateText(paste(constString, collapse = "\n")))
        cat("\n")
      }
      
      
      if(dim(tuneAcc)[1] > 1) {
        if(is.null(x$update)) {
          met <- paste(x$metric, "was used to select the optimal model using")
          if(is.function(x$control$selectionFunction)) {
            met <- paste(met, " a custom selection rule.\n")
          } else {
            
            met <- paste(met,
                         switch(x$control$selectionFunction,
                                best = paste(
                                  " the",
                                  ifelse(x$maximize, "largest", "smallest"),
                                  "value.\n"),
                                oneSE = " the one SE rule.\n",
                                tolerance = " a tolerance rule.\n"))
          }
        } else {
          met <- paste("The tuning", ifelse(ncol(x$bestTune) > 1, "parameters", "parameter"),
                       "was set manually.\n") 
          
        }
        cat(truncateText(met))
      }
      
      cat(truncateText(optString), "\n")
    } else printMat <- NULL
    
    if(details) {
      if(!(x$method %in% c("gbm", "treebag", "nb", "lvq", "knn"))) {
        cat("\n----------------------------------------------------------\n")
        cat("\nThe final model:\n\n")
        switch(x$method,
               lm =, nnet =, multinom =, pls =, earth =,
               lmStepAIC =,
               bagEarth =, bagFDA = print(summary(x$finalModel)),
               rpart =, ctree =, ctree2=, cforest =,
               glmboost =, gamboost =, blackboost =,
               ada =, randomForest =, pcaNNet =,
               svmradial =, svmpoly =,
               svmRadial =, svmPoly =,
               rvmRadial =, rvmPoly =,
               lssvmRadial =, lssvmPoly =,
               gaussprRadial =, gaussprPoly =,
               enet =, lasso =, LMT =, JRip =,
               lda =, rda =, pamr =, gpls =, J48 =,
               ppr = print(x$finalModel),
               fda =  {
                 print(x$finalModel)
                 cat("\n Summary of Terms\n\n")
                 print(x$finalModel$fit)
                 
               })
      }
    }
    invisible(printMat)
  }


truncateText <- function(x){
  if(length(x) > 1) x <- paste(x, collapse = "")
  w <- options("width")$width
  if(nchar(x) <= w) return(x)
  
  cont <- TRUE
  out <- x
  while(cont){
    tmp <- out[length(out)]
    tmp2 <- substring(tmp, 1, w)
    
    spaceIndex <- gregexpr("[[:space:]]", tmp2)[[1]]
    stopIndex <- spaceIndex[length(spaceIndex) - 1] - 1
    tmp <- c(substring(tmp2, 1, stopIndex),
             substring(tmp, stopIndex + 1))
    out <- if(length(out) == 1) tmp else c(out[1:(length(x)-1)], tmp)
    if(all(nchar(out) <= w)) cont <- FALSE
  }
  
  paste(out, collapse = "\n")
}
