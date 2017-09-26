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



#' Print Method for the train Class
#'
#' Print the results of a \code{\link{train}} object.
#'
#' The table of complexity parameters used, their resampled performance and a
#' flag for which rows are optimal.
#'
#' @param x an object of class \code{\link{train}}.
#' @param printCall a logical to print the call at the top of the output
#' @param details a logical to show print or summary methods for the final
#' model. In some cases (such as \code{gbm}, \code{knn}, \code{lvq}, naive
#' Bayes and bagged tree models), no information will be printed even if
#' \code{details = TRUE}
#' @param selectCol a logical whether to add a column with a star next to the
#' selected parameters
#' @param showSD a logical whether to show the standard deviation of the
#' resampling results within parentheses (e.g. "4.24 (0.493)")
#' @param \dots options passed to \code{\link[base]{format}}
#' @return A matrix with the complexity parameters and performance (invisibly).
#' @author Max Kuhn
#' @seealso \code{\link{train}}
#' @keywords print
#' @method print train
#' @export  
#' @examples
#'
#' \dontrun{
#' data(iris)
#' TrainData <- iris[,1:4]
#' TrainClasses <- iris[,5]
#'
#' options(digits = 3)
#'
#' library(klaR)
#' rdaFit <- train(TrainData, TrainClasses, method = "rda",
#'                 control = trainControl(method = "cv"))
#' rdaFit
#' print(rdaFit, showSD = TRUE)
#' }
#'
#' @export print.train

"print.train" <-
  function(x,
           printCall = FALSE,
           details = FALSE,
           selectCol = FALSE,
           showSD = FALSE,
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
      cat(chDim[1], " samples", sep = "")
      if(!is.null(x$control$indexFinal))
        cat(",", length(x$control$indexFinal), "used for final model\n") else
          cat("\n")
      cat(chDim[2],
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
      pp_list(x$preProc$method)
    } else {
      if(inherits(x, "train.recipe")) {
        step_names <- function(x) gsub("^step_", "", class(x)[1])
        steps_used <- unlist(lapply(x$recipe$steps, step_names))
        ppText <- paste("Recipe steps:", paste(steps_used, collapse = ", "))
        cat(truncateText(ppText), "\n")
      } else cat("No pre-processing\n")
    }

    if(!is.null(x$control$index)) {
      resampleN <- unlist(lapply(x$control$index, length))
      numResamp <- length(resampleN)

      resampText <- resampName(x)

      cat("Resampling:", resampText, "\n")
      if(x$control$method != "none") {
        outLabel <- x$metric

        resampleN <- as.character(resampleN)
        if(numResamp > 5) resampleN <- c(resampleN[1:6], "...")
        cat("Summary of sample sizes:", paste(resampleN, collapse = ", "), "\n")
      }
    }
    if(!is.null(x$control$sampling)) {
      cat("Addtional sampling using ")
      cat(switch(x$control$sampling$name,
                 down = "down-sampling",
                 up = "up-sampling",
                 smote = "SMOTE",
                 rose = "ROSE",
                 custom = "a custom function"))
      if(!is.null(x$preProc)) {
        if(x$control$sampling$first)
          cat(" prior to pre-processing") else
            cat(" after to pre-processing")
      }
      cat("\n\n")
    }

    if(x$control$method != "none") {

      tuneAcc <- x$results

      tuneAcc <- tuneAcc[, names(tuneAcc) != "parameter"]

      cat("Resampling results")
      if(dim(tuneAcc)[1] > 1) cat(" across tuning parameters")
      if(showSD) cat(" (values below are 'mean (sd)')")
      cat(":\n\n")

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

        ## See https://stat.ethz.ch/pipermail/r-help/2016-July/440230.html
        if(any(names(tuneAcc) %in% "method"))
          names(tuneAcc)[names(tuneAcc) %in% "method"] <- ".method"
        if(any(names(finalTune) %in% "method"))
          names(finalTune)[names(finalTune) %in% "method"] <- ".method"

        tuneAcc <- merge(tuneAcc, finalTune, all.x = TRUE)

        if(any(names(tuneAcc) %in% ".method"))
          names(tuneAcc)[names(tuneAcc) %in% ".method"] <- "method"

        tuneAcc$Selected[is.na(tuneAcc$Selected)] <- ""

      } else optString <- ""

      sdCols <- grep("SD$", colnames(tuneAcc))
      if(showSD) {
        sdCheck <- unlist(lapply(tuneAcc[, sdCols, drop = FALSE],
                                 function(u) all(is.na(u))))
        if(any(sdCheck)) {
          rmCols <- names(sdCheck)[sdCheck]
          tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% rmCols)]
        }
      } else {
        if(length(sdCols) > 0) tuneAcc <- tuneAcc[, -sdCols, drop = FALSE]
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

      tuneAcc <- tuneAcc[,!grepl("Apparent$|Optimism$", names(tuneAcc)), drop = FALSE]
      colnames(tuneAcc)[colnames(tuneAcc) == ".B"] <- "Resamples"
      nms <- names(tuneAcc)[names(tuneAcc) %in% params]
      sort_args <- vector(mode = "list", length = length(nms))
      for(i in seq(along = nms)) {
        sort_args[[i]] <- tuneAcc[, nms[i]]
      }
      tune_ord <- do.call("order", sort_args)
      if(!is.null(tune_ord)) tuneAcc <- tuneAcc[tune_ord,,drop = FALSE]

      theDots <- list(...)
      theDots$x <- tuneAcc
      #       if(!(any(names(theDots) == "digits"))) theDots$digits <- min(3, getOption("digits"))
      printMat <- do.call("format.data.frame", theDots)
      printMat <- as.matrix(printMat)
      rownames(printMat) <- rep("", dim(printMat)[1])

      if(showSD){
        sdCols <- grep("SD$", colnames(printMat), value = TRUE)
        sd_dat <- printMat[, sdCols, drop = FALSE]
        printMat <- printMat[, !(colnames(printMat) %in% sdCols), drop = FALSE]
        for(col_name in sdCols) {
          not_sd <- gsub("SD$", "", col_name)
          if(any(colnames(printMat) == not_sd)) {
            printMat[, not_sd] <- paste0(printMat[, not_sd], " (",
                                         sd_dat[, col_name], ")")
          }
        }
      }
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
                                  "the",
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

      cat(truncateText(optString))
      if(nzchar(optString)) cat("\n")
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


pp_list <- function(x) {
  if(is.list(x)) {
    pp <- unlist(lapply(x, length))
    pp <- pp[pp > 0]
    if(length(pp) > 0) {
      names(pp) <- gsub("BoxCox", "Box-Cox transformation", names(pp))
      names(pp) <- gsub("YeoJohnson", "Yeo-Johnson transformation", names(pp))
      names(pp) <- gsub("expoTrans", "exponential transformation", names(pp))
      names(pp) <- gsub("scale", "scaled", names(pp))
      names(pp) <- gsub("center", "centered", names(pp))
      names(pp) <- gsub("pca", "principal component signal extraction", names(pp))
      names(pp) <- gsub("ica", "independent component signal extraction", names(pp))
      names(pp) <- gsub("spatialSign", "spatial sign transformation", names(pp))
      names(pp) <- gsub("knnImpute", "nearest neighbor imputation", names(pp))
      names(pp) <- gsub("bagImpute", "bagged tree imputation", names(pp))
      names(pp) <- gsub("medianImpute", "median imputation", names(pp))
      names(pp) <- gsub("range", "re-scaling to [0, 1]", names(pp))
    } else pp <- "None"
    ppText <- paste("Pre-processing:", paste0(names(pp),  " (", pp, ")", collapse = ", "))
    cat(truncateText(ppText), "\n")
  } else {
    pp <- x
    pp <- gsub("BoxCox", "Box-Cox transformation", pp)
    pp <- gsub("YeoJohnson", "Yeo-Johnson transformation", pp)
    pp <- gsub("expoTrans", "exponential transformation", pp)
    pp <- gsub("scale", "scaled", pp)
    pp <- gsub("center", "centered", pp)
    pp <- gsub("pca", "principal component signal extraction", pp)
    pp <- gsub("ica", "independent component signal extraction", pp)
    pp <- gsub("spatialSign", "spatial sign transformation", pp)
    pp <- gsub("knnImpute", "nearest neighbor imputation", pp)
    pp <- gsub("bagImpute", "bagged tree imputation", pp)
    pp <- gsub("medianImpute", "median imputation", pp)
    pp <- gsub("range", "re-scaling to [0, 1]", pp)

    if(length(pp) == 0) pp <- "None"

    ppText <- paste("Pre-processing:", paste(pp, collapse = ", "))
    cat(truncateText(ppText), "\n")
  }
  invisible(NULL)
}
