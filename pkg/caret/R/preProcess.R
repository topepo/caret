ppMethods <- c("BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "range", "knnImpute", "bagImpute", "pca", "ica", "spatialSign", "medianImpute")


preProcess <- function(x, ...) UseMethod("preProcess")

preProcess.default <- function(x, method = c("center", "scale"),
                               thresh = 0.95,
                               pcaComp = NULL,
                               na.remove = TRUE,
                               k = 5,
                               knnSummary = mean,
                               outcome = NULL,
                               fudge = .2,
                               numUnique = 3,
                               verbose = FALSE,
                               ...)
{

  ppMethods <- c("BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "range", "knnImpute", "bagImpute", "medianImpute", "pca", "ica", "spatialSign")
  if(any(!(method %in% ppMethods))) stop(paste("'method' should be one of:", paste(ppMethods, collapse = ", ")))
  if(is.null(method)) stop("NULL values of 'method' are not allowed")
  if(any(method %in% "range") & any(method %in% c("center", "scale", "BoxCox")))
    stop("centering, scaling and/or Box-Cox transformations are inconsistent with scaling to a range of [0, 1]")
  
  if(all(c("pca", "ica") %in% method))
    {
      warning("fastICA automatically uncorrelates the data using PCA. method = 'pca' is not needed")
      method <- method[method != "pca"]
    }

  if(any(method %in% c("pca", "ica", "knnImpute", "spatialSign")) & !(any(method == "scale"))) method  <- c(method, "scale")
  if(any(method %in% c("pca", "ica", "knnImpute", "spatialSign")) & !(any(method == "center"))) method  <- c(method, "center")
  
  if(sum(c("knnImpute","bagImpute", "medianImpute") %in% method) > 1)
    stop("please pick only one imputation method")
  
  method <- unique(method)
  
  ## the row.norm option in fastICA states: "logical value indicating whether rows
  ## of the data matrix X should be standardized beforehand." Basically, this means that
  ## we would center *and* scale before the ICA step, so let's adjust the "scale" method too
  if(any(method == "ica"))
    {
      theDots <- list(...)
      row.norm <- if(is.null(list(...)$row.norm)) FALSE else list(...)$row.norm
      if(row.norm & !(any(method == "scale"))) method  <- c(method, "scale")
    }
  
  if(is.matrix(x)) {
      if(!is.numeric(x)) stop("x must be numeric")
    }
  if(is.data.frame(x)) {
      isFactor <- unlist(lapply(x, is.factor))
      isChar <- unlist(lapply(x, is.character))
      if(any(isFactor | isChar)) stop("all columns of x must be numeric")        
  }
  if(!is.matrix(x) & !is.data.frame(x)) {
    msg <- paste("preProcess is only designed for simple numeric",
                 "matrices and data frames; your predictors have class(es): (",
                 paste("'", class(x), "'", sep = "", collapse = ", "),
                 ") and errors may occur")
    warning(msg)
  }

  
  theCall <- match.call(expand.dots = TRUE)


  
  if(any(method == "BoxCox"))
    {
      if(verbose) cat("Estimating Box-Cox transformations for the predictors...")
      bc <- lapply(x,
                   BoxCoxTrans,
                   fudge = fudge,
                   na.rm = na.remove,
                   numUnique = numUnique,
                   x = if(is.null(outcome)) rep(1, nrow(x)) else outcome)
      if(verbose) cat(" applying them to training data\n")
      ## Find a better way of doing this
      for(i in seq(along = bc)) x[,i] <- predict(bc[[i]], x[,i])
    } else bc <- NULL

  if(any(method == "YeoJohnson"))
    {

      yjWrap <- function(x, numUnique = numUnique)
        {
          if(length(unique(x)) >= numUnique)
          {
            out <- try(powerTransform(y ~ 1,
                                      data = data.frame(y = x[!is.na(x)]),
                                      family = "yjPower"),
                       silent = TRUE)
            if(class(out)[1] == "try-error") out <- NA
            } else out <- NA
          out
        }
      if(verbose) cat("Estimating Yeo-Johnson transformations for the predictors...")
      yj <- lapply(x, yjWrap, numUnique = numUnique)
      if(verbose) cat(" applying them to training data\n")
      ## Find a better way of doing this
      lam <- unlist(lapply(yj, function(x) if(class(x) == "powerTransform") x$lambda else NA))
      lam <- lam[!is.na(lam)]
      if(length(lam) > 0)
        {
          for(i in seq(along = lam))
            {
              who <-  gsub("\\.Y1$", "", names(lam)[i])
              x[,who] <- yjPower(x[,who], yj[[who]]$lambda)
            }
        }
    } else yj <- NULL
  
  if(any(method == "expoTrans"))
  {
    if(verbose) cat("Estimating exponential transformations for the predictors...")
    et <- lapply(x, expoTrans.default, numUnique = numUnique)
    if(verbose) cat(" applying them to training data\n")
    for(i in seq(et)) x[,i] <- predict(et[[i]], x[,i])
  } else et <- NULL  

  if(any(method  %in% c("center")))
    {
      if(verbose) cat("Calculating means for centering\n")
      centerValue <- apply(x, 2, mean, na.rm = na.remove) 
    } else centerValue <- NULL
  if(any(method %in% c("scale")))
    {
      if(verbose) cat("Calculating standard deviations for scaling\n")
      scaleValue <- apply(x, 2, sd, na.rm = na.remove)
    } else scaleValue <- NULL

  if(any(method == "range"))
    {
      ranges <- apply(x, 2, function(x) c(min(x, na.rm = na.remove), max(x, na.rm = na.remove)))
    } else ranges <- NULL

  
  if(any(scaleValue == 0))
    {
      warning(
              paste(
                    "These variables have zero variances:",
                    paste(
                          names(scaleValue)[which(scaleValue == 0)],
                          collapse = ", ")))
      scaleValue[which(scaleValue == 0)] <- 1
    }

  if(any(method == "bagImpute"))
    {
      if(verbose) cat("Computing bagging models for each predictor...")
      bagModels <- as.list(colnames(x))
      names(bagModels) <- colnames(x)
      bagModels <- lapply(bagModels,
                          bagImp,
                          x = x)
      if(verbose) cat(" done\n")
    } else bagModels <- NULL
  
  x <- x[complete.cases(x),,drop = FALSE]
  
  if (any(method == "medianImpute")) 
    {
    if(verbose) cat("Computing medians for each predictor...")
    median <- sapply(x, median, na.rm=TRUE)
    names(median) <- names(x)
    if(verbose) cat(" done\n")
  }
  
  if(any(method == "pca"))
    {
      if(verbose) cat("Computing PCA loadings\n")
      tmp <- prcomp(x, scale = TRUE, retx = FALSE)
      if(is.null(pcaComp))
        {
          cumVar <- cumsum(tmp$sdev^2/sum(tmp$sdev^2)) 
          numComp <- max(2, which.max(cumVar > thresh))
        } else numComp <- pcaComp
      rot <- tmp$rotation[,1:numComp]
    } else {
      rot <- NULL
      numComp <- NULL
    }

  if(any(method == "ica"))
    {
      if(verbose) cat("Computing ICA loadings\n")
      library(fastICA)
      x <- sweep(x, 2, centerValue, "-")
      if(!row.norm & any(method == "scale")) x <- sweep(x, 2, scaleValue, "/")      
      tmp <- fastICA(x, ...)
      ica <- list(
                  ## S = tmp$S, ## was used for debugging
                  row.norm = row.norm,
                  K = tmp$K,
                  W = tmp$W)
    } else {
      ica <- NULL
    }


  
  out <- list(call = theCall,
              dim = dim(x),
              bc = bc,
              yj = yj,
              et = et,
              mean = centerValue,
              std = scaleValue,
              ranges = ranges,
              rotation = rot,
              method = method,
              thresh = thresh,
              pcaComp = pcaComp,
              numComp = numComp,
              ica = ica,
              k = k,
              knnSummary = knnSummary,
              bagImp = bagModels,
              median = median,
              data = if(any(method == "knnImpute")) scale(x[complete.cases(x),,drop = FALSE]) else NULL)
  structure(out, class = "preProcess")
  
}

predict.preProcess <- function(object, newdata, ...)
{

  dataNames <- colnames(newdata)
  ## For centering and scaling, we can be flexible if a column in the
  ## original data set is not in newdata
  if(!is.null(object$mean))
    {
      if(!all(names(object$mean) %in% dataNames))
        {
          if(all(dataNames %in% names(object$mean)))
            {
              object$mean <- object$mean[names(object$mean) %in% dataNames]
              warning("newdata does not contain some variables")
            } else {
              vars <- dataNames[!(dataNames %in% names(object$mean))]
              stop(paste("The following variables were not pre-processed:",
                         paste(vars, collapse = ",")))
            }
        }
    }
  if(!is.null(object$std))
    {
      if(!all(names(object$std) %in% dataNames))
        {
          if(all(dataNames %in% names(object$std)))
            {
              object$std <- object$std[names(object$std) %in% dataNames]
            }
        }
    }
  
  if(!is.null(object$rotation))
    {
      if(!all(names(object$rotation) %in% dataNames))
        {
          stop("newdata does not contain some variables")
        }
    }


  oldClass <- class(newdata)


  if(!is.null(object$bc))
    {
      lam <- unlist(lapply(object$bc, function(x) x$lambda))
      lamIndex <- which(!is.na(lam))
      if(length(lamIndex) > 0)
        {
          for(i in names(lamIndex))
            {
              tt<- newdata[,i]
              tt <- tt[!is.na(tt)]
              if(any(tt <= 0))
                {
                  cat(i, "\n")
                }
              newdata[,i] <- predict(object$bc[[i]], newdata[,i])
            }
        }
    }

  if(!is.null(object$yj))
    {
      lam <- unlist(lapply(object$yj, function(x) if(class(x) == "powerTransform") x$lambda else NA))
      lam <- lam[!is.na(lam)]
      if(length(lam) > 0)
        {
          for(i in seq(along = lam))
            {
              who <-  gsub("\\.Y1$", "", names(lam)[i])
              newdata[,who] <- yjPower(newdata[,who], object$yj[[who]]$lambda)
            }
        }
    }  
  
  if(!is.null(object$et))
  {
      for(i in seq(along = object$et))
      {
        who <-  names(object$et)[i]
        newdata[,who] <- predict(object$et[[who]], newdata[,who])
      }
  }  
  
  if(any(object$method == "range"))
    {
      newdata <- sweep(newdata, 2, object$ranges[1,], "-")
      newdata <- sweep(newdata, 2, object$ranges[2,] - object$ranges[1,], "/")
    }
  
  if(any(object$method == "center")) newdata <- sweep(newdata, 2, object$mean, "-")
  if(any(object$method %in% c("scale"))) newdata <- sweep(newdata, 2, object$std, "/")
  
  cc <- complete.cases(newdata)
  if(any(object$method == "knnImpute") && any(!cc))
    {
      hasMiss <- newdata[!cc,,drop = FALSE]      

      hasMiss <- apply(hasMiss,
                       1,
                       nnimp,
                       old = object$data,
                       k = object$k,
                       foo = object$knnSummary)
      hasMiss <- t(hasMiss)

      newdata[!cc,] <- hasMiss
    }
  
  if(any(object$method == "bagImpute") && any(!cc))
    {
      library(ipred)
      hasMiss <- newdata[!cc,,drop = FALSE]
      missingVars <- apply(hasMiss,
                           2,
                           function(x) any(is.na(x)))
      missingVars <- names(missingVars)[missingVars]
      ## ipred's bagging procedure only allows for data frames
      if(!is.data.frame(hasMiss)) hasMiss <- as.data.frame(hasMiss)
      for(i in seq(along = missingVars))
        {
          preds <- predict(object$bagImp[[missingVars[i]]]$model,
                           hasMiss[, !colnames(hasMiss) %in% missingVars[i]])
          
          hasMiss[is.na(hasMiss[,missingVars[i]]),
                  missingVars[i]] <- preds[is.na(hasMiss[,missingVars[i]])]
        }
      newdata[!cc,] <- hasMiss
    }
  
  if (any(object$method == "medianImpute") && any(!cc)) {
    hasMiss <- newdata[!cc, , drop = FALSE]
    missingVars <- apply(hasMiss, 2, function(x) any(is.na(x)))
    missingVars <- names(missingVars)[missingVars]
    if (!is.data.frame(hasMiss)) 
      hasMiss <- as.data.frame(hasMiss)
    for (i in seq(along = missingVars)) {
      hasMiss[is.na(hasMiss[, missingVars[i]]), missingVars[i]] <- object$median[missingVars[i]]
    }
    newdata[!cc, ] <- hasMiss
  }
  
  if(any(object$method == "pca"))
    {
      newdata <-if(is.matrix(newdata)) newdata %*% object$rotation else as.matrix(newdata) %*% object$rotation
    }
  if(any(object$method == "ica"))
    {
      if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
      ##if(object$ica$row.norm) newdata <- apply(newdata, 1, function(u) u/sd(u))
      newdata <- newdata %*% object$ica$K %*% object$ica$W
      colnames(newdata) <- paste("ICA", 1:ncol(object$ica$W), sep = "")
    }

  
  if(any(oldClass == "data.frame")) newdata <- as.data.frame(newdata)

  if(any(object$method == "spatialSign")) newdata <- spatialSign(newdata)
  if(!(any(object$method %in% c("pca", "ica")))) colnames(newdata) <- dataNames
  newdata
}

print.preProcess <- function(x, ...)
{
  printCall(x$call)
  cat("Created from", x$dim[1], "samples and", x$dim[2], "variables\n")

  pp <- x$method
  pp <- gsub("BoxCox", "Box-Cox transformation", pp)
  pp <- gsub("YeoJohnson", "Yeo-Johnson transformation", pp)    
  pp <- gsub("scale", "scaled", pp)
  pp <- gsub("center", "centered", pp)
  pp <- gsub("pca", "principal component signal extraction", pp)
  pp <- gsub("ica", "independent component signal extraction", pp)
  pp <- gsub("spatialSign", "spatial sign transformation", pp)
  pp <- gsub("knnImpute", paste(x$k, "nearest neighbor imputation"), pp)
  pp <- gsub("bagImpute", "bagged tree imputation", pp)  
  pp <- gsub("range", "re-scaling to [0, 1]", pp)  

  ppText <- paste("Pre-processing:", paste(pp, collapse = ", "))
  cat(truncateText(ppText), "\n\n")

  if(any(x$method == "BoxCox"))
    {
      cat("Lambda estimates for Box-Cox transformation:\n")
      if(length(x$bc) < 11)
         {
           lmbda <- unlist(lapply(x$bc, function(x) x$lambda))
           naLmbda <- sum(is.na(lmbda))
           cat(paste(round(lmbda[!is.na(lmbda)], 2), collapse = ", "))
           if(naLmbda > 0) cat(" (#NA: ", naLmbda, ")\n", sep = "")
         } else print(summary(unlist(lapply(x$bc, function(x) x$lambda))))
      cat("\n")
    }
  if(any(x$method == "YeoJohnson"))
    {
      cat("Lambda estimates for Yeo-Johnson transformation:\n")
      if(length(x$yj) < 11)
         {
           lmbda <- unlist(lapply(x$yj, function(x) if(class(x) == "powerTransform") x$lambda else NA))
           naLmbda <- sum(is.na(lmbda))
           cat(paste(round(lmbda[!is.na(lmbda)], 2), collapse = ", "))
           if(naLmbda > 0) cat(" (#NA: ", naLmbda, ")\n", sep = "")
         } else print(summary(unlist(lapply(x$yj, function(x) if(class(x) == "powerTransform") x$lambda else NA))))
      cat("\n")
    }  
  
  if(any(x$method == "pca"))
    {
      if(is.null(x$pcaComp))
        {
          cat("PCA needed", x$numComp, ifelse(x$numComp > 1, "components", "component"),
              "to capture", round(x$thresh*100, 2),
              "percent of the variance\n")
        } else {
          cat("PCA used", x$pcaComp, ifelse(x$pcaComp > 1, "components", "component"), "as specified.\n")
        }
    }
  if(any(x$method == "ica"))
    {
      cat("ICA used", ncol(x$ica$W), "components\n")
    }  
}


nnimp <- function(new, old, k, foo) {
    library(RANN)
    if(all(is.na(new)))
      stop("cannot impute when all predictors are missing in the new data point")
    nms <- names(new)
    cols2 <- which(!is.na(new))
    new <- matrix(new, ncol = length(new))
    colnames(new) <- nms
    non_missing_cols <- cols2
    nn <- nn2(old[, non_missing_cols, drop = FALSE],
              new[, non_missing_cols, drop = FALSE],
              k = k)
    tmp <- old[nn$nn.idx, -non_missing_cols, drop = FALSE]
    subs <- apply(tmp, 2, foo, na.rm = TRUE)
    new[, -non_missing_cols] <- subs
    new
  }

bagImp <- function(var, x, B = 10)
  {
    library(ipred)
    ## The formula interface is much slower than the
    ## (y, X) interface, but the latter would have to
    ## do case-wise deletion of samples from the
    ## training set.
    if(!is.data.frame(x)) x <- as.data.frame(x)
    mod <- bagging(as.formula(paste(var, "~.")),
                   data = x,
                   nbagg = B)
    list(var = var,
         model = mod)
  }


