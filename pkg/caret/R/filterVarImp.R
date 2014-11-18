## todo start using foreach here

oldfilterVarImp <- function(x, y, nonpara = FALSE, ...)
{
  {
    notNumber <- unlist(lapply(x, function(x) !is.numeric(x)))
    if(any(notNumber))
      {
        for(i in which(notNumber)) x[,i] <- as.numeric(x[,i])
      }  
  }

  if(is.factor(y))
    {
      classLevels <- levels(y)
      
      outStat <- matrix(NA, nrow = dim(x)[2], ncol = length(classLevels))
      for(i in seq(along = classLevels))
        {
          otherLevels <- classLevels[classLevels != classLevels[i]]
          
          for(k in seq(along = otherLevels))
            {
              tmpSubset <- as.character(y) %in% c(classLevels[i], otherLevels[k])
              tmpY <- factor(as.character(y)[tmpSubset])
              tmpX <- x[tmpSubset,]       
              
              rocAuc <- apply(
                              tmpX, 
                              2, 
                              function(x, class, pos)
                              {
                                isMissing <- is.na(x) | is.na(class) 
                                if(any(isMissing))
                                  {
                                    x <- x[!isMissing]
                                    class <- class[!isMissing]
                                  }
                                outResults <- if(length(unique(x)) > 200) roc(x, class = class, positive = pos)
                                else roc(x, class = class, dataGrid = FALSE, positive = pos)
                                aucRoc(outResults)
                              },
                              class = tmpY, 
                              pos = classLevels[i])
              outStat[, i] <- pmax(outStat[, i], rocAuc, na.rm = TRUE) 
            }
          if(i ==1 & length(classLevels) == 2)
            {
              outStat[, 2] <- outStat[, 1]
              break()
            }         
        }  
      colnames(outStat) <- classLevels
      rownames(outStat) <- dimnames(x)[[2]]
      outStat <- data.frame(outStat)
    } else {

      paraFoo <- function(data, y) abs(coef(summary(lm(y ~ data, na.action = na.omit)))[2, "t value"])
      nonparaFoo <- function(x, y, ...)
        {
          meanMod <- sum((y - mean(y, rm.na = TRUE))^2)
          nzv <- nearZeroVar(x, saveMetrics = TRUE)
          
          if(nzv$zeroVar) return(NA)
          if(nzv$percentUnique < 20)
            {
              regMod <- lm(y~x, na.action = na.omit, ...)
            } else {
              regMod <- try(loess(y~x, na.action = na.omit, ...), silent = TRUE)
              
              if(class(regMod) == "try-error" | any(is.nan(regMod$residuals))) try(regMod <- lm(y~x, ...))
              if(class(regMod) == "try-error") return(NA)
            }
          
          pR2 <- 1 - (sum(resid(regMod)^2)/meanMod)
          if(pR2 < 0) pR2 <- 0
          pR2
        }

      testFunc <- if(nonpara) nonparaFoo else paraFoo

      outStat <- apply(x, 2, testFunc, y = y)      
      outStat <- data.frame(Overall = outStat)
    }
  outStat
}


rocPerCol <- function(dat, cls) pROC::roc(cls, dat)$auc

filterVarImp <- function(x, y, nonpara = FALSE, ...)
{
  require(pROC)
  {
    notNumber <- unlist(lapply(x, function(x) !is.numeric(x)))
    if(any(notNumber))
      {
        for(i in which(notNumber)) x[,i] <- as.numeric(x[,i])
      }  
  }

  if(is.factor(y))
    {
      classLevels <- levels(y)
      k <- length(classLevels)
      
      if(k > 2)
        {
          counter <- 1
          classIndex <- vector(mode = "list", length = k)
          tmpStat <- matrix(NA, nrow = ncol(x), ncol = choose(k, 2))
          for(i in 1:k)
            {
              for(j in i:k)
                {
                  if(i != j)
                    {
                      classIndex[[i]] <- c(classIndex[[i]], counter)
                      classIndex[[j]] <- c(classIndex[[j]], counter)
                      index <- which(y %in% c(classLevels[i], classLevels[j]))
                      tmpX <- x[index,,drop = FALSE]
                      tmpY <- factor(as.character(y[index]), levels = c(classLevels[i], classLevels[j]))
                      tmpStat[,counter] <- apply(tmpX, 2, rocPerCol, cls = tmpY)
                      counter <- counter + 1
                    }
                }
            }
          outStat <- matrix(NA, ncol(x), k)
          for(i in 1:k) outStat[,i] <- apply(tmpStat[,classIndex[[i]]], 1, max)
        } else {
          tmp <- apply(x, 2, rocPerCol, cls = y)
          outStat <- cbind(tmp, tmp)
        }

      outStat <- as.data.frame(outStat)
      colnames(outStat) <- classLevels
      rownames(outStat) <- dimnames(x)[[2]]
      outStat <- data.frame(outStat)
    } else {

      paraFoo <- function(data, y) abs(coef(summary(lm(y ~ data, na.action = na.omit)))[2, "t value"])
      nonparaFoo <- function(x, y, ...)
        {
          meanMod <- sum((y - mean(y, rm.na = TRUE))^2)
          nzv <- nearZeroVar(x, saveMetrics = TRUE)
          
          if(nzv$zeroVar) return(NA)
          if(nzv$percentUnique < 20)
            {
              regMod <- lm(y~x, na.action = na.omit, ...)
            } else {
              regMod <- try(loess(y~x, na.action = na.omit, ...), silent = TRUE)
              
              if(class(regMod) == "try-error" | any(is.nan(regMod$residuals))) try(regMod <- lm(y~x, ...))
              if(class(regMod) == "try-error") return(NA)
            }
          
          pR2 <- 1 - (sum(resid(regMod)^2)/meanMod)
          if(pR2 < 0) pR2 <- 0
          pR2
        }

      testFunc <- if(nonpara) nonparaFoo else paraFoo

      outStat <- apply(x, 2, testFunc, y = y)      
      outStat <- data.frame(Overall = outStat)
    }
  outStat
}
