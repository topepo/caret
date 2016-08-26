
rocPerCol <- function(dat, cls){
  ModelMetrics::auc(cls, dat)
}

asNumeric <- function(data){
  fc <- sapply(data, is.factor)
  modifyList(data, lapply(data[, fc], as.numeric))
}

filterVarImp <- function(x, y, nonpara = FALSE, ...){
  # converting factors to numeric
  notNumber <- sapply(x, function(x) !is.numeric(x))
  x = asNumeric(x)

  if(is.factor(y)){
      classLevels <- levels(y)
      k <- length(classLevels)

      if(k > 2){

        Combs <- combn(classLevels, 2)
        CombsN <- combn(1:k, 2)

          lStat <- lapply(1:ncol(Combs), FUN = function(cc){
            yLevs <- as.character(y) %in% Combs[,cc]
            tmpX <- x[yLevs,]
            tmpY <- as.numeric(y[yLevs] == Combs[,cc][2])
            apply(tmpX, 2, rocPerCol, cls = tmpY)
          })
          Stat = do.call("cbind", lStat)

          loutStat <- lapply(1:k, function(j){
            apply(Stat[,CombsN[,j]], 1, max)
          })

          outStat = do.call("cbind", loutStat)

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
