
#' @importFrom ModelMetrics auc
rocPerCol <- function(dat, cls){
  roc_auc <- auc(cls, dat)
  max(roc_auc, 1 - roc_auc)
}

#' @importFrom utils modifyList
asNumeric <- function(data){
  fc <- sapply(data, is.factor)
  modifyList(data, lapply(data[, fc], as.numeric))
}



#' Calculation of filter-based variable importance
#'
#' Specific engines for variable importance on a model by model basis.
#'
#'
#' The importance of each predictor is evaluated individually using a
#' ``filter'' approach.
#'
#' For classification, ROC curve analysis is conducted on each predictor. For
#' two class problems, a series of cutoffs is applied to the predictor data to
#' predict the class. The sensitivity and specificity are computed for each
#' cutoff and the ROC curve is computed. The trapezoidal rule is used to
#' compute the area under the ROC curve. This area is used as the measure of
#' variable importance. For multi-class outcomes, the problem is decomposed
#' into all pair-wise problems and the area under the curve is calculated for
#' each class pair (i.e class 1 vs. class 2, class 2 vs. class 3 etc.). For a
#' specific class, the maximum area under the curve across the relevant
#' pair-wise AUC's is used as the variable importance measure.
#'
#' For regression, the relationship between each predictor and the outcome is
#' evaluated. An argument, \code{nonpara}, is used to pick the model fitting
#' technique. When \code{nonpara = FALSE}, a linear model is fit and the
#' absolute value of the $t$-value for the slope of the predictor is used.
#' Otherwise, a loess smoother is fit between the outcome and the predictor.
#' The $R^2$ statistic is calculated for this model against the intercept only
#' null model.
#'
#' @param x A matrix or data frame of predictor data
#' @param y A vector (numeric or factor) of outcomes)
#' @param nonpara should nonparametric methods be used to assess the
#' relationship between the features and response
#' @param ... options to pass to either \code{\link[stats]{lm}} or
#' \code{\link[stats]{loess}}
#' @return A data frame with variable importances. Column names depend on the
#' problem type.  For regression, the data frame contains one column: "Overall"
#' for the importance values.
#' @author Max Kuhn
#' @keywords models
#' @examples
#'
#' data(mdrr)
#' filterVarImp(mdrrDescr[, 1:5], mdrrClass)
#'
#' data(BloodBrain)
#'
#' filterVarImp(bbbDescr[, 1:5], logBBB, nonpara = FALSE)
#' apply(bbbDescr[, 1:5],
#'       2,
#'       function(x, y) summary(lm(y~x))$coefficients[2,3],
#'       y = logBBB)
#'
#' filterVarImp(bbbDescr[, 1:5], logBBB, nonpara = TRUE)
#'
#' @export filterVarImp
#' @importFrom stats loess resid
#' @importFrom utils combn
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

      outStat <- as.data.frame(outStat, stringsAsFactors = FALSE)
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

              if(inherits(regMod, "try-error") | any(is.nan(regMod$residuals))) try(regMod <- lm(y~x, ...))
              if(inherits(regMod, "try-error")) return(NA)
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
