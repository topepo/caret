twoClassSim <- function(n = 100, 
                        intercept = -5,
                        linearVars = 10,
                        noiseVars = 0,    ## Number of uncorrelated x's
                        corrVars = 0,     ## Number of correlated x's
                        corrType = "AR1", ## Corr structure ('AR1' or 'exch')
                        corrValue = 0,    ## Corr parameter
                        mislabel = 0) {
  requireNamespaceQuietStop("MASS")
  sigma <- matrix(c(2,1.3,1.3,2),2,2)
  
  tmpData <- data.frame(MASS::mvrnorm(n=n, c(0,0), sigma))
  names(tmpData) <- paste("TwoFactor", 1:2, sep = "")
  if(linearVars > 0) {
    tmpData <- cbind(tmpData, matrix(rnorm(n*linearVars), ncol = linearVars))
    colnames(tmpData)[(1:linearVars)+2] <- paste("Linear", gsub(" ", "0", format(1:linearVars)), sep = "")
  }
  tmpData$Nonlinear1 <- runif(n, min = -1)
  tmpData <- cbind(tmpData, matrix(runif(n*2), ncol = 2))
  colnames(tmpData)[(ncol(tmpData)-1):ncol(tmpData)] <- paste("Nonlinear", 2:3, sep = "")
  
  tmpData <- as.data.frame(tmpData)
  p <- ncol(tmpData)
  
  if(noiseVars > 0) {
    tmpData <- cbind(tmpData, matrix(rnorm(n * noiseVars), ncol = noiseVars))
    colnames(tmpData)[(p+1):ncol(tmpData)] <- paste("Noise", 
                                                    gsub(" ", "0", format(1:noiseVars)), 
                                                    sep = "")
  }
  if(corrVars > 0)  {
    p <- ncol(tmpData)
    library(MASS)
    if(corrType == "exch") {
      vc <- matrix(corrValue, ncol = corrVars,  nrow = corrVars)
      diag(vc) <- 1
    }
    if(corrType == "AR1")  {
      vcValues <- corrValue^(seq(0, corrVars - 1, by = 1))
      vc <- toeplitz(vcValues)
    }    
    tmpData <- cbind(tmpData, MASS::mvrnorm(n, mu = rep(0, corrVars), Sigma = vc))
    colnames(tmpData)[(p+1):ncol(tmpData)] <- paste("Corr", 
                                                    gsub(" ", "0", format(1:corrVars)), 
                                                    sep = "")
  }  
  lp <- intercept -
    4 * tmpData$TwoFactor1 + 4*tmpData$TwoFactor2 + 
    2*tmpData$TwoFactor1*tmpData$TwoFactor2 + 
    (tmpData$Nonlinear1^3) + 2 * exp(-6*(tmpData$Nonlinear1 - 0.3)^2) +
    2*sin(pi*tmpData$Nonlinear2* tmpData$Nonlinear3) 
  
  if(linearVars > 0) {
    lin <- seq(10, 1, length = linearVars)/4 
    lin <- lin * rep(c(-1, 1), floor(linearVars)+1)[1:linearVars] 
    for(i in seq(along = lin)) lp <- lp + tmpData[, i+3]*lin[i]
  }
  
  prob <- binomial()$linkinv(lp)
  if(mislabel > 0 & mislabel < 1) {
    shuffle <- sample(1:nrow(tmpData), floor(nrow(tmpData)*j))
    prob[shuffle] <- 1 - prob[shuffle]
  }
  tmpData$Class <- ifelse(prob <= runif(n), "Class1", "Class2")
  tmpData$Class <- factor(tmpData$Class, levels = c("Class1", "Class2"))
  
  tmpData
}
