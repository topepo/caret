
#' @importFrom stats rnorm toeplitz
make_noise <- function(n, noiseVars = 0, 
                       corrVars = 0, corrType = "AR1", corrValue = 0,
                       binary = FALSE) {
  requireNamespaceQuietStop("MASS")
  if(noiseVars > 0) {
    tmpData <- matrix(rnorm(n * noiseVars), ncol = noiseVars)
    colnames(tmpData) <- well_numbered("Noise", noiseVars)
  }
  if(corrVars > 0) {
    loadNamespace("MASS")
    if(corrType == "exch") {
      vc <- matrix(corrValue, ncol = corrVars,  nrow = corrVars)
      diag(vc) <- 1
    }
    if(corrType == "AR1") {
      vcValues <- corrValue^(seq(0, corrVars - 1, by = 1))
      vc <- toeplitz(vcValues)
    }    
    tmpData2 <- MASS::mvrnorm(n, mu = rep(0, corrVars), Sigma = vc)
    colnames(tmpData2) <- well_numbered("Corr", corrVars)
  }  
  if(noiseVars == 0 & corrVars  > 0) out <- tmpData2
  if(noiseVars  > 0 & corrVars == 0) out <- tmpData
  if(noiseVars  > 0 & corrVars  > 0) out <- cbind(tmpData, tmpData2)
  if(binary) out <- ifelse(out > 0, 1, 0)
  as.data.frame(out, stringsAsFactors = TRUE)
}

#' @rdname twoClassSim
#' @importFrom stats rnorm
#' @export
SLC14_1 <- function(n = 100, noiseVars = 0, 
                    corrVars = 0, corrType = "AR1", corrValue = 0) {
  
  dat <- matrix(rnorm(n*20, sd = 3), ncol = 20)
  
  foo <- function(x) x[1] + sin(x[2]) + log(abs(x[3])) + x[4]^2 + x[5]*x[6] + 
    ifelse(x[7]*x[8]*x[9] < 0, 1, 0) +
    ifelse(x[10] > 0, 1, 0) + x[11]*ifelse(x[11] > 0, 1, 0) + 
    sqrt(abs(x[12])) + cos(x[13]) + 2*x[14] + abs(x[15]) + 
    ifelse(x[16] < -1, 1, 0) + x[17]*ifelse(x[17] < -1, 1, 0) -
    2 * x[18] - x[19]*x[20]
  
  dat <- as.data.frame(dat, stringsAsFactors = TRUE)
  colnames(dat) <- well_numbered("Var", ncol(dat))
  if(noiseVars > 0 | corrVars > 0) 
    dat <- cbind(dat, make_noise(n = n, 
                                 noiseVars = noiseVars, 
                                 corrVars = corrVars, 
                                 corrType = corrType, 
                                 corrValue = corrValue))
  
  dat$y <- apply(dat[, 1:20], 1, foo) + rnorm(n, sd = 3)
  dat
}

#' @rdname twoClassSim
#' @importFrom stats rnorm
#' @export
SLC14_2 <- function(n = 100, noiseVars = 0, 
                    corrVars = 0, corrType = "AR1", corrValue = 0) {
  
  dat <- matrix(rnorm(n*200, sd = 4), ncol = 200)
  dat <- as.data.frame(dat, stringsAsFactors = TRUE)
  colnames(dat) <- well_numbered("Var", ncol(dat))
  
  if(noiseVars > 0 | corrVars > 0) 
    dat <- cbind(dat, make_noise(n = n, 
                                 noiseVars = noiseVars, 
                                 corrVars = corrVars, 
                                 corrType = corrType, 
                                 corrValue = corrValue))
  
  dat$y <- apply(dat[, 1:200], 1, function(x) sum(log(abs(x)))) + rnorm(n, sd = 5)  - 1
  dat
}

#' @rdname twoClassSim
#' @importFrom stats rbinom rnorm runif binomial
#' @export
LPH07_1 <- function(n = 100, noiseVars = 0, 
                    corrVars = 0, corrType = "AR1", corrValue = 0, factors = FALSE, class = FALSE) {
  
  dat <- matrix(rbinom(n*10, size = 1, prob = .4), ncol = 10)
  dat <- as.data.frame(dat, stringsAsFactors = TRUE)
  colnames(dat) <- well_numbered("Var", ncol(dat))
  foo <- function(w) 2*w[1]*w[10] + 4*w[2]*w[7] + 3*w[4]*w[5] -
    5*w[6]*w[10] + 3*w[8]*w[9] + w[1]*w[2]*w[4] -
    2*w[7]*(1-w[6])*w[2]*w[9] -
    4*(1 - w[10])*w[1]*(1-w[4])
  if(noiseVars > 0 | corrVars > 0) 
    dat <- cbind(dat, make_noise(n = n, 
                                 noiseVars = noiseVars, 
                                 corrVars = corrVars, 
                                 corrType = corrType, 
                                 corrValue = corrValue,
                                 binary = TRUE))

  if(class) {   
    dat$y <- apply(dat[, 1:10], 1, foo) 
    dat$Class <- runif(nrow(dat)) <= binomial()$linkinv(dat$y)
    dat$Class <- factor(ifelse(dat$Class, "Class1", "Class2"))
    dat$y <- NULL
  } else dat$y <- apply(dat[, 1:10], 1, foo) + rnorm(n)
  
  if(factors) 
    for(i in grep("(^Var)|(^Noise)", names(dat), value = TRUE))
      dat[,i] <- factor(paste0("val", dat[,i]))
  
  
  dat
}

#' @rdname twoClassSim
#' @importFrom stats rnorm
#' @export
LPH07_2 <- function(n = 100, noiseVars = 0, 
                    corrVars = 0, corrType = "AR1", corrValue = 0) {
  
  dat <- matrix(rnorm(n*20, sd = 4), ncol = 20)
  dat <- as.data.frame(dat, stringsAsFactors = TRUE)
  colnames(dat) <- well_numbered("Var", ncol(dat))
  foo <- function(x) x[1]*x[2] + x[10]^2 - x[3]*x[17] -
    x[15]*x[4] + x[9]*x[5] + x[19] - x[20]^2 + x[9]*x[8]
  if(noiseVars > 0 | corrVars > 0) 
    dat <- cbind(dat, make_noise(n = n, 
                                 noiseVars = noiseVars, 
                                 corrVars = corrVars, 
                                 corrType = corrType, 
                                 corrValue = corrValue))
  
  dat$y <- apply(dat[, 1:20], 1, foo) + rnorm(n, sd = 4)
  dat
}
