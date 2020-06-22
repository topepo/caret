#' Simulation Functions
#' 
#' This function simulates regression and classification data with truly
#' important predictors and irrelevant predictions.
#' 
#' The first function (\code{twoClassSim}) generates two class data. The data
#' are simulated in different sets. First, two multivariate normal predictors
#' (denoted here as \code{A} and \code{B}) are created with a correlation our
#' about 0.65. They change the log-odds using main effects and an interaction:
#' 
#' \preformatted{ intercept - 4A + 4B + 2AB }
#' 
#' The intercept is a parameter for the simulation and can be used to control
#' the amount of class imbalance.
#' 
#' The second set of effects are linear with coefficients that alternate signs
#' and have values between 2.5 and 0.025. For example, if there were six
#' predictors in this set, their contribution to the log-odds would be
#' 
#' \preformatted{ -2.50C + 2.05D -1.60E + 1.15F -0.70G + 0.25H }
#' 
#' The third set is a nonlinear function of a single predictor ranging between
#' [0, 1] called \code{J} here:
#' 
#' \preformatted{ (J^3) + 2exp(-6(J-0.3)^2) }
#' 
#' The fourth set of informative predictors are copied from one of Friedman's
#' systems and use two more predictors (\code{K} and \code{L}):
#' 
#' \preformatted{ 2sin(KL) }
#' 
#' All of these effects are added up to model the log-odds.
#' 
#' When \code{ordinal = FALSE}, this is used to calculate the probability of a
#' sample being in the first class and a random uniform number is used to
#' actually make the assignment of the actual class. To mislabel the data, the
#' probability is reversed (i.e. \code{p = 1 - p}) before the random number
#' generation.
#' 
#' For \code{ordinal = TRUE}, random normal errors are added to the linear
#' predictor (i.e. prior to computing the probability) and cut points (0.00,
#' 0.20, 0.75, and 1.00) are used to bin the probabilities into classes
#' \code{"low"}, \code{"med"}, and \code{"high"} (despite the function's name).
#' 
#' The remaining functions simulate regression data sets. \code{LPH07_1} and
#' \code{LPH07_2} are from van der Laan et al. (2007). The first function uses
#' random Bernoulli variables that have a 40\% probability of being a value of
#' 1. The true regression equation is:
#' 
#' \preformatted{ 2*w_1*w_10 + 4*w_2*w_7 + 3*w_4*w_5 - 5*w_6*w_10 + 3*w_8*w_9 +
#' w_1*w_2*w_4 - 2*w_7*(1-w_6)*w_2*w_9 - 4*(1 - w_10)*w_1*(1-w_4) }
#' 
#' The simulated error term is a standard normal (i.e. Gaussian). The noise
#' variables are simulated in the same manner as described above but are made
#' binary based on whether the normal random variable is above or below 0. If
#' \code{factors = TRUE}, each of the predictors is coerced into a factor.
#' This simulation can also be adapted for classification using the option
#' \code{class = TRUE}. In this case, the outcome is converted to be a factor
#' by first computing the logit transformation of the equation above and using
#' uniform random numbers to assign the observed class.
#' 
#' A second function (\code{LPH07_2}) uses 20 independent Gaussians with mean
#' zero and variance 16. The functional form here is:
#' 
#' \preformatted{ x_1*x_2 + x_10^2 - x_3*x_17 - x_15*x_4 + x_9*x_5 + x_19 -
#' x_20^2 + x_9*x_8 }
#' 
#' The error term is also Gaussian with mean zero and variance 16.
#' 
#' The function \code{SLC14_1} simulates a system from Sapp et al. (2014). All
#' informative predictors are independent Gaussian random variables with mean
#' zero and a variance of 9. The prediction equation is:
#' 
#' \preformatted{ x_1 + sin(x_2) + log(abs(x_3)) + x_4^2 + x_5*x_6 +
#' I(x_7*x_8*x_9 < 0) + I(x_10 > 0) + x_11*I(x_11 > 0) + sqrt(abs(x_12)) +
#' cos(x_13) + 2*x_14 + abs(x_15) + I(x_16 < -1) + x_17*I(x_17 < -1) - 2 * x_18
#' - x_19*x_20 }
#' 
#' The random error here is also Gaussian with mean zero and a variance of 9.
#' 
#' \code{SLC14_2} is also from Sapp et al. (2014). Two hundred independent
#' Gaussian variables are generated, each having mean zero and variance 16. The
#' functional form is
#' 
#' \preformatted{ -1 + log(abs(x_1)) + ... + log(abs(x_200)) }
#' 
#' and the error term is Gaussian with mean zero and a variance of 25.
#' 
#' For each simulation, the user can also add non-informative predictors to the
#' data. These are random standard normal predictors and can be optionally
#' added to the data in two ways: a specified number of independent predictors
#' or a set number of predictors that follow a particular correlation
#' structure. The only two correlation structure that have been implemented are
#' 
#' \itemize{ \item compound-symmetry (aka exchangeable) where there is a
#' constant correlation between all the predictors
#' 
#' \item auto-regressive 1 [AR(1)]. While there is no time component to these
#' data, this structure can be used to add predictors of varying levels of
#' correlation. For example, if there were 4 predictors and \code{r} was the
#' correlation parameter, the between predictor correlation matrix would be }
#' 
#' \preformatted{ | 1 sym | | r 1 | | r^2 r 1 | | r^3 r^2 r 1 | | r^4 r^3 r^2 r
#' 1 | }
#' 
#' @aliases twoClassSim SLC14_1 SLC14_2 LPH07_1 LPH07_2
#' @param n The number of simulated data points
#' @param intercept The intercept, which controls the class balance. The
#' default value produces a roughly balanced data set when the other defaults
#' are used.
#' @param linearVars The number of linearly important effects. See Details
#' below.
#' @param noiseVars The number of uncorrelated irrelevant predictors to be
#' included.
#' @param corrVars The number of correlated irrelevant predictors to be
#' included.
#' @param corrType The correlation structure of the correlated irrelevant
#' predictors. Values of "AR1" and "exch" are available (see Details below)
#' @param corrValue The correlation value.
#' @param mislabel The proportion of data that is possibly mislabeled. Only
#' used when \code{ordinal = FALSE}. See Details below.
#' @param ordinal Should an ordered factor be returned? See Details below.
#' @param factors Should the binary predictors be converted to factors?
#' @param class Should the simulation produce class labels instead of numbers?
#' @return a data frame with columns: \item{Class }{A factor with levels
#' "Class1" and "Class2"} \item{TwoFactor1, TwoFactor2 }{Correlated
#' multivariate normal predictors (denoted as \code{A} and \code{B} above)}
#' \item{Nonlinear1, Nonlinear2, Nonlinear3}{Uncorrelated random uniform
#' predictors (\code{J}, \code{K} and \code{L} above).} \item{Linear1,
#' }{Optional uncorrelated standard normal predictors (\code{C} through
#' \code{H} above)}\item{list()}{Optional uncorrelated standard normal
#' predictors (\code{C} through \code{H} above)} \item{Noise1, }{Optional
#' uncorrelated standard normal predictions}\item{list()}{Optional uncorrelated
#' standard normal predictions} \item{Corr1, }{Optional correlated multivariate
#' normal predictors (each with unit variances)}\item{list()}{Optional
#' correlated multivariate normal predictors (each with unit variances)}.
#' @author Max Kuhn
#' @references van der Laan, M. J., & Polley Eric, C. (2007). Super learner.
#' Statistical Applications in Genetics and Molecular Biology, 6(1), 1-23.
#' 
#' Sapp, S., van der Laan, M. J., & Canny, J. (2014). Subsemble: an ensemble
#' method for combining subset-specific algorithm fits. Journal of Applied
#' Statistics, 41(6), 1247-1259.
#' @keywords models
#' @examples
#' 
#' example <- twoClassSim(100, linearVars = 1)
#' splom(~example[, 1:6], groups = example$Class)
#' 
#' @export twoClassSim
twoClassSim <- function(n = 100, 
                        intercept = -5,
                        linearVars = 10,
                        noiseVars = 0,    ## Number of uncorrelated x's
                        corrVars = 0,     ## Number of correlated x's
                        corrType = "AR1", ## Corr structure ('AR1' or 'exch')
                        corrValue = 0,    ## Corr parameter
                        mislabel = 0,
                        ordinal = FALSE) {
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
  
  tmpData <- as.data.frame(tmpData, stringsAsFactors = FALSE)
  p <- ncol(tmpData)
  
  if(noiseVars > 0) {
    tmpData <- cbind(tmpData, matrix(rnorm(n * noiseVars), ncol = noiseVars))
    colnames(tmpData)[(p+1):ncol(tmpData)] <- paste("Noise", 
                                                    gsub(" ", "0", format(1:noiseVars)), 
                                                    sep = "")
  }
  if(corrVars > 0)  {
    p <- ncol(tmpData)
    loadNamespace("MASS")
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
  
  if(ordinal){
    prob <- binomial()$linkinv(lp + rnorm(n,sd = 2)) 
    tmpData$Class <- cut(prob, breaks = c(0, .2, .75, 1), 
                         include.lowest = TRUE, 
                         labels = c("low", "med", "high"), 
                         ordered_result = TRUE)
  } else {
    prob <- binomial()$linkinv(lp)
    if(mislabel > 0 & mislabel < 1) {
      shuffle <- sample(1:nrow(tmpData), floor(nrow(tmpData)*mislabel))
      prob[shuffle] <- 1 - prob[shuffle]
    }
    tmpData$Class <- ifelse(prob <= runif(n), "Class1", "Class2")
    tmpData$Class <- factor(tmpData$Class, levels = c("Class1", "Class2"))
  }
  
  tmpData
}
