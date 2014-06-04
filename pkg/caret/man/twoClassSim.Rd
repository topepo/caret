\name{twoClassSim}
\alias{twoClassSim}
\title{
Two-Class Simulations
}
\description{
This function simulates data with two classes, truly important predictors and irrelevant predictions.
}
\usage{
twoClassSim(n = 100, intercept = -5, 
            linearVars = 10, noiseVars = 0, corrVars = 0, 
            corrType = "AR1", corrValue = 0, mislabel = 0)
}
\arguments{
  \item{n}{
The number of simulated data points
}
  \item{intercept}{
The intercept, which controls the class balance. The default value produces a roughly balanced data set when the other defaults are used.
}
  \item{linearVars}{
The number of linearly important effects. See Details below.
}
  \item{noiseVars}{
The number of uncorrelated irrelevant predictors to be included.
}
  \item{corrVars}{
The number of correlated irrelevant predictors to be included.
}
  \item{corrType}{
The correlation structure of the correlated irrelevant predictors. Values of "AR1" and "exch" are available (see Details below)
}
  \item{corrValue}{
The correlation value.
}
  \item{mislabel}{
  The proportion of data that is possibly mislabeled. See Details below.
  }
}
\details{
The data are simulated in sets. First, two multivariate normal predictors (denoted here as \code{A} and \code{B}) are created with a correlation our about 0.65. They change the log-odds using main effects and an interaction:

\preformatted{
intercept - 4A + 4B + 2AB 
}

The intercept is a parameter for the simulation and can be used to control the amount of class imbalance. 

The second set of effects are linear with coefficients that alternate signs and have values between 2.5 and 0.025. For example, if there were six predictors in this set, their contribution to the log-odds would be 

\preformatted{
-2.50C + 2.05D -1.60E + 1.15F -0.70G + 0.25H
}

The third set is a nonlinear function of a single predictor ranging between [0, 1] called \code{J} here:

\preformatted{
(J^3) + 2exp(-6(J-0.3)^2) 
}

The fourth set of informative predictors are copied from one of Friedman's systems and use two more predictors (\code{K} and \code{L}):

\preformatted{
2sin(KL)
}

All of these effects are added up to model the log-odds. This is used to calculate the probability of a sample being in the first class and a random uniform number is used to actually make the assignment of the actual class. To mislabel the data, the probability is reversed (i.e. \code{p = 1 - p}) before the random number generation. 

The user can also add non-informative predictors to the data. These are random standard normal predictors and can be optionally added to the data in two ways: a specified number of independent predictors or a set number of predictors that follow a particular correlation structure. The only two correlation structure that have been implemented are 

\itemize{
\item compound-symmetry (aka exchangeable) where there is a constant correlation between all the predictors

\item auto-regressive 1 [AR(1)]. While there is no time component to these data, this structure can be used to add predictors of varying levels of correlation. For example, if there were 4 predictors and \code{r} was the correlation parameter, the between predictor correlation matrix would be
}

\preformatted{
      | 1             sym   |
      | r    1              |
      | r^2  r    1         |
      | r^3  r^2  r    1    |
      | r^4  r^3  r^2  r  1 |
}   

}
\value{
a data frame with columns:
  \item{Class }{A factor with levels "Class1" and "Class2"}
  \item{TwoFactor1, TwoFactor2 }{Correlated multivariate normal predictors (denoted as \code{A} and \code{B} above)}
  \item{Nonlinear1, Nonlinear2, Nonlinear3}{Uncorrelated random uniform predictors (\code{J}, \code{K} and \code{L} above).}
  \item{Linear1, \ldots}{Optional uncorrelated standard normal predictors (\code{C} through \code{H} above)}
  \item{Noise1, \ldots}{Optional uncorrelated standard normal predictions}
  \item{Corr1, \ldots}{Optional correlated multivariate normal predictors (each with unit variances)}. 
}

\author{
Max Kuhn
}

\examples{
example <- twoClassSim(100, linearVars = 1)
splom(~example[, 1:6], groups = example$Class)
}

\keyword{models}
