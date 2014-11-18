\name{confusionMatrix}
\alias{confusionMatrix.table}
\alias{confusionMatrix.default}
\alias{confusionMatrix}
\title{Create a confusion matrix}
\description{
Calculates a cross-tabulation of observed and predicted classes with associated statistics.
}
\usage{
confusionMatrix(data, ...)

\method{confusionMatrix}{default}(data, reference, positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL, ...)

\method{confusionMatrix}{table}(data, positive = NULL, prevalence = NULL, ...)

}
\arguments{
  \item{data}{a factor of predicted classes (for the default method) or an object of class \code{\link[base]{table}}.}
  \item{reference}{a factor of classes to be used as the true results}
  \item{positive}{an optional character string for the factor level that corresponds to a "positive" result (if that makes sense for your data). If there are only two factor levels, the first level will be used as the "positive" result.}
  \item{dnn}{a character vector of dimnames for the table}
  \item{prevalence}{a numeric value or matrix for the rate of the "positive" class of the data. When \code{data} has two levels, \code{prevalence} should be a single numeric value. Otherwise, it should be a vector of numeric values with elements for each class. The vector should have names corresponding to the classes.}
  \item{\dots}{options to be passed to \code{table}. NOTE: do not include \code{dnn} here}
}
\details{
The functions requires that the factors have exactly the same levels.

For two class problems, the sensitivity, specificity, positive
predictive value and negative predictive value is calculated using the
\code{positive} argument. Also, the prevalence of the "event" is computed from the 
data (unless passed in as an argument), the detection rate (the rate of true events also 
predicted to be events) and the detection prevalence (the prevalence of predicted events).

Suppose a 2x2 table with notation

 \tabular{rcc}{
                    \tab Reference \tab          \cr
         Predicted  \tab Event     \tab No Event \cr
         Event      \tab A         \tab B        \cr
         No Event   \tab C         \tab D        \cr
       }

The formulas used here are:
\deqn{Sensitivity = A/(A+C)}
\deqn{Specificity = D/(B+D)}
\deqn{Prevalence = (A+C)/(A+B+C+D)}
\deqn{PPV = (sensitivity * Prevalence)/((sensitivity*Prevalence) + ((1-specificity)*(1-Prevalence)))}
\deqn{NPV = (specificity * (1-Prevalence))/(((1-sensitivity)*Prevalence) + ((specificity)*(1-Prevalence)))}
\deqn{Detection Rate =  A/(A+B+C+D)}
\deqn{Detection Prevalence =  (A+B)/(A+B+C+D)}
\deqn{Balanced Accuracy =  (Sensitivity+Specificity)/2}

See the references for discussions of the first five formulas.

For more than two classes, these results are
calculated comparing each factor level to the remaining levels
(i.e. a "one versus all" approach). 

The overall accuracy and unweighted Kappa statistic are calculated. A p-value from McNemar's test is also computed using \code{\link[stats]{mcnemar.test}} (which can produce \code{NA} values with sparse tables).

The overall accuracy rate is computed along with a 95 percent confidence interval for this rate (using \code{\link[stats]{binom.test}}) and a one-sided test to see if the accuracy is better than the "no information rate," which is taken to be the largest class percentage in the data.
 
}
\value{
a list with elements
  \item{table}{the results of \code{table} on \code{data} and  \code{reference}}
  \item{positive}{the positive result level}
  \item{overall}{a numeric vector with overall accuracy and Kappa statistic values}
  \item{byClass}{the sensitivity, specificity, positive predictive value, negative predictive value, prevalence, detection rate, detection prevalence and balanced accuracy for each class. For two class systems, this is calculated once using the \code{positive} argument}

}
\author{Max Kuhn}

\references{Kuhn, M. (2008), ``Building predictive models in R using the caret package, '' \emph{Journal of Statistical Software}, (\url{http://www.jstatsoft.org/v28/i05/}).

Altman,  D.G., Bland,  J.M. (1994) ``Diagnostic tests 1: sensitivity and specificity,'' \emph{British Medical Journal}, vol 308, 1552.

Altman,  D.G., Bland,  J.M. (1994) ``Diagnostic tests 2: predictive values,'' \emph{British Medical Journal}, vol 309, 102. 

Velez,  D.R., et. al. (2008) ``A balanced accuracy function for epistasis modeling in imbalanced datasets using multifactor dimensionality reduction.,'' \emph{Genetic Epidemiology}, vol 4, 306. 
}
\seealso{\code{\link{as.table.confusionMatrix}}, \code{\link{as.matrix.confusionMatrix}}, 
\code{\link{sensitivity}},   \code{\link{specificity}}, \code{\link{posPredValue}}, \code{\link{negPredValue}}, 
\code{\link{print.confusionMatrix}}, \code{\link[stats]{binom.test}}}

\examples{
###################
## 2 class example

lvs <- c("normal", "abnormal")
truth <- factor(rep(lvs, times = c(86, 258)),
                levels = rev(lvs))
pred <- factor(
               c(
                 rep(lvs, times = c(54, 32)),
                 rep(lvs, times = c(27, 231))),               
               levels = rev(lvs))

xtab <- table(pred, truth)

confusionMatrix(xtab)
confusionMatrix(pred, truth)
confusionMatrix(xtab, prevalence = 0.25)   

###################
## 3 class example

confusionMatrix(iris$Species, sample(iris$Species))

newPrior <- c(.05, .8, .15)
names(newPrior) <- levels(iris$Species)

confusionMatrix(iris$Species, sample(iris$Species))

}
\keyword{utilities}

