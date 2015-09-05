\name{shuffleVarImp}


\title{Calculation of variable importance for regression and classification models using prediction shuffling}

\description{
A generic method for calculating variable importance for objects produced by
\code{train} 
}

\usage{
\method{shuffleVarImp}{train}(object, useModel = TRUE, nonpara = TRUE, scale = TRUE, ...)



\arguments{
  \item{object}{An object corresponding to a fitted model}
  \item{test_x}{A data frame of predictors of test set}  
  \item{test_y}{Target variable of test set}  
  \item{shuffleTimes}{How many times to shuffle}   
}

\value{
   A data frame with class \code{c("varImp.train", "data.frame")} for
   \code{varImp.train} or a matrix for other models.
 }

\details{
For models that do not have corresponding \code{varImp} methods, see
\code{filerVarImp}.

Otherwise:


 \bold{Linear Models}: the absolute value of the t--statistic
   for each model parameter is used.

 \bold{Random Forest}: \code{varImp.randomForest} and
   \code{varImp.RandomForest} are wrappers around the importance functions from the
   \pkg{randomForest} and \pkg{party} packages, respectively.
   
   \bold{Partial Least Squares}: the variable importance measure here is based on 
   weighted sums of the absolute regression coefficients. The weights are a function of
   the reduction of the sums of squares across the number of PLS components and are 
   computed separately for each outcome. Therefore, the contribution of the coefficients
  are weighted proportionally to the reduction in the sums of squares.
  
   
  \bold{Recursive Partitioning}: The reduction in the loss function
  (e.g. mean squared error) attributed to each variable at each split is 
  tabulated and the sum is returned. Also, since there may be candidate variables
  that are important but are not used in a split, the top competing variables are
  also tabulated at each split. This can be turned off using the \code{maxcompete}
  argument in \code{rpart.control}. This method does not currently provide
  class--specific measures of importance when the response is a factor.
  
  \bold{Bagged Trees}: The same methodology as a single tree is applied to 
  all bootstrapped trees and the total importance is returned

  \bold{Boosted Trees}: \code{varImp.gbm} is a wrapper around the function from that package (see the \pkg{gbm} package vignette)
  
  \bold{ Multivariate Adaptive Regression Splines}: MARS models 
        include a backwards elimination feature selection routine that
        looks at reductions in the generalized cross-validation (GCV)
        estimate of error. The \code{varImp} function tracks the changes in
        model statistics, such as the GCV, for each predictor and
        accumulates the reduction in the statistic when each
        predictor's feature is added to the model. This total reduction
        is used as the variable importance measure. If a predictor was
        never used in any of the MARS basis functions in the final model 
        (after pruning), it has an importance
        value of zero. Prior to June 2008, the package used an internal function 
        for these calculations. Currently, the \code{varImp}  is a wrapper to
        the \code{\link[earth]{evimp}}  function in the \code{earth} package. There are three statistics that can be used to
        estimate variable importance in MARS models. Using
        \code{varImp(object, value = "gcv")} tracks the reduction in the
        generalized cross-validation statistic as terms are added.
        However, there are some cases when terms are retained 
        in the model that result in an increase in GCV. Negative variable 
        importance values for MARS are set to zero. 
        Alternatively, using
        \code{varImp(object, value = "rss")} monitors the change in the
        residual sums of squares (RSS) as terms are added, which will
        never be negative. 
        Also, the option \code{varImp(object, value =" nsubsets")}, which 
        counts the number of subsets where the variable is used (in the final, 
        pruned model). 
   
  \bold{Nearest shrunken centroids}: The difference between the class centroids and the overall centroid is used to measure the variable influence (see \code{pamr.predict}). The larger the difference between   the class centroid and the overall center of the data, the larger the separation between the classes. The training set predictions must be supplied when an object of class \code{pamrtrained} is given to \code{varImp}. 

  \bold{Cubist}: The Cubist output contains variable usage
  statistics. It gives the percentage of times where each variable was
  used in a condition and/or a linear model. Note that this output
  will probably be inconsistent with the rules shown in the output
  from \code{\link[Cubist]{summary.cubist}}. At each split of the
  tree, Cubist saves a linear model (after feature selection) that is
  allowed to have terms for each variable used in the current split or
  any split above it. Quinlan (1992) discusses a smoothing algorithm
  where each model prediction is a linear combination of the parent
  and child model along the tree. As such, the final prediction is a
  function of all the linear models from the initial node to the
  terminal node. The percentages shown in the Cubist output reflects
  all the models involved in prediction (as opposed to the terminal
  models shown in the output). The variable importance used here is a
  linear combination of the usage in the rule conditions and the
  model.
  
  \bold{PART} and \bold{JRip}: For these rule-based models, the
  importance for a predictor is simply the number of rules that
  involve the predictor.

  \bold{C5.0}: C5.0 measures predictor importance by determining the
percentage of training set samples that fall into all the terminal
nodes after the split. For example, the predictor in the first split
automatically has an importance measurement of 100 percent since all
samples are affected by this split. Other predictors may be used
frequently in splits, but if the terminal nodes cover only a handful
of training set samples, the importance scores may be close to
zero. The same strategy is applied to rule-based models and boosted
versions of the model. The underlying function can also return the
number of times each predictor was involved in a split by using the
option \code{metric = "usage"}.

\bold{Neural Networks}: The method used here is based on Gevrey et al
(2003), which uses combinations of the absolute values of the
weights. For classification models, the class-specific importances
will be the same.
  
\bold{Recursive Feature Elimination}: Variable importance is computed using the ranking method used for feature selection. For the final subset size, the importances for the models across all resamples are averaged to compute an overall value. 

\bold{Feature Selection via Univariate Filters}, the percentage of resamples that a predictor was selected is determined. In other words, an importance of 0.50 means that the predictor survived the filter in half of the resamples. 

}

\author{Max Kuhn}

\references{Gevrey, M., Dimopoulos, I., & Lek, S. (2003). Review
and comparison of methods to study the contribution of variables in
artificial neural network models. Ecological Modelling, 160(3),
249-264.

Quinlan, J. (1992). Learning with continuous classes. Proceedings of
the 5th Australian Joint Conference On Artificial Intelligence,
343-348.}

\keyword{ models }


