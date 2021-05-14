#' Calculation of variable importance for regression and classification models
#'
#' A generic method for calculating variable importance for objects produced by
#' \code{train} and method specific methods
#'
#' For models that do not have corresponding \code{varImp} methods, see
#' \code{\link{filterVarImp}}.
#'
#' Otherwise:
#'
#' \bold{Linear Models}: the absolute value of the t-statistic for each model
#' parameter is used.
#'
#' \bold{\code{glmboost}} and \bold{\code{glmnet}}: the absolute value of the coefficients
#' corresponding the the tuned model are used.
#'
#' \bold{Random Forest}: \code{varImp.randomForest} and
#' \code{varImp.RandomForest} are wrappers around the importance functions from
#' the \pkg{randomForest} and \pkg{party} packages, respectively.
#'
#' \bold{Partial Least Squares}: the variable importance measure here is based
#' on weighted sums of the absolute regression coefficients. The weights are a
#' function of the reduction of the sums of squares across the number of PLS
#' components and are computed separately for each outcome. Therefore, the
#' contribution of the coefficients are weighted proportionally to the
#' reduction in the sums of squares.
#'
#' \bold{Recursive Partitioning}: The reduction in the loss function (e.g. mean
#' squared error) attributed to each variable at each split is tabulated and
#' the sum is returned. Also, since there may be candidate variables that are
#' important but are not used in a split, the top competing variables are also
#' tabulated at each split. This can be turned off using the \code{maxcompete}
#' argument in \code{rpart.control}. This method does not currently provide
#' class-specific measures of importance when the response is a factor.
#'
#' \bold{Bagged Trees}: The same methodology as a single tree is applied to all
#' bootstrapped trees and the total importance is returned
#'
#' \bold{Boosted Trees}: \code{varImp.gbm} is a wrapper around the function
#' from that package (see the \pkg{gbm} package vignette)
#'
#' \bold{ Multivariate Adaptive Regression Splines}: MARS models include a
#' backwards elimination feature selection routine that looks at reductions in
#' the generalized cross-validation (GCV) estimate of error. The \code{varImp}
#' function tracks the changes in model statistics, such as the GCV, for each
#' predictor and accumulates the reduction in the statistic when each
#' predictor's feature is added to the model. This total reduction is used as
#' the variable importance measure. If a predictor was never used in any of the
#' MARS basis functions in the final model (after pruning), it has an
#' importance value of zero. Prior to June 2008, the package used an internal
#' function for these calculations. Currently, the \code{varImp} is a wrapper
#' to the \code{\link[earth]{evimp}} function in the \code{earth} package.
#' There are three statistics that can be used to estimate variable importance
#' in MARS models. Using \code{varImp(object, value = "gcv")} tracks the
#' reduction in the generalized cross-validation statistic as terms are added.
#' However, there are some cases when terms are retained in the model that
#' result in an increase in GCV. Negative variable importance values for MARS
#' are set to zero.  Alternatively, using \code{varImp(object, value = "rss")}
#' monitors the change in the residual sums of squares (RSS) as terms are
#' added, which will never be negative.  Also, the option \code{varImp(object,
#' value =" nsubsets")}, which counts the number of subsets where the variable
#' is used (in the final, pruned model).
#'
#' \bold{Nearest shrunken centroids}: The difference between the class
#' centroids and the overall centroid is used to measure the variable influence
#' (see \code{pamr.predict}). The larger the difference between the class
#' centroid and the overall center of the data, the larger the separation
#' between the classes. The training set predictions must be supplied when an
#' object of class \code{pamrtrained} is given to \code{varImp}.
#'
#' \bold{Cubist}: The Cubist output contains variable usage statistics. It
#' gives the percentage of times where each variable was used in a condition
#' and/or a linear model. Note that this output will probably be inconsistent
#' with the rules shown in the output from
#' \code{\link[Cubist]{summary.cubist}}. At each split of the tree, Cubist
#' saves a linear model (after feature selection) that is allowed to have terms
#' for each variable used in the current split or any split above it. Quinlan
#' (1992) discusses a smoothing algorithm where each model prediction is a
#' linear combination of the parent and child model along the tree. As such,
#' the final prediction is a function of all the linear models from the initial
#' node to the terminal node. The percentages shown in the Cubist output
#' reflects all the models involved in prediction (as opposed to the terminal
#' models shown in the output). The variable importance used here is a linear
#' combination of the usage in the rule conditions and the model.
#'
#' \bold{PART} and \bold{JRip}: For these rule-based models, the importance for
#' a predictor is simply the number of rules that involve the predictor.
#'
#' \bold{C5.0}: C5.0 measures predictor importance by determining the
#' percentage of training set samples that fall into all the terminal nodes
#' after the split. For example, the predictor in the first split automatically
#' has an importance measurement of 100 percent since all samples are affected
#' by this split. Other predictors may be used frequently in splits, but if the
#' terminal nodes cover only a handful of training set samples, the importance
#' scores may be close to zero. The same strategy is applied to rule-based
#' models and boosted versions of the model. The underlying function can also
#' return the number of times each predictor was involved in a split by using
#' the option \code{metric = "usage"}.
#'
#' \bold{Neural Networks}: The method used here is based on Gevrey et al
#' (2003), which uses combinations of the absolute values of the weights. For
#' classification models, the class-specific importances will be the same.
#'
#' \bold{Recursive Feature Elimination}: Variable importance is computed using
#' the ranking method used for feature selection. For the final subset size,
#' the importances for the models across all resamples are averaged to compute
#' an overall value.
#'
#' \bold{Feature Selection via Univariate Filters}, the percentage of resamples
#' that a predictor was selected is determined. In other words, an importance
#' of 0.50 means that the predictor survived the filter in half of the
#' resamples.
#'
#' @aliases varImp varImp.train varImp.earth varImp.rpart varImp.randomForest
#' varImp.gbm varImp.regbagg varImp.classbagg varImp.pamrtrained varImp.lm
#' varImp.mvr varImp.bagEarth varImp.bagFDA varImp.RandomForest varImp.rfe
#' varImp.dsa varImp.fda varImp.multinom varImp.cubist varImp.plsda varImp.JRip
#' varImp.PART varImp.nnet varImp.C5.0 varImp.glmnet
#' @param object an object corresponding to a fitted model
#' @param useModel use a model based technique for measuring variable
#' importance?  This is only used for some models (lm, pls, rf, rpart, gbm, pam
#' and mars)
#' @param nonpara should nonparametric methods be used to assess the
#' relationship between the features and response (only used with
#' \code{useModel = FALSE} and only passed to \code{filterVarImp}).
#' @param scale should the importance values be scaled to 0 and 100?
#' @param \dots parameters to pass to the specific \code{varImp} methods
#' @param numTrees the number of iterations (trees) to use in a boosted tree
#' model
#' @param threshold the shrinkage threshold (\code{pamr} models only)
#' @param data the training set predictors (\code{pamr} models only)
#' @param value the statistic that will be used to calculate importance: either
#' \code{gcv}, \code{nsubsets}, or \code{rss}
#' @param surrogates should surrogate splits contribute to the importance
#' calculation?
#' @param competes should competing splits contribute to the importance
#' calculation?
#' @param estimate which estimate of performance should be used? See
#' \code{\link[pls]{mvrVal}}
#' @param cuts the number of rule sets to use in the model (for \code{partDSA}
#' only)
#' @param weights a numeric vector of length two that weighs the usage of
#' variables in the rule conditions and the usage in the linear models (see
#' details below).
#' @param lambda a single value of the penalty parameter
#' @return A data frame with class \code{c("varImp.train", "data.frame")} for
#' \code{varImp.train} or a matrix for other models.
#' @author Max Kuhn
#' @references Gevrey, M., Dimopoulos, I., & Lek, S. (2003). Review and
#' comparison of methods to study the contribution of variables in artificial
#' neural network models. Ecological Modelling, 160(3), 249-264.
#'
#' Quinlan, J. (1992). Learning with continuous classes. Proceedings of the 5th
#' Australian Joint Conference On Artificial Intelligence, 343-348.
#' @keywords models
#' @export varImp
"varImp" <-
function(object, ...){
   UseMethod("varImp")
}

#' @importFrom stats4 coef
GarsonWeights <- function(object)
  {
    beta <- coef(object)
    abeta <- abs(beta)
    nms <- names(beta)
    i2h <- array(NA, dim = object$n[2:1])
    h2o <- array(NA, dim = object$n[2:3])

    for (hidden in 1:object$n[2]) {
      for (input in 1:object$n[1]) {
        label <- paste("i", input, "->h", hidden,"$", sep = "")
        i2h[hidden, input] <- abeta[grep(label, nms, fixed = FALSE)]
      }
    }
    for(hidden in 1:object$n[2]){
        for(output in 1:object$n[3]){
            label <- paste("h", hidden, "->o",
                           ifelse(object$n[3] == 1, "", output),
                           sep = "")
            h2o[hidden,output] <- abeta[grep(label, nms, fixed = TRUE)]
          }
      }

    if(FALSE)
      {
        ## Test case from Gevrey, M., Dimopoulos, I., & Lek,
        ## S. (2003). Review and comparison of methods to study the
        ## contribution of variables in artificial neural network
        ## models. ecological modelling, 160(3), 249-264.
        i2h <- matrix(c(-1.67624,  3.29022,  1.32466,
                        -0.51874, -0.22921, -0.25526,
                        -4.01764,  2.12486, -0.08168,
                        -1.75691, -1.44702,  0.58286),
                      ncol = 3, byrow = TRUE)
        h2o <- matrix(c(4.57857, -0.48815, -5.73901, -2.65221),
                      ncol = 1)
      }

    ##  From Gevrey et al. (2003): "For each hidden neuron i, multiply
    ##  the absolute value of the hidden-output layer connection
    ##  weight by the absolute value of the hidden-input layer
    ##  connection weight. Do this for each input variable j. The
    ##  following products Pij are obtained"


    ## We'll do this one response at a time. Gevrey et al. (2003) do
    ## not discuss multiple outputs, but the results are the same (at
    ## least in the case of classification).

    imp <- matrix(NA, nrow = object$n[1], ncol = object$n[3])


    for(output in 1:object$n[3])
      {
        Pij <- i2h * NA
        for(hidden in 1:object$n[2]) Pij[hidden,] <- i2h[hidden,] * h2o[hidden,output]

        ## "For each hidden neuron, divide Pij by the sum for all the
        ## input variables to obtain Qij. For example for Hidden 1, Q11 =
        ## P11/(P11+P12+P13).

        Qij <- Pij * NA
        for(hidden in 1:object$n[2]) Qij[hidden,] <- Pij[hidden,] / sum(Pij[hidden,])


        ## "For each input neuron, sum the product Sj formed from the
        ## previous computations of Qij. For example, S1 =
        ## Q11+Q21+Q31+Q41."

        Sj <- apply(Qij, 2, sum)

        ## "Divide Sj by the sum for all the input variables. Expressed as
        ## a percentage, this gives the relative importance or
        ## distribution of all output weights attributable to the given
        ## input variable. For example, for the input neuron 1, the
        ## relative importance is equal to (S1/100)/(S1+S2+S3)"

        imp[,output] <- Sj/sum(Sj)*100
        rm(Pij, Qij, Sj)
      }

    colnames(imp) <- if(!is.null(colnames(object$residuals))) colnames(object$residuals) else paste("Y", 1:object$n[3], sep = "")
    rownames(imp) <- if(!is.null(object$coefnames)) object$coefnames else  paste("X", 1:object$n[1], sep = "")
    imp
}


GarsonWeights_FCNN4R <- function (object, xnames = NULL, ynames = NULL) {
  beta <- abs(object$net@m_w_values[which(object$net@m_w_flags != 0L)])
  dims <- object$net@m_layers

  index <- (dims[1]+1)*dims[2]
  i2h <- t(matrix(beta[1:index], ncol = dims[2]))
  i2h <- i2h[, -1,drop = FALSE]

  h2o <- matrix(beta[(index+1):length(beta)], ncol = dims[3])
  h2o <- h2o[-1,,drop = FALSE]

  imp <- matrix(NA, nrow = dims[1], ncol = dims[3])
  for (output in 1:dims[3]) {
    Pij <- i2h * NA
    for (hidden in 1:dims[2]) Pij[hidden, ] <- i2h[hidden,] * h2o[hidden, output]
    Qij <- Pij * NA
    for (hidden in 1:dims[2]) Qij[hidden, ] <- Pij[hidden,]/sum(Pij[hidden, ])
    Sj <- apply(Qij, 2, sum)
    imp[, output] <- Sj/sum(Sj) * 100
    rm(Pij, Qij, Sj)
  }
  rownames(imp) <- if(is.null(xnames))
    paste("X", 1:dims[1], sep = "") else
      xnames
  colnames(imp) <- if(is.null(ynames))
    paste("Y", 1:dims[3], sep = "") else
      ynames
  imp
}

varImpDependencies <- function(libName){
  code <- getModelInfo(libName, regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along = code$library))
    do.call("requireNamespaceQuietStop", list(package = code$library[i]))
  return(code)
}

#' @rdname varImp
#' @export
varImp.bagEarth <- function(object, ...){
  code <- varImpDependencies("bagEarth")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.bagFDA <- function(object, ...){
  code <- varImpDependencies("bagFDA")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.C5.0 <- function(object, ...){
  code <- varImpDependencies("C5.0")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.cubist <- function(object, weights = c(0.5, 0.5), ...){
  code <- varImpDependencies("cubist")
  code$varImp(object, weights = weights, ...)
}

#' @rdname varImp
#' @export
varImp.dsa <- function(object, cuts = NULL, ...){
  code <- varImpDependencies("partDSA")
  code$varImp(object, cuts = cuts, ...)
}

#' @rdname varImp
#' @export
varImp.glm <- function(object, ...){
  code <- varImpDependencies("glm")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.glmnet <- function(object, lambda = NULL, ...){
  code <- varImpDependencies("glmnet")
  code$varImp(object, lambda = lambda, ...)
}

#' @rdname varImp
#' @export
varImp.JRip <- function(object, ...){
  code <- varImpDependencies("JRip")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.multinom <- function(object, ...){
  code <- varImpDependencies("multinom")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.nnet <- function(object, ...){
  code <- varImpDependencies("nnet")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.avNNet <- function(object, ...){
  code <- varImpDependencies("nnet")
  imps <- lapply(object$model, code$varImp)
  imps <- do.call("rbind", imps)
  imps <- aggregate(imps, by = list(vars = rownames(imps)), mean)
  rownames(imps) <- as.character(imps$vars)
  imps$vars <- NULL
  imps
}

#' @rdname varImp
#' @export
varImp.PART <- function(object, ...){
  code <- varImpDependencies("PART")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.RRF <- function(object, ...){
  code <- varImpDependencies("RRF")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.rpart <- function(object, surrogates = FALSE, competes = TRUE, ...){
  code <- varImpDependencies("rpart")
  code$varImp(object, surrogates = surrogates, competes = competes, ...)
}

#' @rdname varImp
#' @export
varImp.randomForest <- function(object, ...){
  code <- varImpDependencies("rf")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.gbm <- function(object, numTrees = NULL, ...){
  code <- varImpDependencies("gbm")
  code$varImp(object, numTrees = numTrees, ...)
}

#' @rdname varImp
#' @export
varImp.classbagg <- function(object, ...){
  code <- varImpDependencies("treebag")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.regbagg <- function(object, ...){
  code <- varImpDependencies("treebag")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.pamrtrained <- function(object, threshold, data, ...){
  code <- varImpDependencies("pam")
  code$varImp(object,
              threshold = object$bestTune$threshold,
              data = object$finalModel$xData,
              ...)
}

#' @rdname varImp
#' @export
varImp.lm <- function(object, ...){
  code <- varImpDependencies("lm")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.mvr <- function(object, estimate = NULL, ...){
  code <- varImpDependencies("pls")
  code$varImp(object, estimate = estimate, ...)
}

#' @rdname varImp
#' @export
varImp.earth <- function(object, value = "gcv", ...){
  code <- varImpDependencies("earth")
  code$varImp(object, value = value, ...)
}

#' @rdname varImp
#' @export
varImp.RandomForest <- function(object, ...){
  code <- varImpDependencies("cforest")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.plsda <- function(object, ...){
  code <- varImpDependencies("pls")
  code$varImp(object, ...)
}

#' @rdname varImp
#' @export
varImp.fda <- function(object, value = "gcv", ...){
  code <- varImpDependencies("fda")
  code$varImp(object, value = value, ...)
}

#' @rdname varImp
#' @export
varImp.gam <- function(object, ...){
  code <- varImpDependencies("gam")
  code$varImp(object, ...)
}


#' @rdname varImp
#' @export
varImp.Gam <- function(object, ...){
  code <- varImpDependencies("gamSpline")
  code$varImp(object, ...)
}

