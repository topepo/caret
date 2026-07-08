## In these functions, x is the data fram of performance values and tuning parameters.
#' Selecting tuning Parameters
#'
#' Various functions for setting tuning parameters
#'
#' These functions can be used by [train()] to select the "optimal" model from
#' a series of models. Each requires the user to select a metric that will be
#' used to judge performance. For regression models, values of `"RMSE"` and
#' `"Rsquared"` are applicable. Classification models use either `"Accuracy"`
#' or `"Kappa"` (for unbalanced class distributions.
#'
#' More details on these functions can be found at
#' <http://topepo.github.io/caret/model-training-and-tuning.html#custom>.
#'
#' By default, [train()] uses `best`.
#'
#' `best` simply chooses the tuning parameter associated with the largest (or
#' lowest for `"RMSE"`) performance.
#'
#' `oneSE` is a rule in the spirit of the "one standard error" rule of Breiman
#' et al. (1984), who suggest that the tuning parameter associated with the
#' best performance may over fit. They suggest that the simplest model within
#' one standard error of the empirically optimal model is the better choice.
#' This assumes that the models can be easily ordered from simplest to most
#' complex (see the Details section below).
#'
#' `tolerance` takes the simplest model that is within a percent tolerance of
#' the empirically optimal model. For example, if the largest Kappa value is
#' 0.5 and a simpler model within 3 percent is acceptable, we score the other
#' models using `(x - 0.5)/0.5 * 100`. The simplest model whose score is not
#' less than 3 is chosen (in this case, a model with a Kappa value of 0.35 is
#' acceptable).
#'
#' User-defined functions can also be used. The argument `selectionFunction` in
#' [trainControl()] can be used to pass the function directly or to pass the
#' function by name.
#'
#' @aliases oneSE best tolerance
#' @param x a data frame of tuning parameters and model results, sorted from
#'   least complex models to the mst complex
#' @param metric a string that specifies what summary metric will be used to
#'   select the optimal model. By default, possible values are "RMSE" and
#'   "Rsquared" for regression and "Accuracy" and "Kappa" for classification.
#'   If custom performance metrics are used (via the `summaryFunction` argument
#'   in [trainControl()], the value of `metric` should match one of the
#'   arguments. If it does not, a warning is issued and the first metric given
#'   by the `summaryFunction` is used.
#' @param maximize a logical: should the metric be maximized or minimized?
#' @param num the number of resamples (for `oneSE` only)
#' @param tol the acceptable percent tolerance (for `tolerance` only)
#' @return a row index
#' @note In many cases, it is not very clear how to order the models on
#'   simplicity. For simple trees and other models (such as PLS), this is
#'   straightforward. However, for others it is not.
#'
#' For example, many of the boosting models used by \pkg{caret} have parameters
#' for the number of boosting iterations and the tree complexity (others may
#' also have a learning rate parameter). In this implementation, we order
#' models on number of iterations, then tree depth. Clearly, this is arguable
#' (please email the author for suggestions though).
#'
#' For MARS models, they are orders on the degree of the features, then the
#' number of retained terms.
#'
#' RBF SVM models are ordered first by the cost parameter, then by the kernel
#' parameter while polynomial models are ordered first on polynomial degree,
#' then cost and scale.
#'
#' Neural networks are ordered by the number of hidden units and then the
#' amount of weight decay.
#'
#' k-nearest neighbor models are ordered from most neighbors to least (i.e.
#' smoothest to model jagged decision boundaries).
#'
#' Elastic net models are ordered first on the L1 penalty, then by the L2
#' penalty.
#' @author Max Kuhn
#' @seealso [train()], [trainControl()]
#' @references Breiman, Friedman, Olshen, and Stone. (1984) *Classification and
#'   Regression Trees*. Wadsworth.
#' @keywords manip
#' @examplesIf !caret:::is_cran_check()
#'
#' # simulate a PLS regression model
#' test <- data.frame(ncomp = 1:5, RMSE = c(3, 1.1, 1.02, 1, 2), RMSESD = .4)
#'
#' best(test, "RMSE", maximize = FALSE)
#' oneSE(test, "RMSE", maximize = FALSE, num = 10)
#' tolerance(test, "RMSE", tol = 3, maximize = FALSE)
#'
#' ### usage example
#'
#' data(BloodBrain)
#'
#' marsGrid <- data.frame(degree = 1, nprune = (1:10) * 3)
#'
#' set.seed(1)
#' marsFit <- train(
#'   bbbDescr,
#'   logBBB,
#'   method = "earth",
#'   tuneGrid = marsGrid,
#'   trControl = trainControl(
#'     method = "cv",
#'     number = 10,
#'     selectionFunction = "tolerance"
#'   )
#' )
#'
#' # around 18 terms should yield the smallest CV RMSE
#'
#' @export oneSE
oneSE <- function(x, metric, num, maximize) {
  index <- seq_len(nrow(x))

  if (!maximize) {
    bestIndex <- which.min(x[, metric])
    perf <- x[bestIndex, metric] +
      (x[bestIndex, paste(metric, "SD", sep = "")]) / sqrt(num)
    candidates <- index[x[, metric] <= perf]
    bestIter <- min(candidates)
  } else {
    bestIndex <- which.max(x[, metric])
    perf <- x[bestIndex, metric] -
      (x[bestIndex, paste(metric, "SD", sep = "")]) / sqrt(num)

    candidates <- index[x[, metric] >= perf]
    bestIter <- min(candidates)
  }
  bestIter
}

#' @rdname oneSE
#' @export
tolerance <- function(x, metric, tol = 1.5, maximize) {
  index <- seq_len(nrow(x))

  if (!maximize) {
    best <- min(x[, metric])
    perf <- (x[, metric] - best) / best * 100
    candidates <- index[perf < tol]
    bestIter <- min(candidates)
  } else {
    best <- max(x[, metric])
    perf <- (x[, metric] - best) / best * -100
    candidates <- index[perf < tol]
    bestIter <- min(candidates)
  }
  bestIter
}
