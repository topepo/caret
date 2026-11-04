#' Calculate sensitivity, specificity and predictive values
#'
#' These functions calculate the sensitivity, specificity or predictive values
#' of a measurement system compared to a reference results (the truth or a gold
#' standard). The measurement and "truth" data must have the same two possible
#' outcomes and one of the outcomes must be thought of as a "positive" results.
#'
#' The sensitivity is defined as the proportion of positive results out of the
#' number of samples which were actually positive. When there are no positive
#' results, sensitivity is not defined and a value of `NA` is returned.
#' Similarly, when there are no negative results, specificity is not defined
#' and a value of `NA` is returned. Similar statements are true for predictive
#' values.
#'
#' The positive predictive value is defined as the percent of predicted
#' positives that are actually positive while the negative predictive value is
#' defined as the percent of negative positives that are actually negative.
#'
#' Suppose a 2x2 table with notation
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Event \tab No Event
#' \cr Event \tab A \tab B \cr No Event \tab C \tab D \cr }
#'
#' The formulas used here are: \deqn{Sensitivity = A/(A+C)} \deqn{Specificity =
#' D/(B+D)} \deqn{Prevalence = (A+C)/(A+B+C+D)} \deqn{PPV = (sensitivity *
#' Prevalence)/((sensitivity*Prevalence) + ((1-specificity)*(1-Prevalence)))}
#' \deqn{NPV = (specificity * (1-Prevalence))/(((1-sensitivity)*Prevalence) +
#' ((specificity)*(1-Prevalence)))}
#'
#' See the references for discussions of the statistics.
#'
#' @aliases sensitivity sensitivity.default sensitivity.table sensitivity.matrix specificity specificity.default specificity.table specificity.matrix posPredValue posPredValue.default posPredValue.table posPredValue.matrix negPredValue negPredValue.default negPredValue.table negPredValue.matrix
#' @param data for the default functions, a factor containing the discrete
#'   measurements. For the `table` or `matrix` functions, a table or matric
#'   object, respectively.
#' @param reference a factor containing the reference values
#' @param positive a character string that defines the factor level
#'   corresponding to the "positive" results
#' @param negative a character string that defines the factor level
#'   corresponding to the "negative" results
#' @param prevalence a numeric value for the rate of the "positive" class of
#'   the data
#' @param na.rm a logical value indicating whether `NA` values should be
#'   stripped before the computation proceeds
#' @param ... not currently used
#' @return A number between 0 and 1 (or NA).
#' @author Max Kuhn
#' @seealso [confusionMatrix()]
#' @references Kuhn, M. (2008), "Building Predictive Models in R Using the
#'   caret Package," *Journal of Statistical Software*, 28(5), 1-26.
#'   (\doi{10.18637/jss.v028.i05}).
#'
#' Altman, D.G., Bland, J.M. (1994) "Diagnostic tests 1: sensitivity and
#' specificity," *British Medical Journal*, vol 308, 1552.
#'
#' Altman, D.G., Bland, J.M. (1994) "Diagnostic tests 2: predictive values,"
#' *British Medical Journal*, vol 309, 102.
#' @family performance
#' @keywords manip
#' @examplesIf !caret:::is_cran_check()
#'
#' ###################
#' ## 2 class example
#'
#' lvs <- c("normal", "abnormal")
#' truth <- factor(rep(lvs, times = c(86, 258)), levels = rev(lvs))
#' pred <- factor(
#'   c(
#'     rep(lvs, times = c(54, 32)),
#'     rep(lvs, times = c(27, 231))
#'   ),
#'   levels = rev(lvs)
#' )
#'
#' xtab <- table(pred, truth)
#'
#' sensitivity(pred, truth)
#' sensitivity(xtab)
#' posPredValue(pred, truth)
#' posPredValue(pred, truth, prevalence = 0.25)
#'
#' specificity(pred, truth)
#' negPredValue(pred, truth)
#' negPredValue(xtab)
#' negPredValue(pred, truth, prevalence = 0.25)
#'
#' prev <- seq(0.001, .99, length = 20)
#' npvVals <- ppvVals <- prev * NA
#' for (i in seq(along.with = prev)) {
#'   ppvVals[i] <- posPredValue(pred, truth, prevalence = prev[i])
#'   npvVals[i] <- negPredValue(pred, truth, prevalence = prev[i])
#' }
#'
#' plot(
#'   prev,
#'   ppvVals,
#'   ylim = c(0, 1),
#'   type = "l",
#'   ylab = "",
#'   xlab = "Prevalence (i.e. prior)"
#' )
#' points(prev, npvVals, type = "l", col = "red")
#' abline(h = sensitivity(pred, truth), lty = 2)
#' abline(h = specificity(pred, truth), lty = 2, col = "red")
#' legend(
#'   .5,
#'   .5,
#'   c("ppv", "npv", "sens", "spec"),
#'   col = c("black", "red", "black", "red"),
#'   lty = c(1, 1, 2, 2)
#' )
#'
#' ###################
#' ## 3 class example
#'
#' library(MASS)
#'
#' fit <- lda(Species ~ ., data = iris)
#' model <- predict(fit)$class
#'
#' irisTabs <- table(model, iris$Species)
#'
#' ## When passing factors, an error occurs with more
#' ## than two levels
#' try(sensitivity(model, iris$Species))
#'
#' ## When passing a table, more than two levels can
#' ## be used
#' sensitivity(irisTabs, "versicolor")
#' specificity(irisTabs, c("setosa", "virginica"))
#'
#' @export sensitivity
sensitivity <-
  function(data, ...) {
    UseMethod("sensitivity")
  }

#' @rdname sensitivity
#' @export
"sensitivity.default" <-
  function(
    data,
    reference,
    positive = levels(reference)[1],
    na.rm = TRUE,
    ...
  ) {
    xtab <- factor_conf_table(data, reference, na.rm, "inputs must be factors")
    if (is.null(xtab)) {
      return(NA_real_)
    }
    sensitivity.table(xtab, positive = positive)
  }

#' @rdname sensitivity
#' @export
"sensitivity.table" <-
  function(data, positive = rownames(data)[1], ...) {
    ## "truth" in columns, predictions in rows
    check_conf_table(data)
    table_metric(yardstick::sens, data, positive)
  }

#' @rdname sensitivity
#' @export
"sensitivity.matrix" <-
  function(data, positive = rownames(data)[1], ...) {
    sensitivity.table(as.table(data), positive = positive)
  }
