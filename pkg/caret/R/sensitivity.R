#' Calculate sensitivity, specificity and predictive values
#'
#' These functions calculate the sensitivity, specificity or predictive values
#' of a measurement system compared to a reference results (the truth or a gold
#' standard). The measurement and "truth" data must have the same two possible
#' outcomes and one of the outcomes must be thought of as a "positive" results.
#'
#' The sensitivity is defined as the proportion of positive results out of the
#' number of samples which were actually positive. When there are no positive
#' results, sensitivity is not defined and a value of \code{NA} is returned.
#' Similarly, when there are no negative results, specificity is not defined
#' and a value of \code{NA} is returned. Similar statements are true for
#' predictive values.
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
#' @aliases sensitivity sensitivity.default sensitivity.table
#' sensitivity.matrix specificity specificity.default specificity.table
#' specificity.matrix posPredValue posPredValue.default posPredValue.table
#' posPredValue.matrix negPredValue negPredValue.default negPredValue.table
#' negPredValue.matrix
#' @param data for the default functions, a factor containing the discrete
#' measurements. For the \code{table} or \code{matrix} functions, a table or
#' matric object, respectively.
#' @param reference a factor containing the reference values
#' @param positive a character string that defines the factor level
#' corresponding to the "positive" results
#' @param negative a character string that defines the factor level
#' corresponding to the "negative" results
#' @param prevalence a numeric value for the rate of the "positive" class of
#' the data
#' @param na.rm a logical value indicating whether \code{NA} values should be
#' stripped before the computation proceeds
#' @param ... not currently used
#' @return A number between 0 and 1 (or NA).
#' @author Max Kuhn
#' @seealso \code{\link{confusionMatrix}}
#' @references Kuhn, M. (2008), ``Building predictive models in R using the
#' caret package, '' \emph{Journal of Statistical Software},
#' (\doi{10.18637/jss.v028.i05}).
#'
#' Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 1: sensitivity and
#' specificity,'' \emph{British Medical Journal}, vol 308, 1552.
#'
#' Altman, D.G., Bland, J.M. (1994) ``Diagnostic tests 2: predictive values,''
#' \emph{British Medical Journal}, vol 309, 102.
#' @keywords manip
#' @examples
#'
#' \dontrun{
#' ###################
#' ## 2 class example
#'
#' lvs <- c("normal", "abnormal")
#' truth <- factor(rep(lvs, times = c(86, 258)),
#'                 levels = rev(lvs))
#' pred <- factor(
#'                c(
#'                  rep(lvs, times = c(54, 32)),
#'                  rep(lvs, times = c(27, 231))),
#'                levels = rev(lvs))
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
#'
#' prev <- seq(0.001, .99, length = 20)
#' npvVals <- ppvVals <- prev  * NA
#' for(i in seq(along = prev))
#'   {
#'     ppvVals[i] <- posPredValue(pred, truth, prevalence = prev[i])
#'     npvVals[i] <- negPredValue(pred, truth, prevalence = prev[i])
#'   }
#'
#' plot(prev, ppvVals,
#'      ylim = c(0, 1),
#'      type = "l",
#'      ylab = "",
#'      xlab = "Prevalence (i.e. prior)")
#' points(prev, npvVals, type = "l", col = "red")
#' abline(h=sensitivity(pred, truth), lty = 2)
#' abline(h=specificity(pred, truth), lty = 2, col = "red")
#' legend(.5, .5,
#'        c("ppv", "npv", "sens", "spec"),
#'        col = c("black", "red", "black", "red"),
#'        lty = c(1, 1, 2, 2))
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
#' sensitivity(model, iris$Species)
#'
#' ## When passing a table, more than two levels can
#' ## be used
#' sensitivity(irisTabs, "versicolor")
#' specificity(irisTabs, c("setosa", "virginica"))
#' }
#'
#' @export sensitivity
sensitivity <-
  function(data, ...){
    UseMethod("sensitivity")
  }

#' @rdname sensitivity
#' @importFrom stats complete.cases
#' @export
"sensitivity.default" <-
  function(data, reference, positive = levels(reference)[1], na.rm = TRUE, ...)
{
  if(!is.factor(reference) | !is.factor(data))
    stop("inputs must be factors")

  ## todo: relax the =2 constraint and let ngative length be > 2
  if(length(unique(c(levels(reference), levels(data)))) != 2)
    stop("input data must have the same two levels")
  if(na.rm)
    {
      cc <- complete.cases(data) & complete.cases(reference)
      if(any(!cc))
        {
          data <- data[cc]
          reference <- reference[cc]
        }
    }
  numer <- sum(data %in% positive & reference %in% positive)
  denom <- sum(reference %in% positive)
  sens <- ifelse(denom > 0, numer / denom, NA)
  sens
}

#' @rdname sensitivity
#' @export
"sensitivity.table" <-
  function(data, positive = rownames(data)[1], ...)
{
  ## "truth" in columns, predictions in rows
  if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
  if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")

  if(nrow(data) > 2)
    {
      tmp <- data
      data <- matrix(NA, 2, 2)

      colnames(data) <- rownames(data) <- c("pos", "neg")
      posCol <- which(colnames(tmp) %in% positive)
      negCol <- which(!(colnames(tmp) %in% positive))

      data[1, 1] <- sum(tmp[posCol, posCol])
      data[1, 2] <- sum(tmp[posCol, negCol])
      data[2, 1] <- sum(tmp[negCol, posCol])
      data[2, 2] <- sum(tmp[negCol, negCol])
      data <- as.table(data)
      positive <- "pos"
      rm(tmp)
    }

  numer <- sum(data[positive, positive])
  denom <- sum(data[, positive])
  sens <- ifelse(denom > 0, numer / denom, NA)
  sens
}

#' @rdname sensitivity
"sensitivity.matrix" <-
  function(data, positive = rownames(data)[1], ...)
{
  data <- as.table(data)
  sensitivity.table(data)
}
