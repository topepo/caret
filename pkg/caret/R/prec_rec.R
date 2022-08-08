#' Calculate recall, precision and F values
#'
#' These functions calculate the recall, precision or F values of a measurement
#' system for finding/retrieving relevant documents compared to reference
#' results (the truth regarding relevance). The measurement and "truth" data
#' must have the same two possible outcomes and one of the outcomes must be
#' thought of as a "relevant" results.
#'
#' The recall (aka sensitivity) is defined as the proportion of relevant
#' results out of the number of samples which were actually relevant. When
#' there are no relevant results, recall is not defined and a value of
#' \code{NA} is returned.
#'
#' The precision is percentage of predicted truly relevant results of the total
#' number of predicted relevant results and characterizes the "purity in
#' retrieval performance" (Buckland and Gey, 1994)
#'
#' The measure "F" is a combination of precision and recall (see below).
#'
#' Suppose a 2x2 table with notation
#'
#' \tabular{rcc}{ \tab Reference \tab \cr Predicted \tab relevant \tab
#' Irrelevant \cr relevant \tab A \tab B \cr Irrelevant \tab C \tab D \cr }
#'
#' The formulas used here are: \deqn{recall = A/(A+C)} \deqn{precision =
#' A/(A+B)} \deqn{F_i = (1+i^2)*prec*recall/((i^2 * precision)+recall)}
#'
#' See the references for discussions of the statistics.
#'
#' @aliases recall recall.default recall.table precision precision.default
#' precision.table precision.matrix F_meas F_meas.default F_meas.table
#' @param data for the default functions, a factor containing the discrete
#' measurements. For the \code{table} function, a table.
#' @param reference a factor containing the reference values (i.e. truth)
#' @param relevant a character string that defines the factor level
#' corresponding to the "relevant" results
#' @param beta a numeric value used to weight precision and recall. A value of
#' 1 is traditionally used and corresponds to the harmonic mean of the two
#' values but other values weight recall beta times more important than
#' precision.
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
#' Buckland, M., & Gey, F. (1994). The relationship between Recall and
#' Precision. \emph{Journal of the American Society for Information Science},
#' 45(1), 12-19.
#'
#' Powers, D. (2007). Evaluation: From Precision, Recall and F Factor to ROC,
#' Informedness, Markedness and Correlation. Technical Report SIE-07-001,
#' Flinders University
#' @keywords manip
#' @examples
#'
#' ###################
#' ## Data in Table 2 of Powers (2007)
#'
#' lvs <- c("Relevant", "Irrelevant")
#' tbl_2_1_pred <- factor(rep(lvs, times = c(42, 58)), levels = lvs)
#' tbl_2_1_truth <- factor(c(rep(lvs, times = c(30, 12)),
#'                           rep(lvs, times = c(30, 28))),
#'                         levels = lvs)
#' tbl_2_1 <- table(tbl_2_1_pred, tbl_2_1_truth)
#'
#' precision(tbl_2_1)
#' precision(data = tbl_2_1_pred, reference = tbl_2_1_truth, relevant = "Relevant")
#' recall(tbl_2_1)
#' recall(data = tbl_2_1_pred, reference = tbl_2_1_truth, relevant = "Relevant")
#'
#'
#' tbl_2_2_pred <- factor(rep(lvs, times = c(76, 24)), levels = lvs)
#' tbl_2_2_truth <- factor(c(rep(lvs, times = c(56, 20)),
#'                           rep(lvs, times = c(12, 12))),
#'                         levels = lvs)
#' tbl_2_2 <- table(tbl_2_2_pred, tbl_2_2_truth)
#'
#' precision(tbl_2_2)
#' precision(data = tbl_2_2_pred, reference = tbl_2_2_truth, relevant = "Relevant")
#' recall(tbl_2_2)
#' recall(data = tbl_2_2_pred, reference = tbl_2_2_truth, relevant = "Relevant")
#'
#' @export recall
recall <- function(data, ...) UseMethod("recall")

#' @rdname recall
#' @export
"recall.table" <- function(data, relevant = rownames(data)[1], ...){
  if(!all.equal(nrow(data), ncol(data))) stop("the table must have nrow = ncol")
  if(!all.equal(rownames(data), colnames(data))) stop("the table must the same groups in the same order")

  if(nrow(data) > 2) {
    tmp <- data
    data <- matrix(NA, 2, 2)

    colnames(data) <- rownames(data) <- c("rel", "irrel")
    irrelCol <- which(!(colnames(tmp) %in% relevant))
    relCol <- which(colnames(tmp) %in% relevant)

    data[1, 1] <- sum(tmp[relCol, relCol])
    data[1, 2] <- sum(tmp[relCol, irrelCol])
    data[2, 1] <- sum(tmp[irrelCol, relCol])
    data[2, 2] <- sum(tmp[irrelCol, irrelCol])
    data <- as.table(data)
    relevant <- "rel"
    rm(tmp)
  }
  numer <- data[relevant, relevant]
  denom <- sum(data[, relevant])
  rec <- ifelse(denom > 0, numer / denom, NA)
  rec
}

#' @rdname recall
#' @importFrom stats complete.cases
#' @export
recall.default <- function(data, reference, relevant = levels(reference)[1],
                           na.rm = TRUE, ...) {
  if (!is.factor(reference) | !is.factor(data))
    stop("input data must be a factor")
  if (length(unique(c(levels(reference), levels(data)))) != 2)
    stop("input data must have the same two levels")
  if (na.rm) {
    cc <- complete.cases(data) & complete.cases(reference)
    if (any(!cc)) {
      data <- data[cc]
      reference <- reference[cc]
    }
  }
  xtab <- table(data, reference)
  recall.table(xtab, relevant = relevant)
}

#' @rdname recall
#' @export
precision <- function(data, ...) UseMethod("precision")

#' @rdname recall
#' @importFrom stats complete.cases
#' @export
precision.default <- function(data, reference, relevant = levels(reference)[1],
                              na.rm = TRUE, ...) {
  if (!is.factor(reference) | !is.factor(data))
    stop("input data must be a factor")
  if (length(unique(c(levels(reference), levels(data)))) != 2)
    stop("input data must have the same two levels")
  if (na.rm) {
    cc <- complete.cases(data) & complete.cases(reference)
    if (any(!cc)) {
      data <- data[cc]
      reference <- reference[cc]
    }
  }
  xtab <- table(data, reference)
  precision.table(xtab, relevant = relevant)
}

#' @rdname recall
#' @export
precision.table <- function (data, relevant = rownames(data)[1], ...) {
  if (!all.equal(nrow(data), ncol(data)))
    stop("the table must have nrow = ncol")
  if (!all.equal(rownames(data), colnames(data)))
    stop("the table must the same groups in the same order")
  if (nrow(data) > 2) {
    tmp <- data
    data <- matrix(NA, 2, 2)
    colnames(data) <- rownames(data) <- c("rel", "irrel")
    irrelCol <- which(!(colnames(tmp) %in% relevant))
    relCol <- which(colnames(tmp) %in% relevant)
    data[1, 1] <- sum(tmp[relCol, relCol])
    data[1, 2] <- sum(tmp[relCol, irrelCol])
    data[2, 1] <- sum(tmp[irrelCol, relCol])
    data[2, 2] <- sum(tmp[irrelCol, irrelCol])
    data <- as.table(data)
    relevant <- "rel"
    relevant
    rm(tmp)
  }
  numer <- data[relevant, relevant]
  denom <- sum(data[relevant, ])
  spec <- ifelse(denom > 0, numer/denom, NA)
  spec
}

#' @rdname recall
#' @export
F_meas <- function(data, ...) UseMethod("F_meas")

#' @rdname recall
#' @importFrom stats complete.cases
#' @export
F_meas.default <- function(data, reference, relevant = levels(reference)[1],
                           beta = 1,  na.rm = TRUE, ...) {
  if (!is.factor(reference) | !is.factor(data))
    stop("input data must be a factor")
  if (length(unique(c(levels(reference), levels(data)))) != 2)
    stop("input data must have the same two levels")
  if (na.rm) {
    cc <- complete.cases(data) & complete.cases(reference)
    if (any(!cc)) {
      data <- data[cc]
      reference <- reference[cc]
    }
  }
  xtab <- table(data, reference)
  F_meas.table(xtab, relevant = relevant, beta = beta)
}

#' @rdname recall
#' @export
F_meas.table <- function (data, relevant = rownames(data)[1], beta = 1, ...) {
  prec <- precision.table(data, relevant = relevant)
  rec <- recall.table(data, relevant = relevant)
  (1+beta^2)*prec*rec/((beta^2 * prec)+rec)
}

#' @rdname postResample
#' @export
prSummary <- function (data, lev = NULL, model = NULL)  {

  requireNamespaceQuietStop("MLmetrics")
  if (length(levels(data$obs)) > 2)
    stop(paste("Your outcome has", length(levels(data$obs)),
               "levels. `prSummary`` function isn't appropriate.",
         call. = FALSE))
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
    stop("Levels of observed and predicted data do not match.",
         call. = FALSE)
  if (!lev[1] %in% colnames(data))
    stop(paste("Class probabilities are needed to score models using the",
               "area under the PR curve. Set `classProbs = TRUE`",
               "in the trainControl() function."),
         call. = FALSE)

  pr_auc <-
    try(MLmetrics::PRAUC(y_pred = data[, lev[1]],
                         y_true = ifelse(data$obs == lev[1], 1, 0)),
        silent = TRUE)
  if(inherits(pr_auc, "try-error"))
    pr_auc <- NA

  c(AUC = pr_auc,
    Precision = precision.default(data = data$pred, reference = data$obs, relevant = lev[1]),
    Recall = recall.default(data = data$pred, reference = data$obs, relevant = lev[1]),
    F = F_meas.default(data = data$pred, reference = data$obs, relevant = lev[1]))
}

