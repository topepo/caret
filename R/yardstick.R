#' Use yardstick metrics to measure performance
#'
#' `yardstickSummary()` converts a [yardstick::metric_set()] into a summary
#' function that can be passed to the `summaryFunction` argument of
#' [trainControl()] (or the `summary` element of the `functions` argument of
#' [rfeControl()] and [sbfControl()]). This gives [train()] access to all of
#' the performance metrics in the yardstick package.
#'
#' The names of the resulting performance estimates are the names of the
#' metrics (e.g. `"mcc"` or `"brier_class"`), and these should be used for the
#' `metric` argument of [train()].
#'
#' For classification models, any metrics that require class probabilities
#' (such as `roc_auc` or `brier_class`) need `classProbs = TRUE` in
#' [trainControl()]. When there are two classes, the first factor level is
#' treated as the event of interest, consistent with [twoClassSummary()].
#'
#' If case weights are passed to [train()], they are also used to compute the
#' metrics.
#'
#' By default, [train()] determines whether a metric should be maximized or
#' minimized using the yardstick metric's `direction`. A few metrics, such as
#' `mpe` and `msd`, are optimal when they are closest to zero. These can be
#' reported but should not be used to choose the tuning parameters.
#'
#' @param metrics A metric set created by [yardstick::metric_set()].
#' @return A function with arguments `data`, `lev`, and `model` that returns a
#'   named numeric vector of performance estimates.
#' @seealso [postResample()], [trainControl()]
#' @family performance
#' @keywords utilities
#' @examples
#' library(yardstick)
#'
#' reg_summary <- yardstickSummary(metric_set(rmse, ccc, huber_loss))
#'
#' set.seed(3762)
#' dat <- data.frame(obs = rnorm(20))
#' dat$pred <- dat$obs + rnorm(20, sd = 0.5)
#' reg_summary(dat)
#'
#' cls_summary <- yardstickSummary(metric_set(mcc, brier_class, roc_auc))
#'
#' classes <- c("class1", "class2")
#' dat <- data.frame(
#'   obs = factor(sample(classes, 50, replace = TRUE)),
#'   pred = factor(sample(classes, 50, replace = TRUE)),
#'   class1 = runif(50)
#' )
#' dat$class2 <- 1 - dat$class1
#' cls_summary(dat, lev = classes)
#'
#' \dontrun{
#' ctrl <- trainControl(
#'   method = "cv",
#'   classProbs = TRUE,
#'   summaryFunction = cls_summary
#' )
#' train(Class ~ ., data = twoClassSim(200), method = "lda",
#'       metric = "brier_class", trControl = ctrl)
#' }
#' @export
yardstickSummary <- function(metrics) {
  if (!inherits(metrics, "metric_set")) {
    cli::cli_abort(
      "{.arg metrics} must be a metric set created by
       {.fn yardstick::metric_set}, not {.obj_type_friendly {metrics}}."
    )
  }
  metric_info <- attr(metrics, "metrics")
  metric_types <- vapply(metric_info, function(x) class(x)[1], character(1))
  needs_probs <- any(metric_types == "prob_metric")
  is_numeric_set <- inherits(metrics, "numeric_metric_set")

  res <- function(data, lev = NULL, model = NULL) {
    if (is.character(data$obs)) {
      data$obs <- factor(data$obs, levels = lev)
    }
    is_class <- is.factor(data$obs)

    if (is_numeric_set && is_class) {
      cli::cli_abort(
        "The metric set contains regression metrics but the outcome is a
         factor."
      )
    }
    if (!is_numeric_set && !is_class) {
      cli::cli_abort(
        "The metric set contains classification metrics but the outcome is
         numeric."
      )
    }

    args <- list(data, truth = "obs")

    if (needs_probs) {
      prob_cols <- levels(data$obs)
      if (!all(prob_cols %in% colnames(data))) {
        cli::cli_abort(
          c(
            "The metric set contains metrics that require class
             probabilities.",
            "i" = "Set {.code classProbs = TRUE} in {.fn trainControl}."
          )
        )
      }
      # yardstick's binary estimators expect only the event's probabilities
      if (length(prob_cols) == 2) {
        prob_cols <- prob_cols[1]
      }
      args <- c(args, as.list(prob_cols))
    }

    args$estimate <- "pred"
    if ("weights" %in% colnames(data)) {
      args$case_weights <- "weights"
    }

    perf <- do.call(metrics, args)
    out <- perf$.estimate
    names(out) <- perf$.metric
    out
  }
  attr(res, "metric_directions") <-
    vapply(metric_info, attr, character(1), "direction")
  res
}

# The yardstick metrics that compute caret's own performance estimates. The
# "Mean_" prefix of the multiClassSummary() names is removed before matching.
caret_yardstick_names <- c(
  RMSE = "rmse",
  Rsquared = "rsq",
  MAE = "mae",
  Accuracy = "accuracy",
  Kappa = "kap",
  ROC = "roc_auc",
  AUC = "roc_auc",
  prAUC = "pr_auc",
  logLoss = "mn_log_loss",
  Sens = "sens",
  Spec = "spec",
  Sensitivity = "sens",
  Specificity = "spec",
  Pos_Pred_Value = "ppv",
  Neg_Pred_Value = "npv",
  Precision = "precision",
  Recall = "recall",
  F = "f_meas",
  F1 = "f_meas",
  Balanced_Accuracy = "bal_accuracy"
)

# Determine if smaller values of a performance metric are better. This uses
# the directions stored by yardstickSummary() and then the yardstick metric
# with the same name (or the one that computes caret's metric of that name).
is_minimized <- function(metric, summary = NULL) {
  directions <- attr(summary, "metric_directions")
  vapply(
    metric,
    function(x) {
      direction <- unname(directions[x])
      if (length(direction) == 0 || is.na(direction)) {
        direction <- yardstick_direction(to_yardstick_name(x))
      }
      if (identical(direction, "zero")) {
        cli::cli_warn(
          c(
            "The metric {.val {x}} is optimal at zero, which {.pkg caret}
             cannot optimize; larger values will be treated as better.",
            "i" = "Choose a different {.arg metric} to tune the model."
          )
        )
      }
      identical(direction, "minimize")
    },
    logical(1),
    USE.NAMES = FALSE
  )
}

to_yardstick_name <- function(metric) {
  caret_name <- sub("^Mean_", "", metric)
  if (caret_name %in% names(caret_yardstick_names)) {
    return(caret_yardstick_names[[caret_name]])
  }
  metric
}

yardstick_direction <- function(metric) {
  fn <- get0(metric, envir = asNamespace("yardstick"), inherits = FALSE)
  if (!inherits(fn, "metric")) {
    return(NA_character_)
  }
  attr(fn, "direction")
}

# caret's confusion matrices have predictions in the rows and the reference
# data in the columns. These helpers collapse one into a 2x2 table with the
# event class(es) first, the layout that yardstick's table methods expect, so
# that multiclass tables are treated as "event versus the rest".
check_conf_table <- function(data) {
  # report errors from the user-facing method (e.g. sensitivity.table())
  caller <- sys.call(-1)
  if (!isTRUE(all.equal(nrow(data), ncol(data)))) {
    stop(simpleError("the table must have nrow = ncol", call = caller))
  }
  if (!isTRUE(all.equal(rownames(data), colnames(data)))) {
    stop(simpleError(
      "the table must the same groups in the same order",
      call = caller
    ))
  }
  invisible(data)
}

event_table <- function(data, event) {
  is_event <- colnames(data) %in% event
  groups <- list(which(is_event), which(!is_event))
  counts <- matrix(0, 2, 2)
  for (i in 1:2) {
    for (j in 1:2) {
      counts[i, j] <- sum(data[groups[[i]], groups[[j]]])
    }
  }
  dimnames(counts) <- list(c("event", "other"), c("event", "other"))
  as.table(counts)
}

# Undefined values (e.g. from a zero denominator) are NA; yardstick also warns
# about them. S3 dispatch looks for methods in the calling environment first,
# where caret's own precision.table() and recall.table() would be found
# instead of yardstick's, so the generic is called from an empty environment.
table_metric <- function(fn, data, event, ...) {
  args <- list(event_table(data, event), ...)
  res <- suppressWarnings(do.call(fn, args, envir = emptyenv()))$.estimate
  if (is.nan(res)) {
    res <- NA_real_
  }
  res
}

# Validation shared by the default (factor) methods; returns the 2x2 table of
# predictions by reference values, or NULL when missing values should give NA
factor_conf_table <- function(data, reference, na.rm, msg) {
  # report errors from the user-facing method (e.g. sensitivity.default())
  caller <- sys.call(-1)
  if (!is.factor(reference) || !is.factor(data)) {
    stop(simpleError(msg, call = caller))
  }
  ## todo: relax the =2 constraint and let ngative length be > 2
  if (length(unique(c(levels(reference), levels(data)))) != 2) {
    stop(simpleError("input data must have the same two levels", call = caller))
  }
  if (!na.rm && (anyNA(data) || anyNA(reference))) {
    return(NULL)
  }
  data <- factor(data, levels = levels(reference))
  table(data, reference)
}
