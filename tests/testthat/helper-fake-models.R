# Custom `method` lists for the train()/workflow tests.
#
# A custom model is the cleanest way to reach branches that the built-in models
# cannot: a fit that fails in chosen resamples exercises the failure handling in
# the workflows, and a `fit` that tolerates `...` lets `train(testing = TRUE)`
# flow its debug flag through to the workflow.

# A two-parameter linear model. Two parameters matter because adaptive
# resampling refuses a single-row grid, and because the submodel and faceting
# code paths need more than one.
#
# `fail_when` is given the fitted subset and returns TRUE when the fit should
# error; `pred_fails` makes prediction error instead.
make_custom_model <- function(fail_when = NULL, pred_fails = FALSE) {
  list(
    library = NULL,
    type = c("Regression", "Classification"),
    parameters = data.frame(
      parameter = c("shift", "scale"),
      class = c("numeric", "numeric"),
      label = c("Shift", "Scale"),
      stringsAsFactors = FALSE
    ),
    grid = function(x, y, len = NULL, search = "grid") {
      expand.grid(shift = seq_len(max(len, 2L)), scale = 1)
    },
    # `...` is accepted and ignored, so extra arguments passed through train()
    # (such as testing = TRUE) do not upset the fit
    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
      if (!is.null(fail_when) && isTRUE(fail_when(x, y))) {
        stop("fit failed on purpose", call. = FALSE)
      }
      # caret stores the tuning values on the fit as `tuneValue`, which is
      # what predict() below reads
      structure(list(lev = lev, n = nrow(x)), class = "custom_fit")
    },
    predict = function(modelFit, newdata, submodels = NULL) {
      if (pred_fails) {
        stop("predict failed on purpose", call. = FALSE)
      }
      n <- nrow(newdata)
      # caret sets problemType on the fit; branching on it is more reliable
      # than inspecting the class levels
      if (identical(modelFit$problemType, "Classification")) {
        factor(rep(modelFit$lev[1], n), levels = modelFit$lev)
      } else {
        rep(modelFit$tuneValue$shift, n)
      }
    },
    prob = function(modelFit, newdata, submodels = NULL) {
      n <- nrow(newdata)
      out <- matrix(
        1 / length(modelFit$lev),
        nrow = n,
        ncol = length(modelFit$lev)
      )
      colnames(out) <- modelFit$lev
      as.data.frame(out, stringsAsFactors = FALSE)
    },
    sort = function(x) x[order(x$shift), ],
    levels = function(x) x$lev
  )
}

# A custom model that scores several tuning parameters from a single fit, so the
# sub-model branches of the workflows are reachable, and that can be made to
# fail for exactly one resample.
#
# The trick for "exactly one" is a sentinel: one row of the data carries a
# predictor value no other row has, and the model fails when it does not see it.
# Whichever fold holds that row out is the one that fails, however the folds
# happen to fall, so the tests do not depend on fold sizes or ordering.
#
# `fail_fit` fails the fit, `fail_pred` fails the predictions; `sentinel` is the
# value to look for (see engine_sentinel_data()).
make_submodel_model <- function(
  fail_fit = FALSE,
  fail_pred = FALSE,
  sentinel = 999
) {
  saw_sentinel <- function(x) any(unlist(x, use.names = FALSE) >= sentinel)

  list(
    library = NULL,
    type = c("Regression", "Classification"),
    parameters = data.frame(
      parameter = c("shift", "scale"),
      class = c("numeric", "numeric"),
      label = c("Shift", "Scale"),
      stringsAsFactors = FALSE
    ),
    grid = function(x, y, len = NULL, search = "grid") {
      expand.grid(shift = seq_len(max(len, 2L)), scale = 1)
    },
    # one fit per `scale`, with the `shift` values scored from it
    loop = function(grid) {
      grid <- grid[order(grid$shift), , drop = FALSE]
      list(
        loop = grid[1, , drop = FALSE],
        submodels = list(grid[-1, , drop = FALSE])
      )
    },
    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
      complete <- saw_sentinel(x)
      if (fail_fit && !complete) {
        stop("fit failed on purpose", call. = FALSE)
      }
      structure(
        list(lev = lev, complete = complete),
        levels = lev,
        class = "custom_submodel_fit"
      )
    },
    predict = function(modelFit, newdata, submodels = NULL) {
      if (fail_pred && !modelFit$complete) {
        stop("predict failed on purpose", call. = FALSE)
      }
      one <- function(value) {
        # caret passes `lev = NA` (not NULL) for a numeric outcome, so branch on
        # problemType, which createModel() sets on the fit - see caret#1516
        if (identical(modelFit$problemType, "Classification")) {
          factor(
            rep(modelFit$lev[1], nrow(newdata)),
            levels = modelFit$lev
          )
        } else {
          rep(value, nrow(newdata))
        }
      }
      # each candidate predicts its own `shift`, so the results tell them apart
      if (is.null(submodels)) {
        one(modelFit$tuneValue$shift)
      } else {
        # the loop's own row first, then one set per sub-model
        c(list(one(modelFit$tuneValue$shift)), lapply(submodels$shift, one))
      }
    },
    prob = function(modelFit, newdata, submodels = NULL) {
      one <- function(...) {
        out <- matrix(
          1 / length(modelFit$lev),
          nrow = nrow(newdata),
          ncol = length(modelFit$lev)
        )
        colnames(out) <- modelFit$lev
        as.data.frame(out, stringsAsFactors = FALSE)
      }
      if (is.null(submodels)) {
        one()
      } else {
        c(list(one()), lapply(seq_len(nrow(submodels)), one))
      }
    },
    sort = function(x) x[order(x$shift), ],
    levels = function(x) x$lev
  )
}
