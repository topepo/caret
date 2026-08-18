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
