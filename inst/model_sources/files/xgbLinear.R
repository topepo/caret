modelInfo <- list(
  label = "eXtreme Gradient Boosting",
  library = c("xgboost"),
  check = function(pkg) {
    requireNamespace("xgboost")
    if (utils::packageVersion("xgboost") < "3.0.0") {
      cli::cli_abort(
        "{.code method = \"xgbLinear\"} requires {.pkg xgboost} 3.0.0 or later.",
        call = NULL
      )
    }
  },
  type = c("Regression", "Classification"),
  parameters = data.frame(
    parameter = c('nrounds', 'lambda', 'alpha', 'eta'),
    class = rep("numeric", 4),
    label = c(
      '# Boosting Iterations',
      'L2 Regularization',
      'L1 Regularization',
      'Learning Rate'
    )
  ),
  grid = function(x, y, len = NULL, search = "grid") {
    if (search == "grid") {
      out <- expand.grid(
        lambda = c(0, 10^seq(-1, -4, length = len - 1)),
        alpha = c(0, 10^seq(-1, -4, length = len - 1)),
        nrounds = floor((1:len) * 50),
        eta = 0.3
      )
    } else {
      out <- data.frame(
        lambda = 10^runif(len, min = -5, 0),
        alpha = 10^runif(len, min = -5, 0),
        nrounds = sample(1:100, size = len, replace = TRUE),
        eta = runif(len, max = 3)
      )
    }
    out
  },
  loop = NULL,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    if (!inherits(x, "xgb.DMatrix")) {
      x <- as.matrix(x)
    }

    params <- list(
      lambda = param$lambda,
      alpha = param$alpha
    )

    if (is.factor(y)) {
      if (length(lev) == 2) {
        y <- ifelse(y == lev[1], 1, 0)
        params$objective <- "binary:logistic"
      } else {
        y <- as.numeric(y) - 1
        params$objective <- "multi:softprob"
        params$num_class <- length(lev)
      }
    } else {
      params$objective <- "reg:squarederror"
    }

    if (!inherits(x, "xgb.DMatrix")) {
      x <- xgboost::xgb.DMatrix(x, label = y, missing = NA)
    } else {
      xgboost::setinfo(x, "label", y)
    }

    if (!is.null(wts)) {
      xgboost::setinfo(x, 'weight', wts)
    }

    # xgboost >= 3 wants all booster options in `params`, so options passed
    # through `...` that aren't arguments to `xgb.train()` are moved there
    dots <- list(...)
    dot_names <- names(dots)
    if (is.null(dot_names)) {
      dot_names <- rep("", length(dots))
    }
    is_param <- !(dot_names %in% names(formals(xgboost::xgb.train)))
    if (is.character(dots$objective)) {
      is_param[dot_names == "objective"] <- TRUE
    }
    if (is.function(dots$objective)) {
      # a custom objective function replaces the built-in one
      params$objective <- NULL
    }
    params[dot_names[is_param]] <- dots[is_param]

    xgb_train <- function(...) {
      xgboost::xgb.train(
        params = params,
        data = x,
        nrounds = param$nrounds,
        ...
      )
    }
    do.call(xgb_train, dots[!is_param])
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    if (!inherits(newdata, "xgb.DMatrix")) {
      newdata <- as.matrix(newdata)
      newdata <- xgboost::xgb.DMatrix(data = newdata, missing = NA)
    }
    lev <- attr(modelFit, "obsLevels")
    to_class <- function(pred) {
      if (attr(modelFit, "problemType") != "Classification") {
        return(pred)
      }
      if (length(lev) == 2) {
        return(ifelse(pred >= .5, lev[1], lev[2]))
      }
      # multiclass predictions are a matrix with a column per class
      lev[apply(pred, 1, which.max)]
    }
    out <- to_class(predict(modelFit, newdata))
    out
  },
  prob = function(modelFit, newdata, submodels = NULL) {
    if (!inherits(newdata, "xgb.DMatrix")) {
      newdata <- as.matrix(newdata)
      newdata <- xgboost::xgb.DMatrix(data = newdata, missing = NA)
    }
    lev <- attr(modelFit, "obsLevels")
    logit_raw <- identical(
      attr(modelFit, "params")$objective,
      "binary:logitraw"
    )
    to_prob <- function(pred) {
      if (logit_raw) {
        pred <- binomial()$linkinv(pred) # exp(p)/(1+exp(p))
      }
      if (length(lev) == 2) {
        pred <- cbind(pred, 1 - pred)
      }
      colnames(pred) <- lev
      as.data.frame(pred, stringsAsFactors = TRUE)
    }
    out <- to_prob(predict(modelFit, newdata))
    out
  },
  predictors = function(x, ...) {
    x_names <- attr(x, "xNames")
    imp <- xgboost::xgb.importance(model = x, feature_names = x_names)
    x_names[x_names %in% imp$Feature]
  },
  varImp = function(object, numTrees = NULL, ...) {
    x_names <- attr(object, "xNames")
    imp <- xgboost::xgb.importance(model = object, feature_names = x_names)
    imp <- as.data.frame(imp, stringsAsFactors = TRUE)[, 1:2]
    rownames(imp) <- as.character(imp[, 1])
    imp <- imp[, 2, drop = FALSE]
    colnames(imp) <- "Overall"

    missing <- x_names[!(x_names %in% rownames(imp))]
    missing_imp <- data.frame(Overall = rep(0, times = length(missing)))
    rownames(missing_imp) <- missing
    imp <- rbind(imp, missing_imp)

    imp
  },
  levels = function(x) attr(x, "obsLevels"),
  tags = c(
    "Linear Classifier Models",
    "Linear Regression Models",
    "L1 Regularization Models",
    "L2 Regularization Models",
    "Boosting",
    "Ensemble Model",
    "Implicit Feature Selection"
  ),
  sort = function(x) {
    # This is a toss-up, but the # trees probably adds
    # complexity faster than number of splits
    x[order(x$nrounds, x$alpha, x$lambda), ]
  }
)
