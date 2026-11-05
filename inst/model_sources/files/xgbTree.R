modelInfo <- list(
  label = "eXtreme Gradient Boosting",
  library = c("xgboost", "plyr"),
  check = function(pkg) {
    requireNamespace("xgboost")
    if (utils::packageVersion("xgboost") < "3.0.0") {
      cli::cli_abort(
        "{.code method = \"xgbTree\"} requires {.pkg xgboost} 3.0.0 or later.",
        call = NULL
      )
    }
  },
  type = c("Regression", "Classification"),
  parameters = data.frame(
    parameter = c(
      'nrounds',
      'max_depth',
      'eta',
      'gamma',
      'colsample_bytree',
      'min_child_weight',
      'subsample',
      'lambda',
      'alpha'
    ),
    class = rep("numeric", 9),
    label = c(
      '# Boosting Iterations',
      'Max Tree Depth',
      'Shrinkage',
      "Minimum Loss Reduction",
      'Subsample Ratio of Columns',
      'Minimum Sum of Instance Weight',
      'Subsample Percentage',
      'L2 Regularization',
      'L1 Regularization'
    )
  ),
  # used when `lambda` or `alpha` are missing from a user's `tuneGrid`; these
  # parameters were added after many grids had been written without them
  defaults = list(lambda = 1, alpha = 0),
  grid = function(x, y, len = NULL, search = "grid") {
    if (search == "grid") {
      out <- expand.grid(
        max_depth = seq(1, len),
        nrounds = floor((1:len) * 50),
        eta = c(.3, .4),
        gamma = 0,
        colsample_bytree = c(.6, .8),
        min_child_weight = c(1),
        subsample = seq(.5, 1, length = len),
        lambda = 1,
        alpha = 0
      )
    } else {
      out <- data.frame(
        nrounds = sample(1:1000, size = len, replace = TRUE),
        max_depth = sample(1:10, replace = TRUE, size = len),
        eta = runif(len, min = .001, max = .6),
        gamma = runif(len, min = 0, max = 10),
        colsample_bytree = runif(len, min = .3, max = .7),
        min_child_weight = sample(0:20, size = len, replace = TRUE),
        subsample = runif(len, min = .25, max = 1),
        lambda = 10^runif(len, min = -5, max = 0),
        alpha = 10^runif(len, min = -5, max = 0)
      )
      out$nrounds <- floor(out$nrounds)
    }
    out
  },
  loop = function(grid) {
    loop <- plyr::ddply(
      grid,
      c(
        "eta",
        "max_depth",
        "gamma",
        "colsample_bytree",
        "min_child_weight",
        "subsample",
        "lambda",
        "alpha"
      ),
      function(x) c(nrounds = max(x$nrounds))
    )
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$nrounds)) {
      index <- which(
        grid$max_depth == loop$max_depth[i] &
          grid$eta == loop$eta[i] &
          grid$gamma == loop$gamma[i] &
          grid$colsample_bytree == loop$colsample_bytree[i] &
          grid$min_child_weight == loop$min_child_weight[i] &
          grid$subsample == loop$subsample[i] &
          grid$lambda == loop$lambda[i] &
          grid$alpha == loop$alpha[i]
      )
      trees <- grid[index, "nrounds"]
      submodels[[i]] <- data.frame(nrounds = trees[trees != loop$nrounds[i]])
    }
    list(loop = loop, submodels = submodels)
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    if (!inherits(x, "xgb.DMatrix")) {
      x <- as.matrix(x)
    }

    params <- list(
      eta = param$eta,
      max_depth = param$max_depth,
      gamma = param$gamma,
      colsample_bytree = param$colsample_bytree,
      min_child_weight = param$min_child_weight,
      subsample = param$subsample,
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
    if (!is.null(submodels)) {
      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
      tmp[[1]] <- out
      for (j in seq(along = submodels$nrounds)) {
        tmp_pred <- predict(
          modelFit,
          newdata,
          iterationrange = c(1, submodels$nrounds[j])
        )
        tmp[[j + 1]] <- to_class(tmp_pred)
      }
      out <- tmp
    }
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

    if (!is.null(submodels)) {
      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
      tmp[[1]] <- out
      for (j in seq(along = submodels$nrounds)) {
        tmp_pred <- predict(
          modelFit,
          newdata,
          iterationrange = c(1, submodels$nrounds[j])
        )
        tmp[[j + 1]] <- to_prob(tmp_pred)
      }
      out <- tmp
    }
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
    "Tree-Based Model",
    "Boosting",
    "Ensemble Model",
    "Implicit Feature Selection",
    "Accepts Case Weights",
    "L1 Regularization Models",
    "L2 Regularization Models"
  ),
  sort = function(x) {
    # This is a toss-up, but the # trees probably adds
    # complexity faster than number of splits
    x[
      order(
        x$nrounds,
        x$max_depth,
        x$eta,
        x$gamma,
        x$colsample_bytree,
        x$min_child_weight,
        x$lambda,
        x$alpha
      ),
    ]
  }
)
