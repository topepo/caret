modelInfo <- list(
  library = "pre",
  type = c("Classification", "Regression"),
  parameters = data.frame(parameter = c("sampfrac", "maxdepth",
                                        "learnrate", "mtry",
                                        "use.grad",
                                        "penalty.par.val"),
                          class = c(rep("numeric", times = 4),
                                    "logical", "character"),
                          label = c("Subsampling Fraction",
                                    "Max Tree Depth",
                                    "Shrinkage",
                                    "# Randomly Selected Predictors",
                                    "Employ Gradient Boosting",
                                    "Regularization Parameter")),
  grid = function(x, y, len = NULL, search = "grid",
                  sampfrac = .5, maxdepth = 3L, learnrate = .01,
                  mtry = Inf, use.grad = TRUE, penalty.par.val = "lambda.1se") {
    if (search == "grid") {
      if (!is.null(len)) {
        maxdepth <- c(3L, 4L, 2L, 5L, 1L, 6:len)[1:len]
        if (len > 2) {
          sampfrac <- c(.5, .75, 1)
        }
        if (len > 1) {
          penalty.par.val = c("lambda.min", "lambda.1se")
        }
      }
      out <- expand.grid(sampfrac = sampfrac, maxdepth = maxdepth,
                         learnrate = learnrate, mtry = mtry,
                         use.grad = use.grad,
                         penalty.par.val = penalty.par.val)
    } else if (search == "random") {
      out <- data.frame(
        sampfrac = sample(c(.5, .75, 1), size = len, replace = TRUE),
        maxdepth = sample(2L:6L, size = len, replace = TRUE),
        learnrate = sample(c(0.001, 0.01, 0.1), size = len, replace = TRUE),
        mtry = sample(c(ceiling(sqrt(ncol(x))), ceiling(ncol(x)/3), ncol(x)), size = len, replace = TRUE),
        use.grad = sample(c(TRUE, FALSE), size = len, replace = TRUE),
        penalty.par.val = sample(c("lambda.1se", "lambda.min"), size = len, replace = TRUE))
    }
    return(out)
  },
  fit = function(x, y, wts = NULL, param, lev = NULL, last = NULL,
                 weights = NULL, classProbs, ...) {
    theDots <- list(...)
    if(!any(names(theDots) == "family")) {
      theDots$family <- if (is.factor(y)) {
        if (nlevels(y) == 2L) {
          "binomial"
        } else {
          "multinomial"
        }
      } else {
        "gaussian"
      }
    }
    data <- data.frame(x, .outcome = y)
    formula <- .outcome ~ .
    if (is.null(weights)) { weights <- rep(1, times = nrow(x)) }
    pre(formula = formula, data = data, weights = weights,
        sampfrac = param$sampfrac, maxdepth = param$maxdepth,
        learnrate = param$learnrate, mtry = param$mtry,
        use.grad = param$use.grad, ...)
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    if (is.null(submodels)) {
      if (modelFit$family %in% c("gaussian", "mgaussian")) {
        out <- pre:::predict.pre(object = modelFit,
                                 newdata = as.data.frame(newdata))
      } else if (modelFit$family == "poisson") {
        out <- pre:::predict.pre(object = modelFit,
                                 newdata = as.data.frame(newdata), type = "response")
      } else {
        out <- factor(pre:::predict.pre(object = modelFit,
                                        newdata = as.data.frame(newdata), type = "class"))
      }
    } else {
      out <- list()
      for (i in seq(along.with = submodels$penalty.par.val)) {
        if (modelFit$family %in% c("gaussian", "mgaussian")) {
          out[[i]] <- pre:::predict.pre(object = modelFit,
                                        newdata = as.data.frame(newdata),
                                        penalty.par.val = as.character(submodels$penalty.par.val[i]))
        } else if (modelFit$family == "poisson") {
          out[[i]] <- pre:::predict.pre(object = modelFit,
                                        newdata = as.data.frame(newdata),
                                        type = "response",
                                        penalty.par.val = as.character(submodels$penalty.par.val[i]))
        } else {
          out[[i]] <- factor(pre:::predict.pre(object = modelFit,
                                               newdata = as.data.frame(newdata),
                                               type = "class",
                                               penalty.par.val = as.character(submodels$penalty.par.val[i])))
        }
      }
    }
    out
  },
  prob = function(modelFit, newdata, submodels = NULL) {
    if (is.null(submodels)) {
      probs <- pre:::predict.pre(object = modelFit,
                                 newdata = as.data.frame(newdata),
                                 type = "response")
      # For binary classification, create matrix:
      if (is.null(ncol(probs)) || ncol(probs) == 1) {
        probs <- data.frame(1 - probs, probs)
        colnames(probs) <- levels(modelFit$data[,modelFit$y_names])
      }
    } else {
      probs <- list()
      for (i in seq(along.with = submodels$penalty.par.val)) {
        probs[[i]] <- pre:::predict.pre(object = modelFit,
                                   newdata = as.data.frame(newdata),
                                   type = "response",
                                   penalty.par.val = as.character(submodels$penalty.par.val[i]))
        # For binary classification, create matrix:
        if (is.null(ncol(probs[[i]])) || ncol(probs[[i]]) == 1) {
          probs[[i]] <- data.frame(1 - probs[[i]], probs[[i]])
          colnames(probs[[i]]) <- levels(modelFit$data[,modelFit$y_names])
        }
      }
    }
    probs
  },
  sort = function(x) {
    ordering <- order(x$maxdepth, # lower values are simpler
                      x$use.grad, # TRUE employs ctree (vs ctree), so simplest
                      max(x$mtry) - x$mtry, # higher values yield more similar tree, so simpler
                      x$sampfrac != 1L, # subsampling yields simpler trees than bootstrap sampling
                      x$learnrate, # lower learnrates yield more similar trees, so simpler
                      decreasing = FALSE)
    x[ordering,]
  },
  loop = function(fullGrid) {

    # loop should provide a grid containing models that can
    # be looped over for tuning penalty.par.val
    loop_rows <- rownames(unique(fullGrid[,-which(names(fullGrid) == "penalty.par.val")]))
    loop <- fullGrid[rownames(fullGrid) %in% loop_rows, ]

    ## submodels should be a list and length(submodels == nrow(loop)
    ## each element of submodels should be a data.frame with column penalty.par.val, with a row for every value to loop over
    submodels <- list()
    ## for every row of loop:
    for (i in 1:nrow(loop)) {
      lambda_vals <- character()
      ## check which rows in fullGrid without $penalty.par.val are equal to
      ## rows in loop without $penalty.par.val
      for (j in 1:nrow(fullGrid)) {
        if (all(loop[i, -which(colnames(loop) == "penalty.par.val")] ==
              fullGrid[j, -which(colnames(fullGrid) == "penalty.par.val")])) {
          lambda_vals <- c(lambda_vals, as.character(fullGrid[j, "penalty.par.val"]))
        }
      }
      lambda_vals <- lambda_vals[-which(lambda_vals == loop$penalty.par.val[i])]
      submodels[[i]] <- data.frame(penalty.par.val = lambda_vals)
    }
    list(loop = loop, submodels = submodels)
  },
  levels = function(x) { levels(x$data[,x$y_names]) },
  tag = c("Rule-Based Model", "Tree-Based Model", "L1 regularization", "Bagging", "Boosting"),
  label = "Prediction Rule Ensembles",
  predictors = function(x, ...) {
    if (x$family %in% c("gaussian", "poisson", "binomial")) {
      return(suppressWarnings(importance(x, plot = FALSE, ...)$varimps$varname))
    } else {
      warning("Reporting the predictors in the model is not yet available for multinomial and multivariate responses")
      return(NULL)
    }
  },
  varImp = function(x, ...) {
    if (x$family %in% c("gaussian","binomial","poisson")) {
      varImp <- pre:::importance(x, plot = FALSE, ...)$varimps
      varnames <- varImp$varname
      varImp <- data.frame(Overall = varImp$imp)
      rownames(varImp) <- varnames
      return(varImp)
    } else {
      warning("Variable importances cannot be calculated for multinomial or mgaussian family")
      return(NULL)
    }
  },
  oob = NULL,
  notes = NULL,
  check = NULL,
  tags = c("Rule-Based Model", "Regularization")
)
