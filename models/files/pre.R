modelInfo <- list(
  library = "pre",
  type = c("Classification", "Regression"),
  parameters = data.frame(parameter = c("sampfrac", "maxdepth", 
                                        "learnrate", "mtry", 
                                        "ntrees", "winsfrac", 
                                        "use.grad", "tree.unbiased", 
                                        "type"),
                          class = c(rep("numeric", times = 6), 
                                    rep("logical", times = 2), 
                                    "character"),
                          label = c("sampfrac", "maxdepth", 
                                    "learnrate", "mtry",
                                    "ntrees", "winsfrac", 
                                    "use.grad", "tree.unbiased", 
                                    "type")),
  grid = function(x, y, len = NULL, search = "grid", 
                  sampfrac = .5, maxdepth = Inf, learnrate = .01, 
                  mtry = Inf, ntrees = 500, winsfrac = .025, 
                  use.grad = TRUE, tree.unbiased = TRUE, 
                  type = "both") {
    out <- expand.grid(sampfrac = sampfrac, maxdepth = maxdepth, 
                       learnrate = learnrate, mtry = mtry, 
                       ntrees = ntrees, winsfrac = winsfrac,
                       use.grad = use.grad, tree.unbiased = tree.unbiased, 
                       type = type)
    # mtry cannot be used if tree.unbiased = FALSE
    inds <- which(!out$tree.unbiased & !is.infinite(out$mtry))
    if (length(inds) > 0) {
      out <- out[-inds,]
    }
    # use.grad must be TRUE if tree.unbiased = FALSE
    inds <- which(!out$tree.unbiased & !out$use.grad)
    if (length(inds) > 0) {
      out <- out[-inds,]
    }
    # type = "linear" makes all but winsfrac redundant
    inds <- which(out$type == "linear")[-(1:length(winsfrac))]
    if (length(inds) > 0) {
      out <- out[-inds,]
    }
    out[out$type == "linear","winsfrac"] <- winsfrac
    # type = "rules" makes winsfrac redundant
    type_rules <- out[out$type == "rules",]
    inds <- which(out$type == "rules")
    if (length(inds) > 0) {
      out <- out[-inds,]
      type_rules <- unique(type_rules[,-which(names(type_rules) == "winsfrac")])
      type_rules$winsfrac <- winsfrac[1] 
      out <- rbind(out, type_rules)
    }
    return(out)
  },
  fit = function(x, y, wts = NULL, param, lev = NULL, last = NULL, 
                 weights = NULL, classProbs, ...) { 
    list()
    data <- data.frame(cbind(x, .outcome = y))
    formula <- formula(terms(.outcome ~ ., data = data))
    if (is.null(weights)) { weights <- rep(1, times = nrow(x)) }
    pre_args <- list(formula = formula, data = data, 
                     weights = weights, sampfrac = param$sampfrac, 
                     maxdepth = param$maxdepth, 
                     learnrate = param$learnrate, mtry = param$mtry, 
                     ntrees = param$ntrees, winsfrac = param$winsfrac, 
                     use.grad = param$use.grad, 
                     tree.unbiased = param$tree.unbiased, 
                     type = param$type)
    pre_args <- c(pre_args, list(...))
    do.call(pre, pre_args)
  },
  predict = function(modelFit, newdata, submodels = NULL) {
    if (modelFit$family %in% c("gaussian", "mgaussian")) {
      pre:::predict.pre(object = modelFit, newdata = as.data.frame(newdata)) # submodels can be employed to loop through penalty.par.val
    } else if (modelFit$family == "poisson") {
      pre:::predict.pre(object = modelFit, newdata = as.data.frame(newdata), type = "response")
    } else {
      factor(pre:::predict.pre(object = modelFit, newdata = as.data.frame(newdata), type = "class"))      
    }
  },
  prob = function(modelFit, newdata, submodels = NULL) {
    probs <- pre:::predict.pre(object = modelFit, newdata = as.data.frame(newdata), type = "response")
    # For binary classification, create matrix:    
    if (is.null(ncol(probs)) || ncol(probs) == 1) {
      probs <- data.frame(1 - probs, probs)
      colnames(probs) <- levels(modelFit$data[,modelFit$y_names])
    }
    probs
  },
  sort = NULL,
  loop = NULL,  # something with penalty.par.val and predict.pre
  levels = NULL,
  tag = c("Rule-Based Model", "Tree-Based Model", "L1 regularization", "Bagging", "Boosting"),
  label = "Prediction Rule Ensembles",
  predictors = function(x, ...) { 
    if (x$family %in% c("gaussian", "poisson", "binomial")) {
      return(importance(x, plot = FALSE, ...)$varimps$varname)
    } else {
      warning("Reporting the predictors in the model is not yet available for multinomial and multivariate responses")
      return(NULL)
    }
  },
  varImp = NULL,
  oob = NULL,
  notes = NULL,
  check = NULL
)
