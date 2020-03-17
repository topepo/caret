modelInfo <- list(
  label = "Ordered Logistic or Probit Regression",
  library = "MASS",
  loop = NULL,
  type = "Classification",
  parameters = data.frame(parameter = "method",
                          class = "character",
                          label = "parameter"),
  grid = function(x, y, len = NULL, search = "grid") {
    if(search == "grid") {
      out <- data.frame(method = c("logistic", "probit", "loglog", "cloglog", "cauchit"))
    } else {
      out <- data.frame(method = sample(c("logistic", "probit", "loglog", "cloglog", "cauchit"),
                               size = len, replace = TRUE))
    }
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    modelArgs <- list(...)

    ## Set up data
    dat <- if (is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
    dat$.outcome <- y
    modelArgs <- c(list(formula = .outcome ~ .,
                        data = dat,
                        method = as.character(param$method)),
                   modelArgs)

    ## Always force Hessian calculation
    modelArgs$Hess <- TRUE

    ## Pass in model weights, if any
    if (!is.null(wts))
      modelArgs$weights <- wts

    ## Fit model
    ans <- do.call(MASS::polr, modelArgs)

    ## Strip out the call to avoid unnecessary data repetition
    ans$call <- NULL

    ans
  },
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata = newdata, type = "class"),
  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata = newdata, type = "probs"),
  varImp = function(object, ...) {
    ## Extract coefficients (without cutpoints)
    cf <- coef(object)
    ncf <- length(cf)

    ## Calculate standard errors of coefficients and z-statistics
    se <- sqrt(diag(vcov(object)))
    se <- se[seq_len(ncf)]
    z <- cf / se

    ## Organize output as in the method for `glm`
    out <- data.frame(Overall = abs(z))
    if (!is.null(names(cf)))
      rownames(out) <- names(cf)

    out
  },
  predictors = function(x, ...) predictors(terms(x)),
  levels = function(x)
    if (any(names(x) == "obsLevels")) x$obsLevels else NULL,
  tags = c("Logistic Regression", "Linear Classifier", "Accepts Case Weights", "Ordinal Outcomes"),
  sort = function(x) x)
