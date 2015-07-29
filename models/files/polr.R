modelInfo <- list(
    label = "Ordered Logistic or Probit Regression",
    library = "MASS",
    loop = NULL,
    type = "Classification",
    parameters = data.frame(
        parameter = "parameter",
        class = "character",
        label = "parameter"),
    grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
        modelArgs <- list(...)

        ## Set up data
        dat <- if (is.data.frame(x)) x else as.data.frame(x)
        dat$.outcome <- y
        modelArgs <- c(list(formula = .outcome ~ .,
                            data = dat),
                       modelArgs)

        ## Always force Hessian calculation
        modelArgs$Hess <- TRUE

        ## Pass in model weights, if any
        if (!is.null(wts))
            modelArgs$weights <- wts

        ## Fit model
        ans <- do.call("polr", modelArgs)

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
    tags = c("Logistic Regression", "Linear Classifier"),
    sort = function(x) x)
