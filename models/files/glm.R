modelInfo <- list(label = "Generalized Linear Model",
                  library = NULL,
                  loop = NULL,
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    if(length(levels(y)) > 2) stop("glm models can only use 2-class outcomes")

                    theDots <- list(...)
                    if(!any(names(theDots) == "family"))
                    {
                      theDots$family <- if(is.factor(y)) binomial() else gaussian()
                    }

                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts

                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), data = dat), theDots)

                    out <- do.call("glm", modelArgs)
                    ## When we use do.call(), the call infformation can contain a ton of
                    ## information. Inlcuding the contenst of the data. We eliminate it.
                    out$call <- NULL
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    if(modelFit$problemType == "Classification") {
                      probs <-  predict(modelFit, newdata, type = "response")
                      out <- ifelse(probs < .5,
                                    modelFit$obsLevel[1],
                                    modelFit$obsLevel[2])
                    } else {
                      out <- predict(modelFit, newdata, type = "response")
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata, type = "response")
                    out <- cbind(1-out, out)
                    ## glm models the second factor level, we treat the first as the
                    ## event of interest. See Details in ?glm
                    dimnames(out)[[2]] <-  modelFit$obsLevels
                    out
                  },
                  varImp = function(object, ...) {
                    values <- summary(object)$coef
                    varImps <-  abs(values[-1, grep("value$", colnames(values)), drop = FALSE])
                    vimp <- data.frame(varImps)
                    colnames(vimp) <- "Overall"
                    if(!is.null(names(varImps))) rownames(vimp) <- names(varImps)
                    vimp
                  },
                  predictors = function(x, ...) predictors(x$terms),
                  levels = function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL,
                  trim = function(x) {
                    #Based off: http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/
                    x$y = c()
                    x$model = c()

                    x$residuals = c()
                    x$fitted.values = c()
                    x$effects = c()
                    x$qr$qr = c()
                    x$linear.predictors = c()
                    x$weights = c()
                    x$prior.weights = c()
                    x$data = c()

                    x$family$variance = c()
                    x$family$dev.resids = c()
                    x$family$aic = c()
                    x$family$validmu = c()
                    x$family$simulate = c()
                    attr(x$terms,".Environment") = c()
                    attr(x$formula,".Environment") = c()

                    x
                  },
                  tags = c("Generalized Linear Model", "Linear Classifier", 
                           "Two Class Only", "Accepts Case Weights"),
                  sort = function(x) x)
