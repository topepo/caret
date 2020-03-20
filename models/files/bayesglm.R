modelInfo <- list(label = "Bayesian Generalized Linear Model",
                  library = "arm",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    theDots <- list(...)
                    if(!any(names(theDots) == "family"))
                      theDots$family <- if(is.factor(dat$.outcome)) binomial() else gaussian()

                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts

                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat),
                                   theDots)

                    out <- do.call(arm::bayesglm, modelArgs)
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
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata, type = "response")
                    out <- cbind(1-out, out)
                    ## glm models the second factor level. See Details in ?glm
                    colnames(out) <-  modelFit$obsLevels
                    out
                  },
                  levels = function(x) x$obsLevels,
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
                    x$R <- c() #Not in a glm
                    x$xNames <- c()
                    x$xlevels <- c()
                    x
                  },
                  tags = c("Generalized Linear Model", "Logistic Regression", 
                           "Linear Classifier", "Bayesian Model", "Accepts Case Weights"),
                  sort = function(x) x)
