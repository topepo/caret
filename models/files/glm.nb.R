modelInfo <- list(label = "Negative Binomial Generalized Linear Model",
                  library = "MASS",
                  loop = NULL,
                  type = c("Regression"),
                  parameters = data.frame(parameter = "link",
                                          class = "character",
                                          label = "Link Function"),
                  grid = function(x, y, len = NULL, search = "grid") 
                    data.frame(link = c("log", "sqrt", "identity"))[1:min(len, 3),,drop = FALSE],
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y

                    theDots <- list(...)

                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts

                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
                                        data = dat,
                                        link = as.character(param$link)), 
                                   theDots)

                    out <- do.call(getFromNamespace("glm.nb", "MASS"), modelArgs)
                    ## When we use do.call(), the call infformation can contain a ton of
                    ## information. Inlcuding the contenst of the data. We eliminate it.
                    out$call <- NULL
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata, type = "response")
                  },
                  prob = NULL,
                  varImp = function(object, ...) {
                    values <- summary(object)$coef
                    varImps <-  abs(values[-1, grep("value$", colnames(values)), drop = FALSE])
                    out <- data.frame(varImps)
                    colnames(out) <- "Overall"
                    if(!is.null(names(varImps))) rownames(out) <- names(varImps)
                    out
                  },
                  predictors = function(x, ...) predictors(x$terms),
                  levels = NULL,
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
                  tags = c("Generalized Linear Model", "Accepts Case Weights"),
                  sort = function(x) x)
