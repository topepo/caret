modelInfo <- list(label = "Generalized Linear Model",
                  library = NULL,
                  loop = NULL,
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
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
                    if(modelFit$problemType == "Classification")
                    {
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
                    out <- predict(modelFit, newdata, type = "response")
                    out <- cbind(1-out, out)
                    ## glm models the second factor level, we treat the first as the
                    ## event of interest. See Details in ?glm
                    dimnames(out)[[2]] <-  modelFit$obsLevels
                    out
                  },
                  varImp = function(object, ...) {
                    values <- summary(object)$coef
                    varImps <-  abs(values[-1, grep("value$", colnames(values))])
                    out <- data.frame(varImps)
                    colnames(out) <- "Overall"
                    if(!is.null(names(varImps))) rownames(out) <- names(varImps)
                    out   
                  },
                  predictors = function(x, ...) predictors(x$terms),
                  levels = function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL,
                  tags = c("Generalized Linear Model", "Linear Classifier"),
                  sort = function(x) x)
