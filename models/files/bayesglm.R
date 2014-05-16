modelInfo <- list(label = "Bayesian Generalized Linear Model", 
                  library = "arm",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    theDots <- list(...)
                    if(!any(names(theDots) == "family"))
                      theDots$family <- if(is.factor(dat$.outcome)) binomial() else gaussian()              
                    
                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat),
                                   theDots)
                    
                    out <- do.call("bayesglm", modelArgs)
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
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, type = "response")
                    out <- cbind(1-out, out)
                    ## glm models the second factor level. See Details in ?glm
                    colnames(out) <-  modelFit$obsLevels
                    out
                  },
                  tags = c("Generalized Linear Model", "Logistic Regression", "Linear Classifier"),
                  sort = function(x) x)
