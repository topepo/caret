modelInfo <- list(label = "Generalized Linear Model with Stepwise Feature Selection",
                  library = "MASS",
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
                    
                    ## The ... could pass to stepAIC or glm, so we'll try to
                    ## parse them well
                    
                    stepArgs <- names(formals(stepAIC))
                    stepArgs <- stepArgs[!(stepArgs %in% c("object", "..."))]
                    theDots <- list(...)
                    glmArgs <- list()
                    
                    if(!any(names(theDots) == "family"))
                    {
                      glmArgs$family <- if(is.factor(y)) binomial() else gaussian()              
                    } else glmArgs$family <- theDots$family
                    if(any(!(names(theDots) %in% stepArgs))) theDots <- theDots[names(theDots) %in% stepArgs]
                    
                    ## pass in any model weights
                    if(!is.null(wts)) glmArgs$weights <- wts
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), data = dat),
                                   glmArgs)
                    
                    mod <- do.call("glm", modelArgs)
                    
                    theDots$object <- mod
                    out <- do.call("stepAIC", theDots)
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
                  tags = c("Generalized Linear Model", "Feature Selection Wrapper", "Linear Classifier",
                           "Implicit Feature Selection"),
                  sort = function(x) x)
