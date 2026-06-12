modelInfo <- list(label = "Generalized Linear Model with Stepwise Feature Selection",
                  library = "MASS",
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
                    
                    ## The ... could pass to stepAIC or glm, so we'll try to
                    ## parse them well

                    stepArgs <- names(formals(MASS::stepAIC))
                    stepArgs <- stepArgs[!(stepArgs %in% c("object", "..."))]
                    theDots <- list(...)
                    glmArgs <- list()

                    if(!any(names(theDots) == "family")) {
                      glmArgs$family <- if(is.factor(y)) binomial() else gaussian()              
                    } else glmArgs$family <- theDots$family
                    if(any(!(names(theDots) %in% stepArgs))) 
                      theDots <- theDots[names(theDots) %in% stepArgs]
                    
                    if(any(names(theDots) == "direction")) {
                      ## Assume that if you go forward, you should start from nothing
                      if(theDots$direction == "forward") {
                        start_form <- as.formula(".outcome ~ 1")
                        if(!any(names(theDots) == "scope")) {
                          theDots$scope <- list(lower = as.formula(".outcome ~ 1"),
                                                upper = as.formula(paste0(".outcome~", paste0(colnames(x), collapse = "+"))))
                        }
                      } else {
                        ## For backwards or both, start from the current model
                        start_form <- as.formula(".outcome ~ .")
                      }           
                    } else start_form <- as.formula(".outcome ~ .")
                    ##  `If the scope argument is missing the default for direction is "backward".`
                    
                    ## pass in any model weights
                    if(!is.null(wts)) glmArgs$weights <- wts
                    
                    modelArgs <- c(list(formula = start_form, data = dat),
                                   glmArgs)

                    mod <- do.call(glm, modelArgs)

                    theDots$object <- mod
                    out <- do.call(MASS::stepAIC, theDots)
                    out$call <- NULL
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
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
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata, type = "response")
                    out <- cbind(1-out, out)
                    ## glm models the second factor level, we treat the first as the
                    ## event of interest. See Details in ?glm
                    dimnames(out)[[2]] <-  modelFit$obsLevels
                    out
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Generalized Linear Model", "Feature Selection Wrapper", "Linear Classifier",
                           "Implicit Feature Selection", "Two Class Only", "Accepts Case Weights"),
                  sort = function(x) x)
