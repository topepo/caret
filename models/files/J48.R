modelInfo <- list(label = "C4.5-like Trees",
                  library = "RWeka",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = "C",
                                          class = "numeric",
                                          label = "Confidence Threshold"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(C = 0.25),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x)
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control")) {
                      theDots$control$C <- param$C 
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- Weka_control(C = param$C) 
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        control = ctl),
                                   theDots)
                    
                    out <- do.call("J48", modelArgs) 
                    out      
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata)
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata, type = "probability")
                  },
                  levels = function(x) x$obsLevels,
                  predictors = function(x, ...) predictors(x$terms),
                  tags = c("Tree-Based Model", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),])
