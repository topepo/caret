modelInfo <- list(label = "C4.5-like Trees",
                  library = "RWeka",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = "C",
                                          class = "numeric",
                                          label = "Confidence Threshold"),
                  grid = function(x, y, len = NULL) data.frame(C = 0.25),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
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
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "probability"),
                  predictors = function(x, ...) predictors(x$terms),
                  tags = c("Tree-Based Model", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),])
