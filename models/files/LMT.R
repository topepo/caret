modelInfo <- list(label = "Logistic Model Trees",
                  library = "RWeka",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = "iter",
                                          class = "numeric",
                                          label = "# Iteratons"),
                  grid = function(x, y, len = NULL) data.frame(iter = 1+(0:(len-1)) * 20),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control")) {
                      theDots$control$I <- param$iter 
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- Weka_control(I = param$iter) 
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        control = ctl),
                                   theDots)
                    
                    out <- do.call("LMT", modelArgs) 
                    out      
                    },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "probability"),
                  predictors = function(x, ...) predictors(x$terms),
                  tags = c("Model Tree", "Implicit Feature Selection", "Logistic Regression", "Linear Classifier"),
                  sort = function(x) x[order(x[,1]),])
