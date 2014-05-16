modelInfo <- list(label = "Single Rule Classification",
                  library = "RWeka",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('parameter'),
                                          class = c("character"),
                                          label = "none"),
                  grid = function(x, y, len = NULL) 
                    data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat),
                                   theDots)
                    
                    out <- do.call("OneR", modelArgs) 
                    out      
                    },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "probability"),
                  predictors = function(x, ...) predictors(x$terms),
                  tags = c("Rule-Based Model", "Implicit Feature Selection"),
                  sort = function(x) x)
