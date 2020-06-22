modelInfo <- list(label = "Single Rule Classification",
                  library = "RWeka",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('parameter'),
                                          class = c("character"),
                                          label = "none"),
                  grid = function(x, y, len = NULL, search = "grid") 
                    data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat),
                                   theDots)
                    
                    out <- do.call(RWeka::OneR, modelArgs) 
                    out      
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)
                    },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata, type = "probability")
                    },
                  levels = function(x) x$obsLevels,
                  predictors = function(x, ...) predictors(x$terms),
                  tags = c("Rule-Based Model", "Implicit Feature Selection"),
                  sort = function(x) x)
