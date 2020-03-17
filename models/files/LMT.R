modelInfo <- list(label = "Logistic Model Trees",
                  library = "RWeka",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = "iter",
                                          class = "numeric",
                                          label = "# Iteratons"),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <-  data.frame(iter = 1+(0:(len-1)) * 20)
                    } else {
                      out <- data.frame(iter = unique(sample(1:100, size = len, replace = TRUE)))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control")) {
                      theDots$control$I <- param$iter 
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- RWeka::Weka_control(I = param$iter) 
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        control = ctl),
                                   theDots)
                    
                    out <- do.call(RWeka::LMT, modelArgs) 
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
                  tags = c("Model Tree", "Implicit Feature Selection", "Logistic Regression", "Linear Classifier"),
                  sort = function(x) x[order(x[,1]),])
