modelInfo <- list(label = "C4.5-like Trees",
                  library = "RWeka",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c("C", "M"),
                                          class = c("numeric", "numeric"),
                                          label = c("Confidence Threshold", "Minimum Instances Per Leaf")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    upperBound <- min(max(1, floor(nrow(x) / 2)), 50)
                    if(search == "grid"){
                      out <- expand.grid(C = seq(0.01, 0.5, length.out = len),
                                         M = 1:min(upperBound, len))
                      if(len == 1){
                        out <- data.frame(C = 0.25, M = 2)
                      }
                    } else {
                      out <- data.frame(C = runif(len , 0.0, 0.5),
                                        M = round(exp(runif(len, 0, log(upperBound)))))
                    }
                    return(out)
                  } ,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control")) {
                      theDots$control$C <- param$C 
                      theDots$control$M <- param$M
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- RWeka::Weka_control(C = param$C, M = param$M) 
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        control = ctl),
                                   theDots)
                    
                    out <- do.call(RWeka::J48, modelArgs) 
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
                  tags = c("Tree-Based Model", "Implicit Feature Selection"),
                  sort = function(x) x[order(x$C, x$M),])
