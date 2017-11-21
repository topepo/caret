modelInfo <- list(label = "Dynamic Evolving Neural-Fuzzy Inference System ",
                  library = "frbs",
                  type = "Regression",
                  parameters = data.frame(parameter = c('Dthr', 'max.iter'),
                                          class = c("numeric", "numeric"),
                                          label = c('Threshold', 'Max. Iterations')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(Dthr = seq(.1, .5, length = len),      
                                         max.iter = 100) 
                    } else {
                      out <- data.frame(Dthr = runif(len, min = 0, max = 1),
                                        max.iter = sample(1:20, replace = TRUE, size = len))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = as.matrix(cbind(x, y)),
                                 method.type = "DENFIS")
                    
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$Dthr <- param$Dthr                  
                      theDots$control$max.iter <- param$max.iter
                    } else theDots$control <- list(Dthr = param$Dthr,                  
                                                   max.iter = param$max.iter,
                                                   step.size = 0.01, 
                                                   d = 2,
                                                   method.type = "DENFIS",
                                                   name="sim-0")   
                    if(!(any(names(theDots) == "range.data"))) {
                      args$range.data <- apply(args$data.train, 2, extendrange)
                    }
                    do.call(frbs::frbs.learn, c(args, theDots))
                    
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata)[, 1]
                  },
                  prob = NULL,
                  predictors = function(x, ...){
                    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
                  },
                  tags = c("Rule-Based Model"),
                  levels = NULL,
                  sort = function(x) x[order(x$Dthr),])
