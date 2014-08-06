modelInfo <- list(label = "Dynamic Evolving Neural-Fuzzy Inference System ",
                  library = "frbs",
                  type = "Regression",
                  parameters = data.frame(parameter = c('Dthr', 'max.iter'),
                                          class = c("numeric", "numeric"),
                                          label = c('Threshold', 'Max. Iterations')),
                  grid = function(x, y, len = NULL)
                    expand.grid(Dthr = seq(.1, .5, length = len),      
                                max.iter = 100),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = cbind(x, y),
                                 method.type = "DENFIS")
                    args$range.data <- apply(args$data.train, 2, range)
                    
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$Dthr <- param$Dthr                  
                      theDots$control$max.iter <- param$max.iter
                    } else theDots$control <- list(Dthr = param$Dthr,                  
                                                   max.iter = param$max.iter,
                                                   step.size = 0.01, 
                                                   d = 2,
                                                   name="sim-0")     
                    do.call("frbs.learn", c(args, theDots))
                    
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata)
                  },
                  prob = NULL,
                  predictors = function(x, ...){
                    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
                  },
                  tags = c("Rule-Based Model"),
                  levels = NULL,
                  sort = function(x) x[order(x$Dthr),])
