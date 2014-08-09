modelInfo <- list(label = "Simplified TSK Fuzzy Rules",
                  library = "frbs",
                  type = "Regression",
                  parameters = data.frame(parameter = c('num.labels', 'max.iter'),
                                          class = c("numeric", "numeric"),
                                          label = c('#Fuzzy Terms', 'Max. Iterations')),
                  grid = function(x, y, len = NULL)
                    expand.grid(num.labels = 1+(1:len)*2,      
                                max.iter = 100),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = cbind(x, y),
                                 method.type = "FS.HGD")
                    args$range.data <- apply(args$data.train, 2, range)
                    
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$num.labels <- param$num.labels                  
                      theDots$control$max.iter <- param$max.iter
                    } else theDots$control <- list(num.labels = param$num.labels,                  
                                                   max.iter = param$max.iter,
                                                   step.size = 0.01, 
                                                   alpha.heuristic = 1,
                                                   type.tnorm = "MIN", 
                                                   type.snorm = "MAX",
                                                   type.implication.func = "ZADEH",
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
                  sort = function(x) x[order(x$num.labels),])
