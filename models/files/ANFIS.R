modelInfo <- list(label = "Adaptive-Network-Based Fuzzy Inference System",
                  library = "frbs",
                  type = "Regression",
                  parameters = data.frame(parameter = c('num.labels', 'max.iter'),
                                          class = c("numeric", "numeric"),
                                          label = c('#Fuzzy Terms', 'Max. Iterations')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(num.labels = 1+(1:len)*2, max.iter = 10)
                    } else {
                      out <- data.frame(num.labels = sample(1:10, replace = TRUE, size = len),
                                        max.iter = sample(1:20, replace = TRUE, size = len))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = as.matrix(cbind(x, y)),
                                 method.type = "ANFIS")
                    
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$num.labels <- param$num.labels                  
                      theDots$control$max.iter <- param$max.iter
                    } else theDots$control <- list(num.labels = param$num.labels,                  
                                                   max.iter = param$max.iter,
                                                   max.iter = 10,
                                                   step.size = 0.01, 
                                                   type.tnorm = "MIN",
                                                   type.snorm = "MAX", 
                                                   type.implication.func = "ZADEH",
                                                   name="sim-0")    
                  
                    if(!(any(names(theDots) == "range.data"))) {
                      args$range.data <- apply(args$data.train, 2, extendrange)
                    }
                    do.call(frbs::frbs.learn, c(args, theDots))
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata)[,1]
                  },
                  prob = NULL,
                  predictors = function(x, ...){
                    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
                  },
                  tags = c("Rule-Based Model"),
                  levels = NULL,
                  sort = function(x) x[order(x$num.labels),])
                    
