modelInfo <- list(label = "Fuzzy Rules via MOGUL",
                  library = "frbs",
                  type = "Regression",
                  parameters = data.frame(parameter = c('max.gen', 'max.iter', 'max.tune'),
                                          class = rep("numeric", 3),
                                          label = c('Max. Generations', 'Max. Iterations',
                                                    'Max. Tuning Iterations')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(max.gen = 10*(1:len),      
                                         max.iter = 10,
                                         max.tune = 10*(1:len))
                    } else {
                      out <- data.frame(max.gen = sample(1:20, size = len, replace = TRUE),
                                        max.iter = sample(1:20, replace = TRUE, size = len),
                                        max.tune = sample(1:20, size = len, replace = TRUE))
                    }
                    out
                  }, 
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = as.matrix(cbind(x, y)),
                                 method.type = "GFS.FR.MOGUL")
                    
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$max.gen <- param$max.gen                  
                      theDots$control$max.iter <- param$max.iter
                      theDots$control$max.tune <- param$max.tune 
                    } else theDots$control <- list(max.gen = param$max.gen,                  
                                                   max.iter = param$max.iter,
                                                   max.tune = param$max.tune,
                                                   persen_cross = 0.6,
                                                   persen_mutant = 0.3,
                                                   epsilon = 0.4,
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
                  sort = function(x) x[order(x$max.iter),])
