modelInfo <- list(label = "Fuzzy Rules via Thrift",
                  library = "frbs",
                  type = "Regression",
                  parameters = data.frame(parameter = c('popu.size', 
                                                        'num.labels', 
                                                        'max.gen'),
                                          class = rep("numeric", 3),
                                          label = c('Population Size',
                                                    '# Fuzzy Labels',
                                                    'Max. Generations')),
                  grid = function(x, y, len = NULL)
                    expand.grid(popu.size = 10*(1:len),      
                                num.labels = 1+(1:len)*2,
                                max.gen = 10),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = cbind(x, y),
                                 method.type = "GFS.LT.RS")
                    args$range.data <- apply(args$data.train, 2, range)
                    
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$popu.size <- param$popu.size                  
                      theDots$control$num.labels <- param$num.labels
                      theDots$control$max.gen <- param$max.gen 
                    } else theDots$control <- list(popu.size  = param$popu.size,                  
                                                   num.labels = param$num.labels,
                                                   max.gen    = param$max.gen,
                                                   persen_cross = 0.6,
                                                   persen_mutant = 0.3,
                                                   mode.tuning = "GLOBAL", 
                                                   type.tnorm = "MIN",
                                                   type.snorm = "MAX", 
                                                   type.implication.func = "ZADEH",
                                                   type.defuz = "WAM", 
                                                   rule.selection = FALSE,
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
