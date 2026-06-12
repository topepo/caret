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
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(popu.size = 10*(1:len),      
                                         num.labels = 1+(1:len)*2,
                                         max.gen = 10)
                    } else {
                      out <- data.frame(max.gen = sample(1:20, size = len, replace = TRUE),
                                        popu.size = sample(seq(2, 20, by = 2), size = len, replace = TRUE),
                                        num.labels = sample(2:20, size = len, replace = TRUE))
                    }
                    out
                  }, 
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = as.matrix(cbind(x, y)),
                                 method.type = "GFS.THRIFT")
                    
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
                                                   type.defuz = "WAM", 
                                                   type.tnorm = "MIN",
                                                   type.snorm = "MAX", 
                                                   type.mf = "TRIANGLE",
                                                   type.implication.func = "ZADEH",
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
                  sort = function(x) x[order(x$num.labels),])
