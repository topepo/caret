modelInfo <- list(label = "Fuzzy Rules Using Chi's Method",
                  library = "frbs",
                  type = "Classification",
                  parameters = data.frame(parameter = c('num.labels', 'type.mf'),
                                          class = c("numeric", "character"),
                                          label = c('#Fuzzy Terms', 'Membership Function')),
                  grid = function(x, y, len = NULL)
                    expand.grid(num.labels = 1+(1:len)*2,      
                                type.mf = c("GAUSSIAN", "TRAPEZOID", "TRIANGLE")),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = cbind(x, as.numeric(y)),
                                 method.type = "FRBCS.CHI")
                    args$range.data <- apply(x, 2, range)
                    
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$num.labels <- param$num.labels                  
                      theDots$control$type.mf <- param$type.mf
                    } else theDots$control <- list(num.labels  = param$num.labels,                  
                                                   type.mf     = param$type.mf,
                                                   type.tnorm = "MIN",
                                                   type.snorm = "MAX", 
                                                   type.implication.func = "ZADEH",
                                                   num.class = length(unique(y)),
                                                   name="sim-0")     
                    do.call("frbs.learn", c(args, theDots))
                    
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    modelFit$obsLevels[predict(modelFit, newdata)[,1]]
                  },
                  prob = NULL,
                  predictors = function(x, ...){
                    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
                  },
                  tags = c("Rule-Based Model"),
                  levels = NULL,
                  sort = function(x) x[order(x$num.labels),])
