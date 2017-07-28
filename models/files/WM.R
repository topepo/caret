modelInfo <- list(label = "Wang and Mendel Fuzzy Rules",
                  library = "frbs",
                  type = "Regression",
                  parameters = data.frame(parameter = c('num.labels', 'type.mf'),
                                          class = c("numeric", "character"),
                                          label = c('#Fuzzy Terms', 'Membership Function')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(num.labels = 1+(1:len)*2,      
                                         type.mf = c("GAUSSIAN", "TRAPEZOID", "TRIANGLE"))
                    } else {
                      out <- data.frame(num.labels = sample(2:20, size = len, replace = TRUE),
                                        type.mf = sample(c("GAUSSIAN", "TRAPEZOID", "TRIANGLE"), size = len, replace = TRUE))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = as.matrix(cbind(x, y)))
                    
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$num.labels <- param$num.labels                  
                      theDots$control$type.mf <- param$type.mf
                    } else theDots$control <- list(num.labels = param$num.labels,                  
                                                   type.mf = param$type.mf,            
                                                   type.defuz = "WAM", 
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
