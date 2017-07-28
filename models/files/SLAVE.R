modelInfo <- list(label = "Fuzzy Rules Using the Structural Learning Algorithm on Vague Environment",
                  library = "frbs",
                  type = "Classification",
                  parameters = data.frame(parameter = c('num.labels', 'max.iter', 'max.gen'),
                                          class = rep("numeric", 3),
                                          label = c('#Fuzzy Terms', 'Max. Iterations',
                                                    'Max. Generations')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(num.labels = 1+(1:len)*2,      
                                         max.iter = 10,
                                         max.gen = 10)
                    } else {
                      out <- data.frame(num.labels = sample(2:20, size = len, replace = TRUE),
                                        max.iter = sample(1:20, replace = TRUE, size = len),
                                        max.gen = sample(1:20, size = len, replace = TRUE))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = as.matrix(cbind(x, as.numeric(y))),
                                 method.type = "SLAVE")
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$num.labels <- param$num.labels                  
                      theDots$control$max.iter <- param$max.iter
                      theDots$control$max.gen <- param$max.gen
		      theDots$control$num.class <- length(unique(y))
                    } else theDots$control <- list(num.labels = param$num.labels,                  
                                                   max.iter  = param$max.iter,
                                                   max.gen    = param$max.gen,
                                                   persen_cross = 0.6,
                                                   persen_mutant = 0.3,
                                                   k.lower = 0.25,
                                                   k.upper = 0.75, 
                                                   epsilon = 0.1,
                                                   num.class = length(unique(y)),
                                                   name="sim-0") 
                    if(!(any(names(theDots) == "range.data"))) {
                      args$range.data <- apply(args$data.train, 2, extendrange)
                    }
                    do.call(frbs::frbs.learn, c(args, theDots))
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
