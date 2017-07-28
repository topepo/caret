modelInfo <- list(label = "Fuzzy Rules Using Genetic Cooperative-Competitive Learning and Pittsburgh",
                  library = "frbs",
                  type = "Classification",
                  parameters = data.frame(parameter = c('max.num.rule', 'popu.size', 'max.gen'),
                                          class = rep("numeric", 3),
                                          label = c('Max. #Rules', 'Population Size',
                                                    'Max. Generations')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(max.num.rule = 1+(1:len)*2,      
                                         popu.size = 10,
                                         max.gen = 10)
                    } else {
                      out <- data.frame(max.gen = sample(1:20, size = len, replace = TRUE),
                                        popu.size = sample(seq(2, 20, by = 2), size = len, replace = TRUE),
                                        max.num.rule = sample(1:20, size = len, replace = TRUE))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = as.matrix(cbind(x, as.numeric(y))),
                                 method.type = "FH.GBML")
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$max.num.rule <- param$max.num.rule                  
                      theDots$control$popu.size <- param$popu.size
                      theDots$control$max.gen <- param$max.gen
                    } else theDots$control <- list(max.num.rule = param$max.num.rule,                  
                                                   popu.size  = param$popu.size,
                                                   max.gen    = param$max.gen,
                                                   persen_cross = 0.6,
                                                   persen_mutant = 0.3,
                                                   p.dcare = 0.5,
                                                   p.gccl = 0.5,
                                                   num.class = length(unique(y)),
                                                   name="sim-0")  
                    
                    do.call(frbs::frbs.learn, c(args, theDots))
                    
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    modelFit$obsLevels[predict(modelFit, newdata)]
                  },
                  prob = NULL,
                  predictors = function(x, ...){
                    x$colnames.var[x$colnames.var %in% as.vector(x$rule)]
                  },
                  tags = c("Rule-Based Model"),
                  levels = NULL,
                  sort = function(x) x[order(x$max.num.rule),])
