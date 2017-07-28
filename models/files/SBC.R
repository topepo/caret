modelInfo <- list(label = "Subtractive Clustering and Fuzzy c-Means Rules",
                  library = "frbs",
                  type = "Regression",
                  parameters = data.frame(parameter = c('r.a', 'eps.high', 'eps.low'),
                                          class = rep("numeric", 3),
                                          label = c('Radius', 'Upper Threshold', 'Lower Threshold')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(r.a = seq(0, 1, length = len),
                                         eps.high = seq(0, 1, length = len),
                                         eps.low = seq(0, 1, length = len))
                    } else {
                      out <- data.frame(r.a = sample(1:20, size = len*10, replace = TRUE),
                                        eps.high = runif(len*10, min = 0, max = 1),
                                        eps.low = runif(len*10, min = 0, max = 1))
                    }
                    out <- subset(out, eps.high > eps.low)
                    out[1:min(nrow(out), len),]
                  }, 
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    args <- list(data.train = as.matrix(cbind(x, y)),
                                 method.type = "SBC",
                                 control = list(r.a  = param$r.a,                  
                                                eps.high = param$eps.high,
                                                eps.low = param$eps.low))
                    
                    theDots <- list(...)
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
                  sort = function(x) x[order(x$r.a),])
