modelInfo <- list(label = "Subtractive Clustering and Fuzzy c-Means Rules",
                  library = "frbs",
                  type = "Regression",
                  parameters = data.frame(parameter = c('r.a', 'eps.high', 'eps.low'),
                                          class = rep("numeric", 3),
                                          label = c('Radius', 'Upper Threshold', 'Lower Threshold')),
                  grid = function(x, y, len = NULL) {
                    grid <- expand.grid(r.a = seq(0, 1, length = len),
                                        eps.high = seq(0, 1, length = len),
                                        eps.low = seq(0, 1, length = len))
                    subset(grid, eps.high > eps.low)
                    },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    range_data <- apply(cbind(x, y), 2, range)
                    frbs.learn(data.train = cbind(x, y),
                               range.data = range_data,
                               method = "SBC",
                               control = list(r.a = param$r.a,
                                              eps.high = param$eps.high,
                                              eps.low = param$eps.low))
                    
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
                  sort = function(x) x[order(x$r.a),])
