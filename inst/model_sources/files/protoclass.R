modelInfo <- list(label = "Greedy Prototype Selection",
                  library = c("proxy", "protoclass"),
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('eps', 'Minkowski'),
                                          class = c('numeric', 'numeric'),
                                          label = c('Ball Size', 'Distance Order')),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(eps = 1:len, Minkowski = 2),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    out <- protoclass::protoclass(x = x, y = y,
                                                  dxz = as.matrix(proxy:::dist(x, x,
                                                                               method = "Minkowski",
                                                                              p
                  = as.double(param$Minkowski))),
                                                  eps = param$eps, 
                                                  ...)
                    out$Minkowski <- 2
                    out$training <- x
                    out
                  },
                  levels = function(x) x$obsLevels,
                  predict = function(modelFit, newdata, submodels = NULL)
                    as.character(protoclass::predictwithd.protoclass(modelFit,
                                                         as.matrix(proxy:::dist(newdata, modelFit$training,
                                                                                "Minkowski", 
                                                                                p = modelFit$Minkowski)))),
                  prob = NULL,
                  tags = c("Prototype Models"),
                  sort = function(x) x[order(-x$eps),])
