modelInfo <- list(label = "k-Nearest Neighbors",
                  library = "kknn",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('kmax', 'distance', 'kernel'),
                                          class = c('numeric', 'numeric', 'character'),
                                          label = c('Max. #Neighbors', 'Distance', 'Kernel')),
                  grid = function(x, y, len = NULL) 
                    data.frame(kmax = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0], 
                               distance = 2, 
                               kernel = "optimal")
                  ,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    train.kknn(.outcome ~ ., data = dat,
                               kmax = param$kmax,
                               distance = param$distance,
                               kernel = as.character(param$kernel), ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata)
                  },
                  tags = "Prototype Models",
                  prob = NULL,
                  sort = function(x) x[order(-x[,1]),])
