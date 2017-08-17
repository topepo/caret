modelInfo <- list(label = "Multi-Layer Perceptron",
                  library = "RSNNS",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('size'),
                                          class = c('numeric'),
                                          label = c('#Hidden Units')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(size =  ((1:len) * 2) - 1)
                    } else {
                      out <- data.frame(size = unique(sample(1:20, size = len, replace = TRUE)))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    theDots <- list(...)
                    theDots <- theDots[!(names(theDots) %in% c("size", "linOut"))]                   
                    
                    if(is.factor(y)) {
                      y <- RSNNS:::decodeClassLabels(y)
                      lin <- FALSE
                    } else lin <- TRUE
                    args <- list(x = x,
                                 y = y,
                                 size = param$size,
                                 linOut = lin)
                    args <- c(args, theDots)
                    do.call(RSNNS::mlp, args)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)
                    if(modelFit$problemType == "Classification")
                    {
                      out <- modelFit$obsLevels[apply(out, 1, which.max)]
                    } else out <- out[,1]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)
                    colnames(out) <- modelFit$obsLevels
                    out
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Neural Network"),
                  sort = function(x) x[order(x$size),])
