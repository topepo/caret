modelInfo <- list(label = "Multi-Layer Perceptron",
                  library = "RSNNS",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('size', 'decay'),
                                          class = c('numeric', 'numeric'),
                                          label = c('#Hidden Units', 'Weight Decay')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(size = ((1:len) * 2) - 1, 
                                         decay = c(0, 10 ^ seq(-1, -4, length = len - 1)))
                    } else {
                      out <- data.frame(size = sample(1:20, size = len, replace = TRUE), 
                                        decay = 10^runif(len, min = -5, max = 1))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    theDots <- list(...)
                    theDots <- theDots[!(names(theDots) %in% c("size", "linOut"))]
                    if(any(names(theDots) == "learnFunc"))
                    {
                      theDots$learnFunc <- NULL
                      warning("Cannot over-ride 'learnFunc' argument for this model. BackpropWeightDecay is used.")
                    }
                    if(any(names(theDots) == "learnFuncParams"))
                    {
                      prms <- theDots$learnFuncParams
                      prms[2] <-  param$decay
                      warning("Over-riding weight decay value in the 'learnFuncParams' argument you passed in. Other values are retained")
                    } else prms <- c(0.2, param$decay, 0.0, 0.0)    
                    
                    if(is.factor(y)) {
                      y <- RSNNS:::decodeClassLabels(y)
                      lin <- FALSE
                    } else lin <- TRUE
                    args <- list(x = x,
                                 y = y,
                                 learnFunc = "BackpropWeightDecay",
                                 learnFuncParams = prms,                                
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
                  tags = c("Neural Network","L2 Regularization"),
                  sort = function(x) x[order(x$size, -x$decay),])
