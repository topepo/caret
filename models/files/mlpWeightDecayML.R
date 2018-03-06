modelInfo <- list(label = "Multi-Layer Perceptron, multiple layers",
                  library = "RSNNS",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('layer1','layer2','layer3', 'decay'),
                                          class = c('numeric','numeric','numeric', 'numeric'),
                                          label = c('#Hidden Units layer1','#Hidden Units layer2','#Hidden Units layer3', 'Weight Decay')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(layer1 = ((1:len) * 2) - 1, layer2 = 0, layer3 = 0,
                                         decay = c(0, 10 ^ seq(-1, -4, length = len - 1)))
                    } else {
                      out <- data.frame(layer1 = sample(2:20, replace = TRUE, size = len),
                                        layer2 = sample(c(0, 2:20), replace = TRUE, size = len),
                                        layer3 = sample(c(0, 2:20), replace = TRUE, size = len),
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
                    
                    nodes <- c(param$layer1, param$layer2, param$layer3)
                    if (any(nodes == 0)) {
                      nodes <- nodes[nodes > 0]
                      warning(
                        "At least one layer had zero units and ",
                        "were removed. The new structure is ",
                        paste0(nodes, collapse = "->"), call. = FALSE
                      ) 
                    }
                    
                    args <- list(x = x,
                                 y = y,
                                 learnFunc = "BackpropWeightDecay",
                                 learnFuncParams = prms,
                                 size = nodes,
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
                  sort = function(x) x[order(x$layer1, x$layer2, x$layer3, -x$decay),])
