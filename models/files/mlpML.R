modelInfo <- list(label = "Multi-Layer Perceptron, with multiple layers",
                  library = "RSNNS",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('layer1','layer2','layer3'),
                                          class = c('numeric','numeric','numeric'),
                                          label = c('#Hidden Units layer1','#Hidden Units layer2','#Hidden Units layer3')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(layer1 = ((1:len) * 2) - 1, layer2 = 0, layer3 = 0)
                    } else {
                      out <- data.frame(layer1 = sample(2:20, replace = TRUE, size = len),
                                        layer2 = sample(c(0, 2:20), replace = TRUE, size = len),
                                        layer3 = sample(c(0, 2:20), replace = TRUE, size = len))
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

                    if(param$layer1 == 0) stop("the first layer must have at least one hidden unit")
                    if(param$layer2 == 0 & param$layer2 > 0) stop("the second layer must have at least one hidden unit if a third layer is specified")

                    nodes <- c(param$layer1)
                    if(param$layer2 > 0) {
                      nodes <- c(nodes, param$layer2)
                      if(param$layer3 > 0) nodes <- c(nodes, param$layer3)
                    }

                    args <- list(x = x,
                                 y = y,
                                 size = nodes,
                                 linOut = lin)
                    args <- c(args, theDots)
                    #mlp(x,y,size=node,linOut=lin)
                    do.call("mlp", args)
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
                  sort = function(x) x[order(x$layer1, x$layer2, x$layer3),])
