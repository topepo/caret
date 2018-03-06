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
                  tags = c("Neural Network"),
                  sort = function(x) x[order(x$layer1, x$layer2, x$layer3),])
