modelInfo <- list(label = "Neural Network",
                  library = "mxnet",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('layer1', 'layer2', 'layer3', "learning.rate", "momentum", "dropout"),
                                          class = rep('numeric', 6),
                                          label = c('#Hidden Units in Layer 1', '#Hidden Units in Layer 2', '#Hidden Units in Layer 3',
                                                    "Learning Rate", "Momentum", "Dropout Rate")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(layer1 = ((1:len) * 2) - 1, layer2 = 0, layer3 = 0,
                                         learning.rate = 2e-6, momentum = 0.9, dropout = 0)
                    } else {
                      out <- data.frame(layer1 = sample(2:20, replace = TRUE, size = len),
                                        layer2 = 0,# sample(c(0, 2:20), replace = TRUE, size = len),
                                        layer3 = 0,#sample(c(0, 2:20), replace = TRUE, size = len),
                                        learning.rate = runif(len),
                                        momentum = runif(len, min = .5),
                                        dropout = runif(len, max = .5))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    num_units <- param[grepl("layer[1-9]", names(param))]
                    num_units <- num_units[num_units > 0]
                    if(!is.matrix(x)) x <- as.matrix(x)
                    if(is.numeric(y)) {
                      out <- mx.mlp(data = x, 
                                    label = y,
                                    hidden_node = num_units,
                                    out_node = 1, 
                                    out_activation = "rmse", 
                                    learning.rate = param$learning.rate, 
                                    momentum = param$momentum, 
                                    eval.metric = mx.metric.rmse, 
                                    array.layout = "rowmajor",
                                    ...)
                    } else {
                      y <- as.numeric(y) - 1
                      # mx.set.seed(1)
                      out <- mx.mlp(data = x, 
                                    label = y,
                                    hidden_node = num_units,
                                    out_node = length(unique(y)), 
                                    out_activation = "softmax",
                                    learning.rate = param$learning.rate, 
                                    momentum = param$momentum, 
                                    eval.metric = mx.metric.accuracy, 
                                    array.layout = "rowmajor", 
                                    ...)
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    pred <- predict(modelFit, newdata)
                    if(modelFit$problemType == "Regression") {
                      pred <- pred[1,]
                    } else pred <- modelFit$obsLevels[apply(pred, 2, which.max)]
                    pred
                  },
                  predictors = function(x, ...)  {
                    if(any(names(x) == "xNames")) x$xNames else NA
                  },
                  prob =  function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    pred <- t(predict(modelFit, newdata))
                    colnames(pred) <- modelFit$obsLevels
                    pred
                  },
                  tags = c("Neural Network"),
                  sort = function(x) x[order(x$layer1, x$layer2, x$layer3),])
