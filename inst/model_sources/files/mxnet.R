modelInfo <- list(label = "Neural Network",
                  library = "mxnet",
                  loop = NULL,
                  type = c('Classification', 'Regression'),
                  parameters = data.frame(parameter = c('layer1', 'layer2', 'layer3',
                                                        "learning.rate", "momentum", "dropout",
                                                        "activation"),
                                          class = c(rep('numeric', 6), "character"),
                                          label = c('#Hidden Units in Layer 1', '#Hidden Units in Layer 2',
                                                    '#Hidden Units in Layer 3',
                                                    "Learning Rate", "Momentum",
                                                    "Dropout Rate",
                                                    "Activation Function")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(layer1 = ((1:len) * 2) - 1, layer2 = 0, layer3 = 0,
                                         learning.rate = 2e-6,
                                         momentum = 0.9,
                                         dropout = seq(0, .7, length = len),
                                         activation = 'relu')
                    } else {
                      out <- data.frame(layer1 = sample(2:20, replace = TRUE, size = len),
                                        layer2 = 0,
                                        layer3 = 0,
                                        learning.rate = runif(len),
                                        momentum = runif(len),
                                        dropout = runif(len, max = .7),
                                        activation = sample(c('relu', 'sigmoid', 'tanh', 'softrelu'), replace= TRUE, size=len))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    mxnet::mx.set.seed(21)
                    num_units <- param[grepl("layer[1-9]", names(param))]
                    num_units <- num_units[num_units > 0]
                    if(!is.matrix(x)) x <- as.matrix(x)
                    if(is.numeric(y)) {
                      out <- mxnet::mx.mlp(data = x,
                                           label = y,
                                           hidden_node = num_units,
                                           out_node = 1,
                                           out_activation = "rmse",
                                           learning.rate = param$learning.rate,
                                           momentum = param$momentum,
                                           eval.metric = mxnet::mx.metric.rmse,
                                           array.layout = "rowmajor",
                                           activation = rep( as.character(param$activation), length(num_units)),
                                           # Use He/MSRA when available in R
                                           initializer = mxnet::mx.init.Xavier(factor_type = "avg", magnitude = 3, rnd_type = 'uniform'),
                                           ...)
                    } else {
                      y <- as.numeric(y) - 1
                      out <- mxnet::mx.mlp(data = x,
                                           label = y,
                                           hidden_node = num_units,
                                           out_node = length(unique(y)),
                                           out_activation = "softmax",
                                           learning.rate = param$learning.rate,
                                           momentum = param$momentum,
                                           eval.metric = mxnet::mx.metric.accuracy,
                                           array.layout = "rowmajor",
                                           activation = rep( as.character(param$activation), length(num_units)),
                                           initializer = mxnet::mx.init.Xavier(factor_type = "avg", magnitude = 3, rnd_type = 'uniform'),
                                           ...)
                    }
                    if(last)
                      out <- mxnet::mx.serialize(out)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    pred <- predict(modelFit, newdata, array.layout = 'rowmajor')
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
                    pred <- t(predict(modelFit, newdata, array.layout = 'rowmajor'))
                    colnames(pred) <- modelFit$obsLevels
                    pred
                  },
                  notes = paste("The `mxnet` package is not yet on CRAN.",
                                "See [https://mxnet.apache.org/](https://mxnet.apache.org/) for installation instructions."),
                  tags = c("Neural Network"),
                  sort = function(x) x[order(x$layer1, x$layer2, x$layer3),])
