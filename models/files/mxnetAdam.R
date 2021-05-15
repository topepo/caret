modelInfo <- list(label = "Neural Network",
                  library = "mxnet",
                  type = c('Classification','Regression'),
                  parameters = data.frame(parameter = c("layer1", "layer2", "layer3", "dropout",
                                                        "beta1", "beta2", "learningrate", "activation"),
                                          class = c(rep('numeric', 7), "character"),
                                          label = c('#Hidden Units in Layer 1', '#Hidden Units in Layer 2', '#Hidden Units in Layer 3',
                                                    "Dropout Rate",  "beta1", "beta2", "Learning Rate", "Activation Function")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(layer1 = ((1:len) * 4) - 1, layer2 = 0, layer3 = 0,
                                         learningrate = 2e-6,
                                         beta1 = 0.9,
                                         beta2 = 0.9999,
                                         dropout = seq(0, .7, length = len),
                                         activation = 'relu')
                    } else {
                      out <- data.frame(layer1 = sample(2:20, replace = TRUE, size = len),
                                        layer2 = 0,
                                        layer3 = 0,
                                        learningrate = runif(len),
                                        beta1 = runif(len),
                                        beta2 = runif(len),
                                        dropout = runif(len, max = 0.7),
                                        activation = sample(c('relu', 'sigmoid', 'tanh', 'softrelu'), replace= TRUE, size=len))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    num_units <- param[grepl("layer[1-9]", names(param))]
                    num_units <- num_units[num_units > 0]
                    if(!is.matrix(x)) x <- as.matrix(x)
                    if(is.numeric(y)) {
                      mxnet::mx.set.seed(21)
                      out <- mxnet::mx.mlp(data = x, label = y, out_node = 1, out_activation = "rmse",
                                           optimizer = 'adam', eval.metric = mxnet::mx.metric.rmse, array.layout = "rowmajor",
                                           learning.rate = param$learningrate,
                                           beta1 = param$beta1,
                                           beta2 = param$beta2,
                                           dropout = param$dropout,
                                           hidden_node = num_units,
                                           activation = rep( as.character(param$activation), length(num_units)),
                                           # Consider using He/MSRA paper when available in R
                                           initializer = mxnet::mx.init.Xavier(factor_type = "avg", magnitude = 3, rnd_type = 'uniform'),
                                           ...)
                    } else {
                      y <- as.numeric(y) - 1
                      mxnet::mx.set.seed(21)
                      out <- mxnet::mx.mlp(data = x, label = y, out_node = length(unique(y)), out_activation = "softmax",
                                          optimizer = 'adam', eval.metric = mxnet::mx.metric.accuracy, array.layout = "rowmajor",
                                          learning.rate = param$learningrate,
                                          beta1 = param$beta1,
                                          beta2 = param$beta2,
                                          dropout = param$dropout,
                                          hidden_node = num_units,
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
                    } else {
                      pred <- modelFit$obsLevels[apply(pred, 2, which.max)]
                    }
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
                                "See [https://mxnet.apache.org/](https://mxnet.apache.org/) for installation instructions.",
                                "Users are strongly advised to define `num.round` themselves."),
                  tags = c("Neural Network"),
                  sort = function(x) x[order(x$layer1, x$layer2,x$layer3,
                                             x$beta1, x$beta2, x$learningrate,x$dropout ),])
