modelInfo <- list(label = "Model Averaged Neural Network",
                  library = "mxnet",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('layer1', 'layer2', 'layer3', "learning.rate", "momentum", "dropout", "repeats"),
                                          class = rep('numeric', 7),
                                          label = c('#Hidden Units in Layer 1', '#Hidden Units in Layer 2', '#Hidden Units in Layer 3',
                                                    "Learning Rate", "Momentum", "Dropout Rate", "#Models")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(layer1 = ((1:len) * 2) - 1, layer2 = 0, layer3 = 0,
                                         learning.rate = 2e-6,
                                         momentum = 0.9,
                                         dropout = seq(0, .7, length = len),
                                         repeats = 5)
                    } else {
                      out <- data.frame(layer1 = sample(2:20, replace = TRUE, size = len),
                                        layer2 = 0,
                                        layer3 = 0,
                                        learning.rate = runif(len),
                                        momentum = runif(len, min = .9),
                                        dropout = runif(len, max = .7),
                                        repeats = sample(1:15, replace = TRUE, size = len))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    num_units <- param[grepl("layer[1-9]", names(param))]
                    num_units <- num_units[num_units > 0]
                    out <- list(models = vector(mode = "list", length = param$repeats))
                    if(!is.matrix(x)) x <- as.matrix(x)
                    if(is.numeric(y)) {
                      for(i in 1:param$repeats) {
                        out$models[[i]] <- mxnet::mx.mlp(data = x,
                                                         label = y,
                                                         hidden_node = num_units,
                                                         out_node = 1,
                                                         out_activation = "rmse",
                                                         learning.rate = param$learning.rate,
                                                         momentum = param$momentum,
                                                         eval.metric = mxnet::mx.metric.rmse,
                                                         array.layout = "rowmajor",
                                                         ...)
                      }
                    } else {
                      y <- as.numeric(y) - 1
                      for(i in 1:param$repeats) {
                        out$models[[i]] <- mxnet::mx.mlp(data = x,
                                                         label = y,
                                                         hidden_node = num_units,
                                                         out_node = length(unique(y)),
                                                         out_activation = "softmax",
                                                         learning.rate = param$learning.rate,
                                                         momentum = param$momentum,
                                                         eval.metric = mxnet::mx.metric.accuracy,
                                                         array.layout = "rowmajor",
                                                         ...)
                      }
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    for(i in seq(along = modelFit$models)) {
                      tmp <- predict(modelFit$models[[i]], newdata)
                      pred <- if(i == 1) tmp else pred + tmp
                    }
                    pred <- pred/length(modelFit$models)

                    if(modelFit$problemType == "Regression") {
                      pred <- pred[1,]
                    } else {
                      pred <- apply(pred, 2, function(x) x/sum(x))
                      pred <- modelFit$obsLevels[apply(pred, 2, which.max)]
                    }
                    pred
                  },
                  prob =  function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    for(i in seq(along = modelFit$models)) {
                      tmp <- predict(modelFit$models[[i]], newdata)
                      pred <- if(i == 1) tmp else pred + tmp
                    }
                    pred <- pred/length(modelFit$models)
                    pred <- t(apply(pred, 2, function(x) x/sum(x)))

                    colnames(pred) <- modelFit$obsLevels
                    pred
                  },
                  predictors = function(x, ...)  {
                    if(any(names(x) == "xNames")) x$xNames else NA
                  },
                  notes = paste("The `mxnet` package is not yet on CRAN.",
                                "See [https://mxnet.apache.org/](https://mxnet.apache.org/) for installation instructions."),
                  tags = c("Neural Network", "Ensemble Model"),
                  sort = function(x) x[order(x$layer1, x$layer2, x$layer3),])
