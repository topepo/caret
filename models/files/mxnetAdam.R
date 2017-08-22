modelInfo <- list(label = "Neural Network", 
                  library = "mxnet", 
                  type = c('Classification','Regression'),
                  parameters = data.frame(parameter = c('nlayers', 'nnodes', "dropout",
                                                        "beta1", "beta2", "learningrate", "activation"),
                                          class = c(rep('numeric', 6), "character"),
                                          label = c('# of layers', '# hidden units in each layer',
                                                    "dropout rate",  "beta1", "beta2", "learning rate", "activation function")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(nlayers = (1:len), nnodes = 25, 
                                         learningrate = 2e-6, 
                                         beta1 = 0.9, 
                                         beta2 = 0.9999,
                                         dropout = seq(0, .7, length = len), 
                                         activation = 'relu')
                    } else {
                      out <- data.frame(nlayers = sample(2:20, replace = TRUE, size = len), 
                                        nnodes =  sample(c(2:99), replace = TRUE, size = len), 
                                        learningrate = runif(len),
                                        beta1 = runif(len),
                                        beta2 = runif(len),
                                        dropout = runif(len, max = 0.7),
                                        activation = sample(c('relu', 'sigmoid', 'tanh', 'softrelu'), replace= TRUE, size=1))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.matrix(x)) x <- as.matrix(x)
                    if(is.numeric(y)) {
                      mx.set.seed(21)  
                      #browser()
                      out <- mxnet::mx.mlp(data = x, label = y, out_node = 1, out_activation = "rmse", verbose= FALSE,
                                           optimizer = 'adam', eval.metric = mx.metric.rmse, array.layout = "rowmajor", 
                                           learning.rate = param$learningrate,  
                                           beta1 = param$beta1, 
                                           beta2 = param$beta2, 
                                           dropout = param$dropout,
                                           hidden_node = rep(param$nnodes, param$nlayers),
                                           activation = rep( as.character(param$activation), param$nlayers),
                                           initializer = mx.init.Xavier(factor_type = "avg", magnitude = 3, rnd_type = 'uniform'),
                                           ...)
                    } else {
                      y <- as.numeric(y) - 1 
                      mx.set.seed(21)
                      #browser()
                      out <- mxnet::mx.mlp(data = x, label = y, out_node = length(unique(y)), out_activation = "softmax",  verbose= FALSE,
                                          optimizer = 'adam', eval.metric = mx.metric.accuracy, array.layout = "rowmajor", 
                                          learning.rate = param$learningrate, 
                                          beta1 = param$beta1, 
                                          beta2 = param$beta2, 
                                          dropout = param$dropout, 
                                          hidden_node = rep(param$nnodes, param$nlayers),
                                          activation = rep( as.character(param$activation), param$nlayers),
                                          initializer = mx.init.Xavier(factor_type = "avg", magnitude = 3, rnd_type = 'uniform'),
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
                  notes = paste("The mxnet package is not yet on CRAN.",
                                "See http://mxnet.io/ for installation instructions.",
                                "Users are strongly advised to define 'num.round' themselves."),
                  tags = c("Neural Network"),
                  sort = function(x) x[order(x$nlayers, x$nnodes, x$beta1, x$beta2, x$learningrate,x$dropout ),])