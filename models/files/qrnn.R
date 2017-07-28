modelInfo <- list(label = "Quantile Regression Neural Network",
                  library = "qrnn",
                  loop = NULL,
                  type = c('Regression'),
                  parameters = data.frame(parameter = c('n.hidden', 'penalty', 'bag'),
                                          class = c('numeric', 'numeric', 'logical'),
                                          label = c('#Hidden Units', ' Weight Decay', 'Bagged Models?')),
                  grid = function(x, y, len = NULL, search = "grid") expand.grid(n.hidden = ((1:len) * 2) - 1, 
                                                                penalty = c(0, 10 ^ seq(-1, -4, length = len - 1)),
                                                                bag = FALSE),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    qrnn::qrnn.fit(as.matrix(x), matrix(y),
                                   n.hidden = param$n.hidden,
                                   print.level = 0,
                                   penalty =  param$penalty,
                                   bag= param$bag,
                                   ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                    qrnn::qrnn.predict(as.matrix(newdata), modelFit)[,1],
                  prob = NULL,
                  tags = c("Neural Network", "L2 Regularization", "Quantile Regression", "Bagging",
                           "Ensemble Model", "Robust Model"),
                  sort = function(x) x[order(x$n.hidden, -x$penalty),])
