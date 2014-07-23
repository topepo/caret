modelInfo <- list(label = "Bayesian Regularized Neural Networks",
                  library = "brnn",
                  type = "Regression",
                  parameters = data.frame(parameter = c('neurons1', 'neurons2'),
                                          class = c("numeric", "numeric"),
                                          label = c('Layer 1 Neurons', 'Layer 2 Neurons')),
                  grid = function(x, y, len = NULL)
                    expand.grid(neurons1 = 1:len, neurons2 = 1:len,
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    brnn(as.matrix(x), y, 
                         neurons1 = param$neurons1, 
                         neurons2 = param$neurons2, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit,as.matrix(newdata)),
                  prob = NULL,
                  predictors = function(x, s = NULL, ...) 
                    names(x$x_spread),
                  tags = c("Bayesian Model", "Neural Network", "Regularization"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
