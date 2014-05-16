modelInfo <- list(label = "Bayesian Regularized Neural Networks",
                  library = "brnn",
                  type = "Regression",
                  parameters = data.frame(parameter = 'neurons',
                                          class = "numeric",
                                          label = '# Neurons'),
                  grid = function(x, y, len = NULL)
                    expand.grid(neurons = 1:len),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    brnn(as.matrix(x), y, neurons = param$neurons, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit,as.matrix(newdata)),
                  prob = NULL,
                  predictors = function(x, s = NULL, ...) 
                    names(x$x_spread),
                  tags = c("Bayesian Model", "Neural Network", "Regularization"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
