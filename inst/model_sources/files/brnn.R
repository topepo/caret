modelInfo <- list(label = "Bayesian Regularized Neural Networks",
                  library = "brnn",
                  type = "Regression",
                  parameters = data.frame(parameter = 'neurons',
                                          class = "numeric",
                                          label = '# Neurons'),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(neurons = 1:len)
                    } else {
                      out <- data.frame(neurons = sample(1:20, replace = TRUE, size = len))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...)
                    brnn::brnn(as.matrix(x), y, neurons = param$neurons, ...),
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit,as.matrix(newdata)),
                  prob = NULL,
                  predictors = function(x, s = NULL, ...) 
                    names(x$x_spread),
                  tags = c("Bayesian Model", "Neural Network", "Regularization"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
