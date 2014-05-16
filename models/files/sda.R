modelInfo <- list(label = "Shrinkage Discriminant Analysis",
                  library = c("sda"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('diagonal','lambda'),
                                          class = c("logical", "numeric"),
                                          label = c('Diagonalize','shrinkage')),
                  grid = function(x, y, len = NULL) 
                    data.frame(diagonal = FALSE, lambda = seq(0, 1, length = len)),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...)
                    sda::sda(as.matrix(x), 
                             y, 
                             diagonal = param$diagonal, 
                             lambda = param$lambda, 
                             ...),
                  predict = function(modelFit, newdata, submodels = NULL)
                    sda::predict.sda(modelFit, as.matrix(newdata))$class,
                  prob = function(modelFit, newdata, submodels = NULL)
                    sda::predict.sda(modelFit, as.matrix(newdata))$posterior,
                  predictors = function(x, ...) {
                    colnames(x$beta)
                  },
                  tags = c("Discriminant Analysis", "Regularization", "Linear Classifier"),
                  sort = function(x) x[order(x$diagonal, x$lambda),])
