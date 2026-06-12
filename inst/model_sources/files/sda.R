modelInfo <- list(label = "Shrinkage Discriminant Analysis",
                  library = c("sda"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('diagonal','lambda'),
                                          class = c("logical", "numeric"),
                                          label = c('Diagonalize','shrinkage')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(diagonal = FALSE, lambda = seq(0, 1, length = len))
                    } else {
                      out <- data.frame(lambda = runif(len, min = 0, 1),
                                        diagonal = sample(c(TRUE, FALSE), size = len, replace = TRUE))
                    }
                    out
                  },
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
                  levels = function(x) x$obsLevels,
                  tags = c("Discriminant Analysis", "Regularization", "Linear Classifier"),
                  sort = function(x) x[order(x$diagonal, x$lambda),])
