modelInfo <- list(label = "Robust Regularized Linear Discriminant Analysis",
                  library = "rrlda",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('lambda', 'hp', 'penalty'),
                                          class = c('numeric', 'numeric', 'character'),
                                          label = c('Penalty Parameter', 'Robustness Parameter', 'Penalty Type')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(lambda = (1:len)*.25,
                                         hp = seq(.5, 1, length = len),
                                         penalty = "L2")
                    } else {
                      out <- data.frame(lambda = 10^runif(len, min = -5, 1),
                                        hp = runif(len, min = 0, 1),
                                        penalty = sample(c("L1", "L2"), size = len, replace = TRUE))
                    }
                    out
                  }, 
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    require("rrlda")
                    rrlda:::rrlda(x, as.numeric(y), lambda = param$lambda,
                                  hp = param$hp, penalty = as.character(param$penalty), ...)    
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)$class
                    modelFit$obsLevels[as.numeric(out)]
                    },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)$posterior
                    colnames(out) <- modelFit$obsLevels
                    out
                    },
                  levels = function(x) x$obsLevels,
                  notes = paste(
                    "Unlike other packages used by `train`, the `rrlda`",
                    "package is fully loaded when this model is used."
                  ),
                  tags = c("Discriminant Analysis", "Robust Model", "Regularization", "Linear Classifier"),
                  sort = function(x) x[order(-x$lambda),])
