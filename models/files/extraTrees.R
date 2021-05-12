    modelInfo <- list(label = "Random Forest by Randomization",
                      library = c("extraTrees"),
                      loop = NULL,
                      type = c('Regression', 'Classification'),
                      parameters = data.frame(parameter = c('mtry', 'numRandomCuts'),
                                              class = c('numeric', 'numeric'),
                                              label = c('# Randomly Selected Predictors', '# Random Cuts')),
                      grid = function(x, y, len = NULL, search = "grid"){
                        if(search == "grid") {
                          out <- expand.grid(mtry = caret::var_seq(p = ncol(x),
                                                                   classification = is.factor(y),
                                                                   len = len),
                                             numRandomCuts = 1:len)
                        } else {
                          out <- data.frame(mtry = sample(1:ncol(x), size = len, replace = TRUE),
                                            numRandomCuts = sample(1:25, size = len, replace = TRUE))
                        }
                      },
                      fit = function(x, y, wts, param, lev, last, classProbs, ...)
                        extraTrees::extraTrees(x, y, mtry = min(param$mtry, ncol(x)), numRandomCuts = param$numRandomCuts, ...),
                      predict = function(modelFit, newdata, submodels = NULL)
                        predict(modelFit, newdata),
                      prob = function(modelFit, newdata, submodels = NULL)
                        predict(modelFit, newdata, probability = TRUE),
                      levels = function(x) x$obsLevels,
                      tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                      sort = function(x) x[order(x[,1]),])
