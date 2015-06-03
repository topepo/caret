    modelInfo <- list(label = "Random Forest by Randomization",
                      library = c("extraTrees"),
                      loop = NULL,
                      type = c('Regression', 'Classification'),
                      parameters = data.frame(parameter = c('mtry', 'numRandomCuts'),
                                              class = c('numeric', 'numeric'),
                                              label = c('# Randomly Selected Predictors', '# Random Cuts')),
                      grid = function(x, y, len = NULL){
                        expand.grid(mtry = var_seq(p = ncol(x), 
                                                   classification = is.factor(y), 
                                                   len = len), 
                                    numRandomCuts = 1:len)
                      },
                      fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                        extraTrees(x, y, mtry = param$mtry, numRandomCuts = param$numRandomCuts, ...),
                      predict = function(modelFit, newdata, submodels = NULL)
                        predict(modelFit, newdata),
                      prob = function(modelFit, newdata, submodels = NULL)
                        predict(modelFit, newdata, probability = TRUE),
                      levels = function(x) x$obsLevels,
                      tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                      sort = function(x) x[order(x[,1]),])
