modelInfo <- list(label = "Bagged Model",
                  library = "caret",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('vars'),
                                          class = c('numeric'),
                                          label = c('#Randomly Selected Predictors')),
                  grid = function(x, y, len = NULL) 
                    data.frame(vars = ncol(x)),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    out <- bag(x, y, vars = param$vars, ...)
                    out$xNames <- colnames(x)
                    out
                    },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type= "prob"),
                  predictors = function(x, ...)
                    x$xNames,
                  varImp = NULL,
                  tags = c("Bagging", "Ensemble Model"),
                  sort = function(x) x)
