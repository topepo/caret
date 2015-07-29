modelInfo <- list(label = "Robust SIMCA",
                  library = "rrcovHD",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = 'parameter',
                                          class = "character",
                                          label = 'parameter'),
                  grid = function(x, y, len = NULL, search = "grid") {
                    data.frame(parameter = "none")
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    RSimca(x, y, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)@classification,
                  prob = NULL,
                  tags = c('Robust Model', "Linear Classifier"),
                  levels = function(x) names(x@prior),
                  sort = function(x) x)
