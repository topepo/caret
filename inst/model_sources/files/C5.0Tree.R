modelInfo <- list(label = "Single C5.0 Tree", 
                  library = "C50",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c('parameter'),
                                          class = c("character"),
                                          label = c('none')),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    C50:::C5.0.default(x = x, y = y, weights = wts, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata, type= "prob"),
                  predictors = function(x, ...) {
                    vars <- C50::C5imp(x, metric = "splits")
                    rownames(vars)[vars$Overall > 0]
                  },
                  levels = function(x) x$obsLevels,
                  varImp = function(object, ...) C50::C5imp(object, ...),
                  tags = c("Tree-Based Model", "Implicit Feature Selection", 
                           "Handle Missing Predictor Data", "Accepts Case Weights"),
                  sort = function(x) x[order(x[,1]),],
                  trim = function(x) {
                    x$boostResults <- NULL
                    x$size <- NULL
                    x$call <- NULL
                    x$output <- NULL
                    x
                  })
