modelInfo <- list(label = "SIMCA",
                  library = c("rrcov","rrcovHD"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = 'parameter',
                                          class = "character",
                                          label = 'parameter'),
                  grid = function(x, y, len = NULL, search = "grid") {
                    data.frame(parameter = "none")
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    rrcovHD::CSimca(x, y, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    rrcov::predict(modelFit, newdata)@classification,
                  prob = NULL,
                  tags = c('Robust Model'),
                  levels = function(x) names(x@prior),
                  sort = function(x) x)
