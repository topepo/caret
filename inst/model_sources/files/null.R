modelInfo <- list(label = "Non-Informative Model",
                  library = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    nullModel(y = y, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata)
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata, type = "prob")
                  },
                  levels = function(x) x$levels,
                  tags = NULL,
                  notes = paste("Since this model always predicts",
                                "the same value, R-squared values",
                                "will always be estimated to be NA."),
                  sort = function(x) x)
