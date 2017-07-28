modelInfo <- list(label = "High Dimensional Discriminant Analysis",
                  library = "HDclassif",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('threshold', 'model'),
                                          class = c('character', 'numeric'),
                                          label = c('Threshold', 'Model Type')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    mods <- c("AkjBkQkDk", "AkBkQkDk", "ABkQkDk", "AkjBQkDk", "AkBQkDk", 
                              "ABQkDk", "AkjBkQkD", "AkBkQkD", "ABkQkD", "AkjBQkD", 
                              "AkBQkD", "ABQkD", "AjBQD", "ABQD")
                    if(search == "grid") {
                      out <- expand.grid(model = c("all"), 
                                         threshold = seq(0.05, .3, length = len))
                    } else {
                      out <- data.frame(model = sample(mods, size = len, replace = TRUE),
                                        threshold = runif(len, min = 0, max = 1))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    HDclassif::hdda(x, y, model = as.character(param$model), threshold = param$threshold, ...)
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    as.character(predict(modelFit, newdata)$class)
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    data.frame(unclass(predict(modelFit, newdata)$posterior))
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Discriminant Analysis", "Linear Classifier"),
                  sort = function(x) x[order(-x$threshold),])
