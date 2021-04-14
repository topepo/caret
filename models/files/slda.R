modelInfo <- list(label = "Stabilized Linear Discriminant Analysis",
                  library = c("ipred"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('parameter'),
                                          class = c("character"),
                                          label = c('none')),
                  grid = function(x, y, len = NULL, search = "grid") 
                    data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    ipred::slda(.outcome ~ ., data = dat, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)$class
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)$posterior
                  },
                  levels = function(x) x$obsLevels,
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else predictors(x$mylda),
                  tags = c("Discriminant Analysis", "Linear Classifier"),
                  sort = function(x) x)
