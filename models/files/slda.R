modelInfo <- list(label = "Stabilized Linear Discriminant Analysis",
                  library = c("ipred"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('parameter'),
                                          class = c("character"),
                                          label = c('none')),
                  grid = function(x, y, len = NULL) 
                    data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    slda(.outcome ~ ., data = dat, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata)$posterior,
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else predictors(x$mylda),
                  tags = c("Discriminant Analysis", "Linear Classifier"),
                  sort = function(x) x)
