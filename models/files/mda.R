modelInfo <- list(label = "Mixture Discriminant Analysis",
                  library = "mda",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('subclasses'),
                                          class = c('numeric'),
                                          label = c('#Subclasses Per Class')),
                  grid = function(x, y, len = NULL, search = "grid") 
                    data.frame(subclasses = (1:len) + 1),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    mda::mda(as.formula(".outcome ~ ."), data = dat,
                             subclasses = param$subclasses, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "posterior"),
                  predictors = function(x, ...) predictors(x$terms),
                  levels = function(x) x$obsLevels,
                  tags = c("Discriminant Analysis", "Mixture Model"),
                  sort = function(x) x[order(x[,1]),])
