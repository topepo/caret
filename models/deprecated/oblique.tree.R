modelInfo <- list(label = "Oblique Trees",
                  library = c("oblique.tree"),
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('oblique.splits', 'variable.selection'),
                                          class = c('character', 'character'),
                                          label = c('Oblique Splits', 'Variable Selection Method')),
                  grid = function(x, y, len = NULL, search = "grid")
                    expand.grid(oblique.splits = c("only", "on", "off"),
                                variable.selection = c("none", "model.selection.aic")),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    dat <- if(is.data.frame(x)) x else as.data.frame(x)
                    dat$.outcome <- y
                    oblique.tree::oblique.tree(.outcome ~ ., data = dat,
                                               oblique.splits = as.character(param$oblique.splits),
                                               variable.selection = as.character(param$variable.selection),
                                               ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL){
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    newdata$.outcome <- factor(rep(modelFit$obsLevels[1], nrow(newdata)),
                                               levels = modelFit$obsLevels)
                    predict(modelFit, newdata, type = "class")
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    newdata$.outcome <- factor(rep(modelFit$obsLevels[1], nrow(newdata)),
                                               levels = modelFit$obsLevels)
                    predict(modelFit, newdata, type = "vector")
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Tree-Based Model", "Implicit Feature Selection", "Oblique Tree"),
                  sort = function(x) x[order(x$variable.selection),])
