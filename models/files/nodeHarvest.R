modelInfo <- list(label = "Tree-Based Ensembles",
                  library = c("nodeHarvest"),
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('maxinter', 'mode'),
                                          class = c('numeric', 'character'),
                                          label = c('Maximum Interaction Depth', 'Prediction Mode')),
                  grid = function(x, y, len = NULL)
                    expand.grid(maxinter = 1:len, mode = c("mean", "outbag")),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    if(is.numeric(y))
                    {
                      out <- nodeHarvest(x, y,
                                         maxinter = param$maxinter,
                                         mode = param$mode,
                                         ...)
                    } else {
                      if(length(levels(y)) > 2) stop("Two Class problems only")
                      out <- nodeHarvest(x,
                                         ifelse(y == levels(y)[1], 1, 0),
                                         maxinter = param$maxinter,
                                         mode = param$mode,
                                         ...)                          
                    }
                    out   
                  },
                  predict = function(modelFit, newdata, submodels = NULL){
                    if(modelFit$problemType == "Regression")
                    {
                      predict(modelFit, as.matrix(newdata), maxshow = 0)
                    } else  {
                      prbs <- predict(modelFit, as.matrix(newdata), maxshow = 0)
                      ifelse(prbs > .5, modelFit$obsLevels[1], modelFit$obsLevels[2])
                    }
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, as.matrix(newdata), maxshow = 0)
                    if(is.vector(out))
                    {
                      out <- cbind(out, 1 - out)
                      colnames(out) <- modelFit$obsLevels
                    }
                    out
                  },
                  tags = c("Tree-Based Model", "Implicit Feature Selection", "Ensemble Model"),
                  sort = function(x) x[order(x$maxinter, x$mode),])
