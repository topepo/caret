modelInfo <- list(label = "Supervised Principal Component Analysis",
                  library = "superpc",
                  type = c('Regression'),
                  parameters = data.frame(parameter = c('threshold', 'n.components'),
                                          class = c('numeric', 'numeric'),
                                          label = c('Threshold', '#Components')),
                  grid = function(x, y, len = NULL) {
                    expand.grid(n.components = 1:3, threshold = seq(.1, .9, length = len))
                  },
                  loop = function(grid) {
                    largest <- which(grid$n.components == max(grid$n.components) &
                                       grid$threshold == max(grid$threshold))
                    loop <- grid[largest,, drop = FALSE]
                    submodels <- list(grid[-largest,, drop = FALSE])
                    list(loop = loop, submodels = submodels)           
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    out <- superpc.train(list(x = t(x), y = y),
                                         type = "regression",
                                         ...)
                    ## prediction will need to source data, so save that too
                    out$data <- list(x = t(x), y = y)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- superpc.predict(modelFit,
                                           modelFit$data,
                                           newdata = list(x=t(newdata)),
                                           n.components = modelFit$tuneValue$n.components,
                                           threshold = modelFit$tuneValue$threshold)$v.pred.1df
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$threshold))
                      {
                        tmp[[j+1]] <- superpc.predict(modelFit,
                                                      modelFit$data,
                                                      newdata = list(x=t(newdata)),
                                                      threshold = submodels$threshold[j],
                                                      n.components = submodels$n.components[j])$v.pred.1df
                      }
                      out <- tmp
                    }
                    
                    out
                  },
                  prob = NULL,
                  tags = c("Feature Extraction", "Linear Regression"),
                  sort = function(x) x[order(x$threshold, x$n.components),])
