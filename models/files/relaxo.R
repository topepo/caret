modelInfo <- list(label = "Relaxed Lasso",
                  library = c("relaxo", "plyr"),
                  type = c('Regression'),
                  parameters = data.frame(parameter = c('lambda', 'phi'),
                                          class = c('numeric', 'numeric'),
                                          label = c('Penalty Parameter', 'Relaxation Parameter')),
                  grid = function(x, y, len = NULL) {
                    library(relaxo)
                    tmp <- relaxo(as.matrix(x), y)
                    expand.grid(phi = seq(0.1, 0.9, length = len),
                                lambda = 10^seq(log10(min(tmp$lambda)), log10(quantile(tmp$lambda, probs = .9)), length = len))
                  },
                  loop = function(grid) {
                    loop <- ddply(grid,  .(phi), function(x) c(lambda = max(x$lambda)))
                    
                    submodels <- vector(mode = "list", length = nrow(loop))
                    
                    for(i in seq(along = submodels))
                    {
                      submodels[[i]] <- data.frame(lambda = subset(grid, subset = phi == loop$phi[i] 
                                                                                 & lambda < loop$lambda[i])$lambda)
                    } 
                    list(loop = loop, submodels = submodels)           
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    relaxo(as.matrix(x), y, phi = param$phi, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit,
                                   as.matrix(newdata),
                                   lambda = min(max(modelFit$lambda), modelFit$tuneValue$lambda),
                                   phi = modelFit$tuneValue$phi)
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$lambda))
                      {
                        tmp[[j+1]] <- predict(modelFit,
                                              as.matrix(newdata),
                                              lambda = min(max(modelFit$lambda), submodels$lambda[j]),
                                              phi = modelFit$tuneValue$phi)
                      }
                      out <- tmp
                    }
                    
                    out
                  },
                  prob = NULL,
                  tags = c("Implicit Feature Selection", 
                           "L1 Regularization", "L2 Regularization",
                           "Linear Regression"),
                  sort = function(x) x[order(x$phi, -x$lambda),])
