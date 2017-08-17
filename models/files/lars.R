modelInfo <- list(label = "Least Angle Regression",
                  library = "lars",
                  type = "Regression",
                  parameters = data.frame(parameter = 'fraction',
                                          class = "numeric",
                                          label = 'Fraction'),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <-  expand.grid(fraction = seq(0.05, 1, length = len))
                    } else {
                      out <- data.frame(fraction = runif(len, min = 0, max = 1))
                    }
                    out
                  },
                  loop = function(grid) {   
                    grid <- grid[order(grid$fraction, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])     
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...)
                    lars::lars(as.matrix(x), y, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit,
                                   as.matrix(newdata),
                                   type = "fit",
                                   mode = "fraction",
                                   s = modelFit$tuneValue$fraction)$fit
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$fraction))
                      {
                        tmp[[j+1]] <- predict(modelFit,
                                              as.matrix(newdata),
                                              type = "fit",
                                              mode = "fraction",
                                              s = submodels$fraction[j])$fit
                      }
                      out <- tmp
                    }
                    out        
                  },
                  predictors = function(x, s = NULL, ...) {
                    if(is.null(s))
                    {
                      if(!is.null(x$tuneValue))
                      {
                        s <- x$tuneValue$fraction
                      } else stop("must supply a vaue of s")
                      out <- predict(x, s = s,
                                     type = "coefficients",
                                     mode = "fraction")$coefficients
                      
                    } else {
                      out <- predict(x, s = s, ...)$coefficients
                      
                    }
                    names(out)[out != 0]
                  },
                  tags = c("Linear Regression", "Implicit Feature Selection", "L1 Regularization"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
