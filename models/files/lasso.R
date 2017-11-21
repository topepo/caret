modelInfo <- list(label = "The lasso",
                  library = "elasticnet",
                  type = "Regression",
                  parameters = data.frame(parameter = 'fraction',
                                          class = "numeric",
                                          label = 'Fraction of Full Solution'),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <-  expand.grid(fraction = seq(.1, .9, length = len))
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
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    elasticnet::enet(as.matrix(x), y, lambda = 0, ...)
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- elasticnet::predict.enet(modelFit, 
                                   newdata, 
                                   s = modelFit$tuneValue$fraction, 
                                   mode = "fraction")$fit

                    if(!is.null(submodels)) {
                      if(nrow(submodels) > 1) {
                        out <- c(
                          list(if(is.matrix(out)) out[,1]  else out),
                          as.list(
                            as.data.frame(
                              elasticnet::predict.enet(modelFit,
                                      newx = newdata,
                                      s = submodels$fraction,
                                      mode = "fraction")$fit)))
                        
                      } else {
                        tmp <- elasticnet::predict.enet(
                          modelFit,
                          newx = newdata,
                          s = submodels$fraction,
                          mode = "fraction")$fit
                        out <- c(list(if(is.matrix(out)) out[,1]  else out),  list(tmp))
                      }
                    }
                    out        
                  },
                  predictors = function(x, s = NULL, ...) {
                    if(is.null(s)) {
                      if(!is.null(x$tuneValue))  {
                        s <- x$tuneValue$fraction
                      } else stop("must supply a vaue of s")
                      out <- elasticnet::predict.enet(
                        x, s = s,
                        type = "coefficients",
                        mode = "fraction")$coefficients
                      
                    } else {
                      out <- elasticnet::predict.enet(x, s = s)$coefficients
                    }
                    names(out)[out != 0]
                  },
                  tags = c("Linear Regression", "Implicit Feature Selection", "L1 Regularization"),
                  prob = NULL,
                  sort = function(x) x[order(x$fraction),])
