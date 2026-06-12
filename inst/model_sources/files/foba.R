modelInfo <- list(label = "Ridge Regression with Variable Selection",
                  library = "foba",
                  type = "Regression",
                  parameters = data.frame(parameter = c('k', 'lambda'),
                                          class = c("numeric", "numeric"),
                                          label = c('#Variables Retained', 'L2 Penalty')),
                  grid = function(x, y, len = NULL, search = "grid")  {   
                    if(search == "grid") {
                      out <- expand.grid(lambda = 10 ^ seq(-5, -1, length = len),
                                         k = caret::var_seq(p = ncol(x), 
                                                            classification = is.factor(y), 
                                                            len = len))
                    } else {
                      out <- data.frame(lambda = 10^runif(len, min = -5, 1),
                                        k = sample(1:ncol(x), replace = TRUE, size = len))
                    }
                    out
                    },
                  loop = function(grid) {   
                    grid <- grid[order(grid$lambda, grid$k, decreasing = TRUE),, drop = FALSE]  
                    uniqueLambda <- unique(grid$lambda)
                    loop <- data.frame(lambda = uniqueLambda)
                    loop$k <- NA
                    
                    submodels <- vector(mode = "list", length = length(uniqueLambda))
                    
                    for(i in seq(along = uniqueLambda)) {
                      subK <- grid[grid$lambda == uniqueLambda[i],"k"]
                      loop$k[loop$lambda == uniqueLambda[i]] <- subK[which.max(subK)]
                      submodels[[i]] <- data.frame(k = subK[-which.max(subK)])
                    }  
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...)
                    foba::foba(as.matrix(x), y, lambda = param$lambda, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, k = modelFit$tuneValue$k, type = "fit")$fit
                    
                    if(!is.null(submodels))  {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$k)) {
                        tmp[[j+1]] <- predict(modelFit, newdata, k = submodels$k[j], type = "fit")$fit
                      }
                      out <- tmp
                    }
                    out       
                  },
                  predictors = function(x, k = NULL, ...) {
                    if(is.null(k)) {
                      if(!is.null(x$tuneValue)) k <- x$tuneValue$k[1]  else stop("Please specify k")
                    }
                    names(predict(x, k = k, type = "coefficients")$selected.variables)
                  },
                  tags = c("Linear Regression", "Ridge Regression", 
                           "L2 Regularization", "Feature Selection Wrapper"),
                  prob = NULL,
                  sort = function(x) x[order(x$k, -x$lambda),])
