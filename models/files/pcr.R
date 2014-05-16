modelInfo <- list(label = "Principal Component Analysis",
                  library = "pls",
                  type = "Regression",
                  parameters = data.frame(parameter = 'ncomp',
                                          class = "numeric",
                                          label = '#Components'),
                  grid = function(x, y, len = NULL) 
                    data.frame(ncomp = seq(1, min(ncol(x) - 1, len), by = 1)),
                  loop = function(grid) {     
                    grid <- grid[order(grid$ncomp, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])  
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    dat <- x
                    dat$.outcome <- y
                    pcr(.outcome ~ ., data = dat, ncomp = param$ncomp, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- as.vector(pls:::predict.mvr(modelFit, newdata, ncomp = max(modelFit$ncomp)))
                    
                    if(!is.null(submodels))
                    {
                      tmp <- apply(predict(modelFit, newdata, ncomp = submodels$ncomp), 3, function(x) list(x))
                      tmp <-  as.data.frame(tmp)
                      out <- c(list(out), as.list(tmp))
                    }
                    out            
                  },
                  predictors = function(x, ...) rownames(x$projection),
                  tags = c("Linear Regression", "Feature Extraction"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
