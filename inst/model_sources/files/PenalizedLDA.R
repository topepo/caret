modelInfo <- list(label = "Penalized Linear Discriminant Analysis",
                  library = c("penalizedLDA", "plyr"),
                  loop = function(grid) {
                    loop <- plyr::ddply(grid, .(lambda), function(x) c(K = max(x$K)))
                    if(length(unique(loop$K)) == 1) return(list(loop = loop, submodels = NULL))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$K))
                    {
                      index <- which(grid$lambda == loop$lambda[i])
                      subK <- grid[index, "K"]
                      otherK <- data.frame(K = subK[subK != loop$K[i]])
                      if(nrow(otherK) > 0) submodels[[i]] <- otherK
                    }    
                    list(loop = loop, submodels = submodels)
                  },
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('lambda', 'K'),
                                          class = c('numeric', 'numeric'),
                                          label = c('L1 Penalty', '#Discriminant Functions')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(lambda = 10 ^ seq(-1, -4, length = len), 
                                        K = length(levels(y)) - 1)
                    } else {
                      out <- data.frame(lambda = 10^runif(len, min = -5, 1), 
                                        K = sample(1:(length(levels(y)) - 1), size = len, replace = TRUE))
                    }
                    out
                  }, 
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    penalizedLDA::PenalizedLDA(as.matrix(x), as.numeric(y),
                                                lambda = param$lambda,
                                                K = param$K,
                                                ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out0 <- predict(modelFit, newdata)$ypred
                    out <- out0[,ncol(out0)]
                    out <- modelFit$obsLevels[out]
                    if(!is.null(submodels))
                    {
                      tmp <- out0[, submodels$K,drop = FALSE]
                      tmp <- apply(tmp, 2, function(x, l) l[x], l = modelFit$obsLevels)
                      out <- as.data.frame(cbind(out, tmp), stringsAsFactors = FALSE)                                 
                    }
                    out
                  },
                  levels = function(x) x$obsLevels,
                  prob = NULL,
                  tags = c("Discriminant Analysis", "L1 Regularization", 
                           "Implicit Feature Selection", "Linear Classifier"),
                  sort = function(x) x[order(x$lambda, x$K),])
