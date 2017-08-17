modelInfo <- list(label = "The Bayesian lasso",
                  library = "monomvn",
                  type = "Regression",
                  parameters = data.frame(parameter = 'sparsity',
                                          class = "numeric",
                                          label = 'Sparsity Threshold'),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(len == 1) return(data.frame(sparsity = .5))
                    if(search == "grid") {
                      out <-  expand.grid(sparsity = seq(.3, .7, length = len))
                    } else {
                      out <- data.frame(sparsity = runif(len, min = 0, max = 1))
                    }
                    out
                  },
                  loop = function(grid) {   
                    grid <- grid[order(grid$sparsity, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])     
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    mod <- monomvn::blasso(as.matrix(x), y, ...)
                    mod$.percent <- apply(mod$beta, 2, function(x) mean(x != 0))
                    mod$.sparsity <- param$sparsity
                    mod$.betas <- colMeans(mod$beta)
                    mod
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    betas <- modelFit$.betas
                    betas[modelFit$.percent <= modelFit$.sparsity] <- 0
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- (newdata %*% betas)[,1]
                    if(modelFit$icept) out <- out + mean(modelFit$mu)

                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels))
                      for(i in 1:nrow(submodels)) {
                        betas <- modelFit$.betas
                        betas[modelFit$.percent <= submodels$sparsity[i]] <- 0
                        tmp[[i]] <- (newdata %*% betas)[,1]
                        if(modelFit$icept) tmp[[i]] <- tmp[[i]] + mean(modelFit$mu)
                      }
                      out <- c(list(out), tmp) 
                    }
                    out        
                  },
                  predictors = function(x, s = NULL, ...) {
                    x$xNames[x$.percent <= x$.sparsity]
                  },
                  notes = "This model creates predictions using the mean of the posterior distributions but sets some parameters specifically to zero based on the tuning parameter `sparsity`. For example, when `sparsity = .5`, only coefficients where at least half the posterior estimates are nonzero are used.",
                  tags = c("Linear Regression", "Bayesian Model", 
                           "Implicit Feature Selection", "L1 Regularization"),
                  prob = NULL,
                  sort = function(x) x[order(-x$sparsity),])
