modelInfo <- list(label = "Spike and Slab Regression",
                  library = c("spikeslab", "plyr"),
                  type = "Regression",
                  parameters = data.frame(parameter = 'vars',
                                          class = "numeric",
                                          label = 'Variables Retained'),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(vars = caret::var_seq(p = ncol(x), 
                                                              classification = is.factor(y), 
                                                              len = len))
                    } else {
                      out <- data.frame(vars = unique(sample(1:ncol(x), size = len, replace = TRUE)))
                    }
                    out
                  },
                  loop = function(grid) {   
                    grid <- grid[order(grid$vars, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])     
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    require(spikeslab)
                    mod <- spikeslab::spikeslab(x = as.matrix(x), y = y, max.var = param$vars, ...)
                    ## Get a key to go between the column of the path matrix and the 
                    ## number of non-zero coefficients. There can be multiple path 
                    ## values that have the same number of nonzero betas and, 
                    ## in some cases, no solution that has k non-zero values. In 
                    ## this case, we will impute using the next adjacent value. 
                    path <- data.frame(k = apply(mod$gnet.path$path, 1, function(x) sum(x != 0)))
                    path$index <- 1:nrow(path)
                    path <- plyr::ddply(path, plyr::`.`(k), function(x) x[which.min(x$index),])
                    if(all(path$k != ncol(x)))
                      path <- rbind(path, data.frame(k = ncol(x), index = max(path$index)))
                    mod$.path <- path
                    mod$.size <- param$vars
                    mod
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- spikeslab::predict.spikeslab(modelFit, newdata)$yhat.gnet.path
                    if(is.vector(out)) out <- matrix(out, nrow = 1)
                    if(!is.null(submodels)) {
                      vars <- data.frame(k = c(modelFit$.size, submodels$vars))
                      vars$order <- 1:nrow(vars)
                      vars <- merge(vars, modelFit$.path, all.x = TRUE)
                      vars <- vars[order(vars$order),]
                      
                      out <- out[,vars$index]
                      out <- as.list(as.data.frame(out, stringsAsFactors = TRUE))
                    } else {
                      index <- modelFit$.path$index[modelFit$.path$k == modelFit$.size]
                      out <- out[,index]
                    }
                    out        
                  },
                  predictors = function(x, s = NULL, ...) {
                    coefs <- x$gnet
                    names(coefs)[coefs != 0]
                  },
                  notes = paste(
                    "Unlike other packages used by `train`, the `spikeslab`",
                    "package is fully loaded when this model is used."
                  ),
                  tags = c("Linear Regression", "Bayesian Model", 
                           "Implicit Feature Selection"),
                  prob = NULL,
                  sort = function(x) x)
