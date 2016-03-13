modelInfo <- list(label = "eXtreme Gradient Boosting",
                  library = c("xgboost", "plyr"),
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('nrounds', 'max_depth', 'eta',
                                                        'gamma', 'colsample_bytree',
                                                        'min_child_weight'),
                                          class = rep("numeric", 6),
                                          label = c('# Boosting Iterations', 'Max Tree Depth', 
                                                    'Shrinkage', "Minimum Loss Reduction",
                                                    'Subsample Ratio of Columns',
                                                    'Minimum Sum of Instance Weight')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(max_depth = seq(1, len),
                                         nrounds = floor((1:len) * 50),
                                         eta = c(.3, .4),
                                         gamma = 0,
                                         colsample_bytree = c(.6, .8),
                                         min_child_weight = c(1))
                    } else {
                      out <- data.frame(nrounds = sample(1:1000, size = len, replace = TRUE),
                                        max_depth = sample(1:10, replace = TRUE, size = len),         
                                        eta = runif(len, min = .001, max = .6),
                                        gamma = runif(len, min = 0, max = 10),
                                        colsample_bytree = runif(len, min = .3, max = .7),
                                        min_child_weight = sample(0:20, size = len, replace = TRUE))
                      out$nrounds <- floor(out$nrounds)
                      out <- out[!duplicated(out),]
                    }
                    out
                  },
                  loop = function(grid) {     
                    loop <- ddply(grid, c("eta", "max_depth", "gamma", "colsample_bytree", "min_child_weight"),
                                  function(x) c(nrounds = max(x$nrounds)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$nrounds)) {
                      index <- which(grid$max_depth == loop$max_depth[i] & 
                                       grid$eta == loop$eta[i] & 
                                       grid$gamma == loop$gamma[i] &
                                       grid$colsample_bytree == loop$colsample_bytree[i] &
                                       grid$min_child_weight == loop$min_child_weight[i])
                      trees <- grid[index, "nrounds"] 
                      submodels[[i]] <- data.frame(nrounds = trees[trees != loop$nrounds[i]])
                    }    
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(is.factor(y)) {
                      if(length(lev) == 2) {
                        y <- ifelse(y == lev[1], 1, 0) 
                        dat <- xgb.DMatrix(as.matrix(x), label = y)
                        out <- xgb.train(list(eta = param$eta, 
                                              max_depth = param$max_depth,
                                              gamma = param$gamma,
                                              colsample_bytree = param$colsample_bytree,
                                              min_child_weight = param$min_child_weight), 
                                         data = dat,
                                         nrounds = param$nrounds,
                                         objective = "binary:logistic",
                                         ...)
                      } else {
                        y <- as.numeric(y) - 1
                        dat <- xgb.DMatrix(as.matrix(x), label = y)
                        out <- xgb.train(list(eta = param$eta, 
                                              max_depth = param$max_depth,
                                              gamma = param$gamma,
                                              colsample_bytree = param$colsample_bytree,
                                              min_child_weight = param$min_child_weight), 
                                         data = dat,
                                         num_class = length(lev),
                                         nrounds = param$nrounds,
                                         objective = "multi:softprob",
                                         ...)
                      }     
                    } else {
                      dat <- xgb.DMatrix(as.matrix(x), label = y)
                      out <- xgb.train(list(eta = param$eta, 
                                            max_depth = param$max_depth,
                                            gamma = param$gamma,
                                            colsample_bytree = param$colsample_bytree,
                                            min_child_weight = param$min_child_weight), 
                                       data = dat,
                                       nrounds = param$nrounds,
                                       objective = "reg:linear",
                                       ...)
                    }
                    out
                    
                    
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    newdata <- xgb.DMatrix(as.matrix(newdata))
                    out <- predict(modelFit, newdata)
                    if(modelFit$problemType == "Classification") {
                      if(length(modelFit$obsLevels) == 2) {
                        out <- ifelse(out >= .5, 
                                      modelFit$obsLevels[1], 
                                      modelFit$obsLevels[2])
                      } else {
                        out <- matrix(out, ncol = length(modelFit$obsLevels), byrow = TRUE)
                        out <- modelFit$obsLevels[apply(out, 1, which.max)]
                      }
                    }
                    
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$nrounds)) {
                        tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
                        if(modelFit$problemType == "Classification") {
                          if(length(modelFit$obsLevels) == 2) {
                            tmp_pred <- ifelse(tmp_pred >= .5, 
                                               modelFit$obsLevels[1], 
                                               modelFit$obsLevels[2])
                          } else {
                            tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), byrow = TRUE)
                            tmp_pred <- modelFit$obsLevels[apply(tmp_pred, 1, which.max)]
                          }
                        }
                        tmp[[j+1]]  <- tmp_pred
                      }
                      out <- tmp
                    }
                    out  
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    newdata <- xgb.DMatrix(as.matrix(newdata))
                    out <- predict(modelFit, newdata)
                    if(length(modelFit$obsLevels) == 2) {
                      out <- cbind(out, 1 - out)
                      colnames(out) <- modelFit$obsLevels
                    } else {
                      out <- matrix(out, ncol = length(modelFit$obsLevels), byrow = TRUE)
                      colnames(out) <- modelFit$obsLevels
                    }
                    out <- as.data.frame(out)
                    
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$nrounds)) {
                        tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
                        if(length(modelFit$obsLevels) == 2) {
                          tmp_pred <- cbind(tmp_pred, 1 - tmp_pred)
                          colnames(tmp_pred) <- modelFit$obsLevels
                        } else {
                          tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), byrow = TRUE)
                          colnames(tmp_pred) <- modelFit$obsLevels
                        }
                        tmp_pred <- as.data.frame(tmp_pred)
                        tmp[[j+1]]  <- tmp_pred
                      }
                      out <- tmp
                    }
                    out  
                  },
                  predictors = function(x, ...) {
                    imp <- xgb.importance(x$xNames, model = x)
                    x$xNames[x$xNames %in% imp$Feature]
                  },
                  varImp = function(object, numTrees = NULL, ...) {
                    imp <- xgb.importance(object$xNames, model = object)
                    imp <- as.data.frame(imp)[, 1:2]
                    rownames(imp) <- as.character(imp[,1])
                    imp <- imp[,2,drop = FALSE]
                    colnames(imp) <- "Overall"
                    imp   
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Tree-Based Model", "Boosting", "Ensemble Model", "Implicit Feature Selection"),
                  sort = function(x) {
                    # This is a toss-up, but the # trees probably adds
                    # complexity faster than number of splits
                    x[order(x$nrounds, x$max_depth, x$eta, x$gamma, x$colsample_bytree, x$min_child_weight),] 
                  })
