# modelInfo
xgbDART <- list(label = "eXtreme Gradient Boosting",
                  library = c("xgboost", "plyr"),
                  check = function(pkg) {
                    requireNamespace("xgboost")
                    current <- packageDescription("xgboost")$Version
                    expected <- "0.6.4.6"
                    if(compareVersion(current, expected) < 0)
                      stop("This modeling workflow requires xgboost version ",
                           expected, "or greater. Consider using the drat repo.", call. = FALSE)
                  },
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c("nrounds", 
                                                        "max_depth",
                                                        "eta",
                                                        "subsample",
                                                        "rate_drop",
                                                        "skip_drop", 
                                                        "min_child_weight",
                                                        "sample_type"),
                                          class = c(rep("numeric", 7), 'character'),
                                          label = c('# Boosting Iterations', 
                                                    'Max Tree Depth',
                                                    'Shrinkage',
                                                    'Subsample Percentage',
                                                    "Fraction of previous trees to drop during dropout",
                                                    'Probability of skipping dropout during an iteration',
                                                    'Minimum Sum of Instance Weight',
                                                    'Sampling algorithm')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(nrounds = floor((1:len) * 50),
                                         max_depth = seq(1, len),
                                         eta = c(0.3, 0.4), # "Usually" one should be the lowest possible
                                         subsample = seq(.5, 1, length = len),
                                         rate_drop = c(0.005, 0.20),
                                         skip_drop = c(0.005, 0.20),  
                                         min_child_weight = c(1),
                                         sample_type = "uniform")
                    } else {
                      out <- data.frame(nrounds = sample(1:1000, size = len, replace = TRUE),
                                        max_depth = sample(1:10, replace = TRUE, size = len),
                                        eta = runif(len, min = 0.001, max = 0.6),
                                        subsample = runif(len, min = .25, max = 1),
                                        rate_drop = runif(len, min = 0.0, max = 0.25),
                                        skip_drop = runif(len, min = 0.0, max = 0.25),
                                        min_child_weight = sample(0:20, size = len, replace = TRUE),
                                        sample_type = sample(c("uniform","weighted"), replace = TRUE, size = len))
                      out$nrounds <- floor(out$nrounds)
                      out <- out[!duplicated(out),]
                    }
                    out
                  },
                  loop = function(grid) {
                    loop <- plyr::ddply(grid, c( "max_depth","eta", "rate_drop",
                                                 "skip_drop", "min_child_weight",
                                                 "subsample", "sample_type"),
                                        function(x) c(nrounds = max(x$nrounds)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$nrounds)) {
                      index <- which(grid$max_depth == loop$max_depth[i] &
                                       grid$eta == loop$eta[i] &
                                       grid$rate_drop == loop$rate_drop[i] &
                                       grid$skip_drop == loop$skip_drop[i] &
                                       grid$min_child_weight == loop$min_child_weight[i]&
                                       grid$subsample == loop$subsample[i] &
                                       grid$sample_type == loop$sample_type[i] )
                      trees <- grid[index, "nrounds"]
                      submodels[[i]] <- data.frame(nrounds = trees[trees != loop$nrounds[i]])
                    }
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!inherits(x, "xgb.DMatrix"))
                      x <- as.matrix(x)
                    
                    if(is.factor(y)) {
                      
                      if(length(lev) == 2) {
                        
                        y <- ifelse(y == lev[1], 1, 0)
                        
                        if(!inherits(x, "xgb.DMatrix"))
                          x <- xgboost::xgb.DMatrix(x, label = y, missing = NA) else
                            setinfo(x, "label", y)
                        
                        if (!is.null(wts))
                          setinfo(x, 'weight', wts)
                        
                        out <- xgboost::xgb.train(list(max_depth = param$max_depth,
                                                       eta = param$eta,
                                                       rate_drop = param$rate_drop,
                                                       skip_drop = param$skip_drop,
                                                       min_child_weight = param$min_child_weight,
                                                       sample_type = param$sample_type,
                                                       subsample = param$subsample),
                                                  data = x,
                                                  nrounds = param$nrounds,
                                                  objective = "binary:logistic",
                                                  booster = 'dart',
                                                  ...)
                      } else {
                        
                        y <- as.numeric(y) - 1
                        
                        if(!inherits(x, "xgb.DMatrix"))
                          x <- xgboost::xgb.DMatrix(x, label = y, missing = NA) else
                            setinfo(x, "label", y)
                        
                        if (!is.null(wts))
                          setinfo(x, 'weight', wts)
                        
                        out <- xgboost::xgb.train(list(max_depth = param$max_depth,
                                                       eta = param$eta,
                                                       rate_drop = param$rate_drop,
                                                       skip_drop = param$skip_drop,
                                                       min_child_weight = param$min_child_weight,
                                                       subsample = param$subsample,
                                                       sample_type = param$sample_type),
                                                  data = x,
                                                  num_class = length(lev),
                                                  nrounds = param$nrounds,
                                                  objective = "multi:softprob",
                                                  booster = 'dart',
                                                  ...)
                      }
                    } else {
                      
                      if(!inherits(x, "xgb.DMatrix"))
                        x <- xgboost::xgb.DMatrix(x, label = y, missing = NA) else
                          setinfo(x, "label", y)
                      
                      if (!is.null(wts))
                        setinfo(x, 'weight', wts)
                      
                      # browser()
                      out <- xgboost::xgb.train(list(max_depth = param$max_depth,
                                                     eta = param$eta,
                                                     rate_drop = param$rate_drop,
                                                     skip_drop = param$skip_drop,
                                                     min_child_weight = param$min_child_weight,
                                                     subsample = param$subsample,
                                                     sample_type = param$sample_type),
                                                data = x,
                                                nrounds = param$nrounds,
                                                objective = "reg:linear",
                                                booster= "dart",
                                                ...)
                    }
                    out
                    
                    
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!inherits(newdata, "xgb.DMatrix")) {
                      newdata <- as.matrix(newdata)
                      newdata <- xgboost::xgb.DMatrix(data=newdata, missing = NA)
                    }
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
                    if(!inherits(newdata, "xgb.DMatrix")) {
                      newdata <- as.matrix(newdata)
                      newdata <- xgboost::xgb.DMatrix(data=newdata, missing = NA)
                    }
                    
                    if( !is.null(modelFit$param$objective) && modelFit$param$objective == 'binary:logitraw'){
                      p <- predict(modelFit, newdata)
                      out <- exp(p)/(1+exp(p))
                    } else {
                      out <- predict(modelFit, newdata)
                    }
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
                    imp <- xgboost::xgb.importance(x$xNames, model = x)
                    x$xNames[x$xNames %in% imp$Feature]
                  },
                  varImp = function(object, numTrees = NULL, ...) {
                    imp <- xgboost::xgb.importance(object$xNames, model = object)
                    imp <- as.data.frame(imp)[, 1:2]
                    rownames(imp) <- as.character(imp[,1])
                    imp <- imp[,2,drop = FALSE]
                    colnames(imp) <- "Overall"
                    
                    missing <- object$xNames[!(object$xNames %in% rownames(imp))]
                    missing_imp <- data.frame(Overall=rep(0, times=length(missing)))
                    rownames(missing_imp) <- missing
                    imp <- rbind(imp, missing_imp)
                    
                    imp
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Tree-Based Model", "Boosting", "Ensemble Model", "Implicit Feature Selection", "Accepts Case Weights"),
                  sort = function(x) { 
                    x[order(x$nrounds,   x$max_depth,        x$eta,       x$rate_drop,
                            x$skip_drop, x$min_child_weight, x$subsample, x$sample_type),]
                  })