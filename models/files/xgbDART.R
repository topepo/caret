#xgbDART
modelInfo <- list(label = "eXtreme Gradient Boosting",
                  library = c("xgboost", "plyr"),
                  check = function(pkg) {
                    requireNamespace("xgboost")
                    current <- packageDescription("xgboost")$Version
                    expected <- "0.6.4"
                    if(compareVersion(current, expected) < 0)
                      stop("This modeling workflow requires xgboost version ",
                           expected, " or greater. Consider using the drat repo.", call. = FALSE)
                  },
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c("nrounds",
                                                        "max_depth",
                                                        "eta",
                                                        "gamma",
                                                        "subsample",
                                                        "colsample_bytree",
                                                        "rate_drop",
                                                        "skip_drop",
                                                        "min_child_weight"),
                                          class = c(rep("numeric", 9)),
                                          label = c("# Boosting Iterations",
                                                    "Max Tree Depth",
                                                    "Shrinkage",
                                                    "Minimum Loss Reduction",
                                                    "Subsample Percentage",
                                                    "Subsample Ratio of Columns",
                                                    "Fraction of Trees Dropped",
                                                    "Prob. of Skipping Drop-out",
                                                    "Minimum Sum of Instance Weight")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(nrounds = floor((1:len) * 50),
                                         max_depth = seq(1, len),
                                         eta = c(0.3, 0.4), # "Usually" one should be the lowest possible
                                         gamma = 0,
                                         subsample = seq(.5, 1, length = len),
                                         colsample_bytree = c(0.6, 0.8),
                                         rate_drop = c(0.01, 0.50),
                                         skip_drop = c(0.05, 0.95),
                                         min_child_weight = c(1))
                    } else {
                      out <- data.frame(nrounds = sample(1:1000, size = len, replace = TRUE),
                                        max_depth = sample(1:10, replace = TRUE, size = len),
                                        eta = runif(len, min = 0.001, max = 0.6),
                                        gamma = runif(len, min = 0.0, max = 10.0),
                                        subsample = runif(len, min = 0.25, max = 1.00),
                                        colsample_bytree = runif(len, min = 0.30, max = 0.70),
                                        rate_drop = runif(len, min = 0.01, max = 0.50),
                                        skip_drop = runif(len, min = 0.05, max = 0.95),
                                        min_child_weight = sample(0:20, size = len, replace = TRUE))
                      out$nrounds <- floor(out$nrounds)
                    }
                    out
                  },
                  loop = function(grid) {
                    loop <- plyr::ddply(grid, c( "max_depth", "eta",            "rate_drop",
                                                 "skip_drop", "min_child_weight",
                                                 "subsample", "colsample_bytree", "gamma"),
                                        function(x) c(nrounds = max(x$nrounds)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$nrounds)) {
                      index <- which(grid$max_depth == loop$max_depth[i] &
                                       grid$eta == loop$eta[i] &
                                       grid$gamma == loop$gamma[i] &
                                       grid$subsample == loop$subsample[i]  &
                                       grid$colsample_bytree == loop$colsample_bytree[i]  &
                                       grid$rate_drop == loop$rate_drop[i] &
                                       grid$skip_drop == loop$skip_drop[i] &
                                       grid$min_child_weight == loop$min_child_weight[i] )
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
                            xgboost::setinfo(x, "label", y)

                        if (!is.null(wts))
                          xgboost::setinfo(x, 'weight', wts)

                        out <- xgboost::xgb.train(list(max_depth = param$max_depth,
                                                       eta = param$eta,
                                                       rate_drop = param$rate_drop,
                                                       skip_drop = param$skip_drop,
                                                       min_child_weight = param$min_child_weight,
                                                       gamma = param$gamma,
                                                       subsample = param$subsample,
                                                       colsample_bytree = param$colsample_bytree),
                                                  data = x,
                                                  nrounds = param$nrounds,
                                                  objective = "binary:logistic",
                                                  booster = 'dart',
                                                  ...)
                      } else {

                        y <- as.numeric(y) - 1

                        if(!inherits(x, "xgb.DMatrix"))
                          x <- xgboost::xgb.DMatrix(x, label = y, missing = NA) else
                            xgboost::setinfo(x, "label", y)

                        if (!is.null(wts))
                          xgboost::setinfo(x, 'weight', wts)

                        out <- xgboost::xgb.train(list(max_depth = param$max_depth,
                                                       eta = param$eta,
                                                       rate_drop = param$rate_drop,
                                                       skip_drop = param$skip_drop,
                                                       min_child_weight = param$min_child_weight,
                                                       gamma = param$gamma,
                                                       subsample = param$subsample,
                                                       colsample_bytree = param$colsample_bytree),
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
                          xgboost::setinfo(x, "label", y)

                      if (!is.null(wts))
                        xgboost::setinfo(x, 'weight', wts)

                      out <- xgboost::xgb.train(list(max_depth = param$max_depth,
                                                     eta = param$eta,
                                                     rate_drop = param$rate_drop,
                                                     skip_drop = param$skip_drop,
                                                     min_child_weight = param$min_child_weight,
                                                     gamma = param$gamma,
                                                     subsample = param$subsample,
                                                     colsample_bytree = param$colsample_bytree),
                                                data = x,
                                                nrounds = param$nrounds,
                                                objective = "reg:squarederror",
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
                    out <- predict(modelFit, newdata, ntreelimit = modelFit$niter)
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
                      p <- predict(modelFit, newdata, ntreelimit = modelFit$niter)
                      out <- binomial()$linkinv(p) # exp(p)/(1+exp(p))
                    } else {
                      out <- predict(modelFit, newdata, ntreelimit = modelFit$niter)
                    }
                    if(length(modelFit$obsLevels) == 2) {
                      out <- cbind(out, 1 - out)
                      colnames(out) <- modelFit$obsLevels
                    } else {
                      out <- matrix(out, ncol = length(modelFit$obsLevels), byrow = TRUE)
                      colnames(out) <- modelFit$obsLevels
                    }
                    out <- as.data.frame(out, stringsAsFactors = TRUE)

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
                        tmp_pred <- as.data.frame(tmp_pred, stringsAsFactors = TRUE)
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
                    imp <- as.data.frame(imp, stringsAsFactors = TRUE)[, 1:2]
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
                    x[order(x$nrounds,          x$max_depth, x$eta,              x$rate_drop, x$skip_drop,
                            x$min_child_weight, x$subsample, x$colsample_bytree, x$gamma),]
                  })
