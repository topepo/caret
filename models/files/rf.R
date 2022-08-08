modelInfo <- list(label = "Random Forest",
                  library = "randomForest",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("mtry", "nodesize"),
                                          class = c("numeric", "numeric"),
                                          label = c("#Randomly Selected Predictors",
                                                    "Minimum Node Size")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(mtry = caret::var_seq(p = ncol(x),
                                                              classification = is.factor(y),
                                                              len = len),
                                         nodesize = ifelse( is.factor(y), 1, 5)
                      )
                    } else {
                      out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, replace = TRUE)),
                                        nodesize = sample(1:(min(20,nrow(x))), size = len, replace = TRUE)
                      )
                    }
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...)
                    randomForest::randomForest(x, y,
                                               mtry = min(param$mtry, ncol(x)),
                                               nodesize = param$nodesize,
                                               ...),
                  predict = function(modelFit, newdata, submodels = NULL)
                    if(!is.null(newdata)) predict(modelFit, newdata) else predict(modelFit),
                  prob = function(modelFit, newdata, submodels = NULL)
                    if(!is.null(newdata)) predict(modelFit, newdata, type = "prob") else predict(modelFit, type = "prob"),
                  predictors = function(x, ...) {
                    ## After doing some testing, it looks like randomForest
                    ## will only try to split on plain main effects (instead
                    ## of interactions or terms like I(x^2).
                    varIndex <- as.numeric(names(table(x$forest$bestvar)))
                    varIndex <- varIndex[varIndex > 0]
                    varsUsed <- names(x$forest$ncat)[varIndex]
                    varsUsed
                  },
                  varImp = function(object, ...){
                    varImp <- randomForest::importance(object, ...)
                    if(object$type == "regression") {
                      if("%IncMSE" %in% colnames(varImp)) {
                        varImp <- as.data.frame(varImp[,"%IncMSE", drop = FALSE])
                        colnames(varImp) <- "Overall"
                      } else {
                        varImp <- as.data.frame(varImp[,1, drop = FALSE])
                        colnames(varImp) <- "Overall"
                      }
                    }
                    else {
                      retainNames <- levels(object$y)
                      if(all(retainNames %in% colnames(varImp))) {
                        varImp <- varImp[, retainNames, drop = FALSE]
                      } else {
                        varImp <- as.data.frame(varImp[,1, drop = FALSE])
                        colnames(varImp) <- "Overall"
                      }
                    }

                    out <- as.data.frame(varImp, stringsAsFactors = TRUE)
                    if(dim(out)[2] == 2) {
                      tmp <- apply(out, 1, mean)
                      out[,1] <- out[,2] <- tmp
                    }
                    out
                  },
                  levels = function(x) x$classes,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),],
                  oob = function(x) {
                    out <- switch(x$type,
                                  regression =   c(sqrt(max(x$mse[length(x$mse)], 0)), x$rsq[length(x$rsq)]),
                                  classification =  c(1 - x$err.rate[x$ntree, "OOB"],
                                                      e1071::classAgreement(x$confusion[,-dim(x$confusion)[2]])[["kappa"]]))
                    names(out) <- if(x$type == "regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
                    out
                  })
