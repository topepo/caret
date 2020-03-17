modelInfo <- list(label = "Regularized Random Forest",
                  library = "RRF",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('mtry', 'coefReg'),
                                          class = c('numeric', 'numeric'),
                                          label = c('#Randomly Selected Predictors', 'Regularization Value')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(mtry = caret::var_seq(p = ncol(x), 
                                                               classification = is.factor(y), 
                                                               len = len),
                                         coefReg = seq(0.01, 1, length = len))
                    } else {
                      out <- data.frame(mtry = sample(1:ncol(x), size = len, replace = TRUE),
                                        coefReg = runif(len, min = 0, max = 1))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    RRF::RRF(x, y, mtry = param$mtry, coefReg = param$coefReg, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "prob"),
                  varImp = function(object, ...) {
                    varImp <- RRF::importance(object, ...)
                    if(object$type == "regression")
                      varImp <- data.frame(Overall = varImp[,"%IncMSE"])
                    else {
                      retainNames <- levels(object$y)
                      if(all(retainNames %in% colnames(varImp))) {
                        varImp <- varImp[, retainNames]
                      } else {
                        varImp <- data.frame(Overall = varImp[,1])
                      }
                    }  
                    out <- as.data.frame(varImp, stringsAsFactors = TRUE)
                    if(dim(out)[2] == 2) {
                      tmp <- apply(out, 1, mean)
                      out[,1] <- out[,2] <- tmp  
                    }
                    out
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection", "Regularization"),
                  sort = function(x) x[order(x$coefReg),])
