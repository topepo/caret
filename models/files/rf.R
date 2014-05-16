modelInfo <- list(label = "Random Forest",
                  library = "randomForest",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = "mtry",
                                          class = "numeric",
                                          label = "#Randomly Selected Predictors"),
                  grid = function(x, y, len = NULL) {
                    p <- ncol(x)
                    if(len == 1) {  
                      tuneSeq <- if(!is.factor(y)) max(floor(p/3), 1) else floor(sqrt(p))
                    } else {
                      if(p <= len)
                      { 
                        tuneSeq <- floor(seq(2, to = p, length = p))
                      } else {
                        if(p < 500 ) tuneSeq <- floor(seq(2, to = p, length = len))
                        else tuneSeq <- floor(2^seq(1, to = log(p, base = 2), length = len))
                      }
                    }
                    if(any(table(tuneSeq) > 1))
                    {
                      tuneSeq <- unique(tuneSeq)
                      cat(
                        "note: only",
                        length(tuneSeq),
                        "unique complexity parameters in default grid.",
                        "Truncating the grid to",
                        length(tuneSeq), ".\n\n")      
                    }
                    data.frame(mtry = tuneSeq)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    randomForest(x, y, mtry = param$mtry, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "prob"),
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
                    
                    out <- as.data.frame(varImp)
                    if(dim(out)[2] == 2) {
                      tmp <- apply(out, 1, mean)
                      out[,1] <- out[,2] <- tmp  
                    }
                    out
                  },
                  levels = function(x) x$classes,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),])
