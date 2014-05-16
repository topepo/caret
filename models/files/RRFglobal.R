modelInfo <- list(label = "Regularized Random Forest",
                  library = "RRF",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('mtry', 'coefReg'),
                                          class = c('numeric', 'numeric'),
                                          label = c('#Randomly Selected Predictors', 'Regularization Value')),
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
                    expand.grid(mtry = tuneSeq,
                                coefReg = seq(0.01, 1, length = len))
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    RRF(x, y, mtry = param$mtry, coefReg = param$coefReg, ...)
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
                    out <- as.data.frame(varImp)
                    if(dim(out)[2] == 2) {
                      tmp <- apply(out, 1, mean)
                      out[,1] <- out[,2] <- tmp  
                    }
                    out
                  },
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection", "Regularization"),
                  sort = function(x) x[order(x$coefReg),])
