modelInfo <- list(label = "Weighted Subspace Random Forest",
                  library = "wsrf",
                  loop = NULL,
                  type = c("Classification"),
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
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x)
                    dat$.outcome <- y
                    wsrf(.outcome ~ ., data = dat, mtry = param$mtry, ...)
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata)
                    },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata, type = "prob")
                    },
                  predictors = function(x, ...) x$xNames,
                  varImp = NULL,
                  levels = function(x) x$obsLevels,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),])
