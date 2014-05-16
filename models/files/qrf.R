modelInfo <- list(label = "Quantile Random Forest",
                  library = "quantregForest",
                  loop = NULL,
                  type = c("Regression"),
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
                    quantregForest(x, y, mtry = param$mtry, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, quantiles = .5)
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = NULL,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", 
                           "Implicit Feature Selection", "Quantile Regression",
                           "Robust Model"),
                  sort = function(x) x[order(x[,1]),])
