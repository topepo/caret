modelInfo <- list(label = "ROC-Based Classifier",
                  library = "rocc",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('xgenes'),
                                          class = c('numeric'),
                                          label = c('#Variables Retained')),
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
                    data.frame(xgenes = tuneSeq)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    newY <- factor(ifelse(y == levels(y)[1], 1, 0), levels = c("0", "1"))
                    tr.rocc(g = t(as.matrix(x)), out = newY, xgenes = param$xgenes)
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    tmp <- p.rocc(modelFit, t(as.matrix(newdata)))
                    factor(ifelse(tmp == "1",  modelFit$obsLevels[1],  modelFit$obsLevels[2]),
                           levels =  modelFit$obsLevels)
                  },
                  prob = NULL,
                  predictors = function(x, ...) x$genes,
                  tags = "ROC Curves",
                  sort = function(x) x[order(x$xgenes),])
