modelInfo <- list(label = "Random Forest by Randomization",
                  library = c("extraTrees"),
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('mtry', 'numRandomCuts'),
                                          class = c('numeric', 'numeric'),
                                          label = c('# Randomly Selected Predictors', '# Random Cuts')),
                  grid = function(x, y, len = NULL){
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
                    expand.grid(mtry = tuneSeq, numRandomCuts = 1:len)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    extraTrees(x, y, mtry = param$mtry, numRandomCuts = param$numRandomCuts, ...),
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata),
                  prob = NULL,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),])
