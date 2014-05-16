modelInfo <- list(label = "Random Forest with Additional Feature Selection", 
                  library = c("Boruta", "randomForest"),
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('mtry'),
                                          class = c("numeric"),
                                          label = c("#Randomly Selected Predictors")),
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
                    data.frame(mtry = tuneSeq)
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    fs <- Boruta(x, y, mtry = param$mtry, ...)
                    keepers <- as.character(names(fs$finalDecision)[fs$finalDecision == "Confirmed"])
                    out <- randomForest(x[,keepers, drop = FALSE], y, mtry = param$mtry, ...)
                    out$Boruta <- fs
                    out
                    },
                  predict = function(modelFit, newdata, submodels = NULL) predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL) predict(modelFit, newdata, type = "prob") ,
                  tags = c("Tree-Based Model", "Ensemble Model", "Feature Selection Wrapper", "Random Forest"),
                  levels = function(x) x$classes,
                  sort = function(x) x[order(x[,1]),])
