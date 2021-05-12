modelInfo <- list(label = "Quantile Random Forest",
                  library = "quantregForest",
                  loop = NULL,
                  type = c("Regression"),
                  parameters = data.frame(parameter = "mtry",
                                          class = "numeric",
                                          label = "#Randomly Selected Predictors"),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
                                                              classification = is.factor(y), 
                                                              len = len))
                    } else {
                      out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, replace = TRUE)))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...)
                    quantregForest::quantregForest(x, y, mtry = min(param$mtry, ncol(x)), ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, what = 0.5)
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = NULL,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", 
                           "Implicit Feature Selection", "Quantile Regression",
                           "Robust Model"),
                  sort = function(x) x[order(x[,1]),])
