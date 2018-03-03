modelInfo <- list(label = "Random Forest with Additional Feature Selection", 
                  library = c("Boruta", "randomForest"),
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('mtry'),
                                          class = c("numeric"),
                                          label = c("#Randomly Selected Predictors")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
                                                              classification = is.factor(y), 
                                                              len = len))
                    } else {
                      out <- data.frame(mtry = unique(sample(1:ncol(x), replace = TRUE, size = len)))
                    }
                    out
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
