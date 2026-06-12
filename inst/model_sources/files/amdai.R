modelInfo <- list(label = "Adaptive Mixture Discriminant Analysis",
                  library = "adaptDA",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = "model",
                                          class = "character",
                                          label = "Model Type"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(model = "lda"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    mod <- adaptDA::amdai(x, as.numeric(y), 
                                 model = as.character(param$model), ...)
                    mod$levels <- levels(y)
                    mod
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, K = length(modelFit$levels))$cls
                    factor(modelFit$levels[out], levels = modelFit$levels)
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata, K = length(modelFit$levels))$P
                    factor(modelFit$levels[out], levels = modelFit$levels)
                    colnames(out)<-  modelFit$obsLevels
                    out
                  },
                  varImp = NULL,
                  predictors = function(x, ...) predictors(x$terms),
                  levels = function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL,
                  tags = c("Discriminant Analysis", "Mixture Model"),
                  sort = function(x) x)
