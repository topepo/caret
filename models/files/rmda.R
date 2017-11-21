modelInfo <- list(label = "Robust Mixture Discriminant Analysis",
                  library = "robustDA",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c("K", "model"),
                                          class = c("numeric", "character"),
                                          label = c('#Subclasses Per Class', 'Model')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    mods <- c("EII", "VII", "EEI", "EVI", "VEI", "VVI")
                    if(search == "grid") {
                      out <- expand.grid(K = (1:len) + 1,  model = c("VEV"))
                    } else {
                      out <- data.frame(K = sample(2:10, size = len, replace = TRUE), 
                                        model = sample(mods, size = len, replace = TRUE))
                    }
                    out
                  }, 
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    mod <- robustDA::rmda(x, as.numeric(y),
                                          K = param$K,
                                          model = as.character(param$model),
                                          ...)
                    mod$levels <- levels(y)
                    mod
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)$cls
                    factor(modelFit$levels[out], levels = modelFit$levels)
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata)$P
                    colnames(out)<-  modelFit$obsLevels
                    out
                  },
                  varImp = NULL,
                  predictors = function(x, ...) colnames(x$prms$data),
                  levels = function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL,
                  tags = c("Discriminant Analysis", "Mixture Model", "Robust Methods"),
                  sort = function(x) x)
