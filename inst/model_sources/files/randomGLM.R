modelInfo <- list(label = "Ensembles of Generalized Linear Models",
                  library = "randomGLM",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = 'maxInteractionOrder',
                                          class = "numeric",
                                          label = 'Interaction Order'),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <-  expand.grid(maxInteractionOrder = 1:min(len, 3))
                    } else {
                      out <- data.frame(maxInteractionOrder = sample(1:3, length = len))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    require(randomGLM)
                    if(!is.matrix(x)) x <- as.matrix(x)
                    mod <- randomGLM::randomGLM(
                      x = x, 
                      y, 
                      maxInteractionOrder = param$maxInteractionOrder, 
                      ...
                    )
                    mod
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- predict(modelFit, newdata)    
                    if(modelFit$problemType == "Classification") 
                      out <- modelFit$obsLevel[apply(out, 1, which.max)]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    predict(modelFit, newdata)    
                  },                  
                  predictors = function(x, s = NULL, ...) {
                    all_pred <- lapply(x$models, function(x) names(coef(x)))
                    all_pred <- unique(unlist(all_pred))
                    all_pred <- strsplit(all_pred, ".times.", fixed = TRUE)
                    all_pred <- unique(unlist(all_pred))
                    all_pred[all_pred != "(Intercept)"]
                  },
                  tags = c("Generalized Linear Model", "Linear Classifier",
                           "Ensemble Model", "Bagging"),
                  notes = paste(
                    "Unlike other packages used by `train`, the `randomGLM`",
                    "package is fully loaded when this model is used."
                  ),                  
                  prob = NULL,
                  sort = function(x) x)
