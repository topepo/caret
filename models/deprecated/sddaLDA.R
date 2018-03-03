modelInfo <- list(label = "Stepwise Diagonal Linear Discriminant Analysis",
                  library = c("SDDA"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c("parameter"),
                                          class = c("character"),
                                          label = c('parameter')),
                  grid = function(x, y, len = NULL, search = "grid") 
                    data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...)
                    SDDA::sdda(as.matrix(x), y, method = "lda", ...),
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, as.matrix(newdata), type = "class"),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, as.matrix(newdata), type = "prob"),
                  levels = function(x) x$obsLevels,
                  tags = c("Discriminant Analysis", "Feature Selection Wrapper", "Linear Classifier"),
                  sort = function(x) x)
