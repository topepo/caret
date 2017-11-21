modelInfo <- list(label = "Maximum Uncertainty Linear Discriminant Analysis",
                  library = "HiDimDA",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('parameter'),
                                          class = c("character"),
                                          label = c('parameter')),
                  grid = function(x, y, len = NULL, search = "grid") 
                    data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    HiDimDA::Mlda(x, y, q = param$.q, maxq = param$.q, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)$class
                    out <- modelFit$obsLevels[as.numeric(out)]
                    out
                  },
                  levels = function(x) x$obsLevels,
                  prob = NULL,
                  tags = c("Discriminant Analysis", "Linear Classifier"),
                  sort = function(x) x[order(x[,1]),])
