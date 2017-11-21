modelInfo <- list(label = "Robust Linear Discriminant Analysis",
                  library = "rrcov",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('parameter'),
                                          class = c('character'),
                                          label = c('none')),
                  grid = function(x, y, len = NULL, search = "grid") 
                    data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    rrcov:::Linda(x, y, ...) ,
                  predict = function(modelFit, newdata, submodels = NULL) 
                    rrcov:::predict(modelFit, newdata)@classification,
                  prob = function(modelFit, newdata, submodels = NULL) {
                    probs <- rrcov:::predict(modelFit, newdata)@posterior
                    colnames(probs) <- names(modelFit@prior)
                    probs
                  },
                  tags = c("Discriminant Analysis", "Linear Classifier", "Robust Model"),
                  levels = function(x) names(x@prior),
                  sort = function(x) x)
