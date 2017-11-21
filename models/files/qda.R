modelInfo <- list(label = "Quadratic Discriminant Analysis",
                  library = "MASS",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) MASS::qda(x, y, ...)  ,
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)$posterior,
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else colnames(x$means),
                  tags = c("Discriminant Analysis", "Polynomial Model"),
                  levels = function(x) names(x$prior),
                  sort = function(x) x)
