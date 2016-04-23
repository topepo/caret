modelInfo <- list(label = "Diagonal Discriminant Analysis",
                  library = "sparsediscrim",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c("model", "shrinkage"),
                                          class = rep("character", 2),
                                          label = c("Model", "Shrinkage Type")),
                  grid = function(x, y, len = NULL, search = "grid") 
                    data.frame(model = rep(c("Linear", "Quadratic"), each = 3),
                               shrinkage = rep(c("None", "Variance", "Mean"), 2)),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(param$model == "Linear") {
                      if(param$shrinkage == "None") {
                        out <- dlda(x, y, ...)  
                      } else {
                        if(param$shrinkage == "Variance") {
                          out <- sdlda(x, y, ...)  
                        } else out <- smdlda(x, y, ...)  
                      }
                    } else {
                      if(param$shrinkage == "None") {
                        out <- dqda(x, y, ...)  
                      } else {
                        if(param$shrinkage == "Variance") {
                          out <- sdqda(x, y, ...)  
                        } else out <- smdqda(x, y, ...)  
                      }
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)$scores
                    as.data.frame(t(apply(out, 2, function(x) exp(x)/sum(exp(x)))))
                    },
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else colnames(x$means),
                  tags = c("Discriminant Analysis", "Linear Classifier", "Polynomial Model", "Regularization"),
                  levels = function(x) names(x$prior),
                  sort = function(x) x)
