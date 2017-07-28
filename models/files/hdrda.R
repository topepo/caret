modelInfo <- list(label = "High-Dimensional Regularized Discriminant Analysis",
                  library = "sparsediscrim",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c("gamma", "lambda", "shrinkage_type"),
                                          class = c(rep("numeric", 2), "character"),
                                          label = c("Gamma", "Lambda", "Shrinkage Type")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(gamma = seq(0, 1, length = len), 
                                         lambda =  seq(0, 1, length = len),
                                         shrinkage_type = c("ridge", "convex"))
                    } else {
                      out <- data.frame(gamma = runif(len, min = 0, max = 1), 
                                        lambda = runif(len, min = 0, max = 1),
                                        shrinkage_type = sample(c("ridge", "convex"), size = len, replace = TRUE))
                    }
                    out
                  }, 
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    sparsediscrim::hdrda(x, y, gamma = param$gamma, lambda = param$lambda,
                          shrinkage_type = as.character(param$shrinkage_type),
                          ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata)$posterior,
                  predictors = function(x, ...) x$varnames,
                  tags = c("Discriminant Analysis", "Polynomial Model", "Regularization",
                           "Linear Classifier"),
                  levels = function(x) names(x$prior),
                  sort = function(x) {
                    # since lds is less complex than qda, we
                    # sort on lambda (larger are least complex)
                    x[order(-x$lambda, x$gamma),] 
                  })
