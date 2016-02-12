modelInfo <- list(label = "Regularized Discriminant Analysis",
                  library = "klaR",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c("gamma", "lambda"),
                                          class = rep("numeric", 2),
                                          label = c("Gamma", "Lambda")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(gamma = seq(0, 1, length = len), 
                                         lambda =  seq(0, 1, length = len))
                    } else {
                      out <- data.frame(gamma = runif(len, min = 0, max = 1), 
                                        lambda = runif(len, min = 0, max = 1))
                    }
                    out
                  }, 
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    klaR:::rda(x, y, gamma = param$gamma, lambda = param$lambda, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    klaR:::predict.rda(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, submodels = NULL)
                    klaR:::predict.rda(modelFit, newdata)$posterior,
                  predictors = function(x, ...) x$varnames,
                  tags = c("Discriminant Analysis", "Polynomial Model", "Regularization",
                           "Linear Classifier"),
                  levels = function(x) names(x$prior),
                  sort = function(x) {
                    # since lds is less complex than qda, we
                    # sort on lambda (larger are least complex)
                    x[order(-x$lambda, x$gamma),] 
                  })
