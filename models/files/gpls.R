modelInfo <- list(label = "Generalized Partial Least Squares",
                  library = "gpls",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('K.prov'),
                                          class = c('numeric'),
                                          label = c('#Components')),
                  grid = function(x, y, len = NULL) data.frame(K.prov =seq(1, len)),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    gpls(x, y, K.prov = param$K.prov, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)$predicted
                    out <- cbind(out, 1-out)
                    colnames(out) <-  modelFit$obsLevels
                    out
                  },
                  predictors = function(x, ...) {
                    out <- if(hasTerms(x)) predictors(x$terms) else colnames(x$data$x.order)
                    out[!(out %in% "Intercept")]
                  },
                  tags = c("Logistic Regression", "Partial Least Squares", "Linear Classifier"),
                  sort = function(x) x[order(x[,1]),],
                  levels = function(x) x$obsLevels)
