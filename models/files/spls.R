modelInfo <- list(label = "Sparse Partial Least Squares",
                  library = "spls",
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('K', 'eta', 'kappa'),
                                          class = c('numeric', 'numeric', 'numeric'),
                                          label = c('#Components', 'Threshold', 'Kappa')),
                  grid = function(x, y, len = NULL) {
                    expand.grid(K = 1:len, 
                                eta = seq(.1, .9, length = len), 
                                kappa = .5)
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(is.factor(y))
                    {
                      caret:::splsda(x, y, K = param$K, eta = param$eta,
                                     kappa = param$kappa, ...)
                    } else {
                      spls(x, y, K = param$K, eta = param$eta,
                           kappa = param$kappa, ...)
                    }          
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(length(modelFit$obsLevels) < 2)
                    {
                      predict(modelFit, newdata)
                    } else {
                      as.character(caret:::predict.splsda(modelFit, newdata, type = "class"))
                    }
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    caret:::predict.splsda(modelFit, newdata, type = "prob")
                  },
                  predictors = function(x, ...) colnames(x$x)[x$A],
                  tags = c("Partial Least Squares", "Feature Extraction", "Linear Classifier", "Linear Regression",
                           "L1 Regularization"),
                  levels = NULL,
                  sort = function(x) x[order(-x$eta, x$K),])
