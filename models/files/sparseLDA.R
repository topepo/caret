modelInfo <- list(label = "Sparse Linear Discriminant Analysis",
                  library = c("sparseLDA"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('NumVars', 'lambda'),
                                          class = c("numeric", "numeric"),
                                          label = c('# Predictors', 'Lambda')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(NumVars = caret::var_seq(p = ncol(x), 
                                                                  classification = is.factor(y), 
                                                                  len = len),
                                         lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
                    } else {
                      out <- data.frame(lambda = 10^runif(len, min = -5, 1),
                                        NumVars = sample(1:ncol(x), size = len, replace = TRUE))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    sparseLDA:::sda(x, y, 
                                    lambda = param$lambda, 
                                    stop = -param$NumVars, 
                                    ...)
                  ,
                  predictors = function(x) x$xNames[x$varIndex],
                  predict = function(modelFit, newdata, submodels = NULL)
                    sparseLDA:::predict.sda(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, submodels = NULL)
                    sparseLDA:::predict.sda(modelFit, newdata)$posterior,
                  levels = function(x) x$obsLevels,
                  tags = c("Discriminant Analysis", "L1 Regularization", 
                           "Implicit Feature Selection", "Linear Classifier"),
                  sort = function(x) x[order(x$NumVars, -x$lambda),])
