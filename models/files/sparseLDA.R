modelInfo <- list(label = "Sparse Linear Discriminant Analysis",
                  library = c("sparseLDA"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('NumVars', 'lambda'),
                                          class = c("numeric", "numeric"),
                                          label = c('# Predictors', 'Lambda')),
                  grid = function(x, y, len = NULL){
                    p <- ncol(x) 
                    if(p <= len)
                    { 
                      tuneSeq <- floor(seq(2, to = p, length = p))
                    } else {
                      if(p < 500 ) tuneSeq <- floor(seq(2, to = p, length = len))
                      else tuneSeq <- floor(2^seq(1, to = log(p, base = 2), length = len))
                    }
                    expand.grid(NumVars = tuneSeq,
                                lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
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
                  tags = c("Discriminant Analysis", "L1 Regularization", 
                           "Implicit Feature Selection", "Linear Classifier"),
                  sort = function(x) x[order(x$NumVars, -x$lambda),])
