modelInfo <- list(label = "Sparse Mixture Discriminant Analysis",
                  library = c("sparseLDA"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('NumVars', 'lambda', "R"),
                                          class = c("numeric", "numeric", "numeric"),
                                          label = c('# Predictors', 'Lambda', '# Subclasses')),
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
                                R = (1:len) + 1,
                                lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    smda(x, y,
                         Rj = param$R,
                         lambda = param$lambda,
                         stop = -param$NumVars,
                         ...),
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata)$class,
                  prob = NULL,
                  predictors = function(x, ...) x$varNames,
                  tags = c("Discriminant Analysis", "L1 Regularization", 
                           "Implicit Feature Selection", "Mixture Model"),
                  sort = function(x) x[order(x$NumVars, x$R, -x$lambda),])
