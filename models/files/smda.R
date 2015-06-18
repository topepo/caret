modelInfo <- list(label = "Sparse Mixture Discriminant Analysis",
                  library = c("sparseLDA"),
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('NumVars', 'lambda', "R"),
                                          class = c("numeric", "numeric", "numeric"),
                                          label = c('# Predictors', 'Lambda', '# Subclasses')),
                  grid = function(x, y, len = NULL){
                    expand.grid(NumVars = caret::var_seq(p = ncol(x), 
                                                  classification = is.factor(y), 
                                                  len = len),
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
                  levels = function(x) x$obsLevels,
                  predictors = function(x, ...) x$varNames,
                  tags = c("Discriminant Analysis", "L1 Regularization", 
                           "Implicit Feature Selection", "Mixture Model"),
                  sort = function(x) x[order(x$NumVars, x$R, -x$lambda),])
