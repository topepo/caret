modelInfo <- list(label = "Heteroscedastic Discriminant Analysis",
                  library = "hda",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('gamma', 'lambda', 'newdim'),
                                          class = c('numeric', 'numeric', 'numeric'),
                                          label = c('Gamma', 'Lambda', 'Dimension of the Discriminative Subspace')),
                  grid = function(x, y, len = NULL) 
                    expand.grid(gamma = seq(0.1, 1, length = len), 
                                lambda =  seq(0, 1, length = len),
                                newdim = 2:(min(len, ncol(data)))),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    hda(x, y,
                        newdim = param$newdim,
                        reg.lamb = param$lambda,
                        reg.gamm = param$gamma,
                        crule = TRUE, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    tmp <- predict(modelFit, as.matrix(newdata))
                    if(is.vector(tmp)) tmp <- matrix(tmp, ncol = 1)
                    as.character(predict(modelFit$naivebayes, tmp))
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {        
                    tmp <- predict(modelFit, as.matrix(newdata))
                    if(is.vector(tmp)) tmp <- matrix(tmp, ncol = 1)
                    as.data.frame(predict(modelFit$naivebayes, tmp, type = "raw"))
                  },
                  tags = c("Discriminant Analysis", "Linear Classifier", "Regularization"),
                  sort = function(x) x[order(x$newdim, -x$lambda, x$gamma),])
