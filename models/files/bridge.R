modelInfo <- list(label = "Bayesian Ridge Regression",
                  library = "monomvn",
                  type = "Regression",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    out <- monomvn::bridge(as.matrix(x), y, ...)
                    out
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- modelFit$beta %*% t(newdata)
                    if(modelFit$icept) out <- out + (matrix(1, ncol = ncol(out), nrow = nrow(out)) * modelFit$mu)
                    apply(out, 2, mean)        
                  },
                  predictors = function(x, s = NULL, ...) {
                    x$xNames
                  },
                  tags = c("Linear Regression", "Bayesian Model", "L2 Regularization"),
                  prob = NULL,
                  sort = function(x) x)
