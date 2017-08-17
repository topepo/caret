modelInfo <- list(label = "Bayesian Ridge Regression (Model Averaged)",
                  library = "monomvn",
                  type = "Regression",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    out <- monomvn::blasso(as.matrix(x), y, ...)
                    out
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- modelFit$beta %*% t(newdata)
                    if(modelFit$icept) out <- out + (matrix(1, ncol = ncol(out), nrow = nrow(out)) * modelFit$mu)
                    apply(out, 2, mean)        
                  },
                  predictors = function(x, s = NULL, ...) {
                    x$xNames[apply(x$beta, 2, function(x) any(x != 0))]
                  },
                  notes = "This model makes predictions by averaging the predictions based on the posterior estimates of the regression coefficients. While it is possible that some of these posterior estimates are zero for non-informative predictors, the final predicted value may be a function of many (or even all) predictors. ",
                  tags = c("Linear Regression", "Bayesian Model", "L1 Regularization"),
                  prob = NULL,
                  sort = function(x) x)
