modelInfo <- list(label = "Radial Basis Function Kernel Regularized Least Squares",
                  library = c("KRLS", "kernlab"),
                  loop = NULL,
                  type = c('Regression'),
                  parameters = data.frame(parameter = c('lambda', 'sigma'),
                                          class = c('numeric', 'numeric'),
                                          label = c('Regularization Parameter', 'Sigma')),
                  grid = function(x, y, len = NULL) {
                    ## this was changed to follow what kernlab does inside of ksvm and rvm:
                    sigmaEstimate <- try(sigest(x, na.action = na.omit, scaled = TRUE),
                                         silent = TRUE)
                    if(!(class(sigmaEstimate) == "try-error"))
                    {
                      out <- seq(sigmaEstimate[1], sigmaEstimate[3], length = len)
                    } else out <- 10 ^((1:len) - 3)
                    
                    expand.grid(lambda = NA, sigma = 1/out)
                  }
                  ,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    krls(x, y, lambda = if(is.na(param$lambda)) NULL else param$lambda,
                         sigma = param$sigma, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata)$fit[,1]
                  },
                  tags = c("Kernel Method", "L2 Regularization", "Radial Basis Function"),
                  prob = NULL,
                  sort = function(x) x[order(x$lambda),])
