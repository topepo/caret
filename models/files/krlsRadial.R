modelInfo <- list(label = "Radial Basis Function Kernel Regularized Least Squares",
                  library = c("KRLS", "kernlab"),
                  loop = NULL,
                  type = c('Regression'),
                  parameters = data.frame(parameter = c('lambda', 'sigma'),
                                          class = c('numeric', 'numeric'),
                                          label = c('Regularization Parameter', 'Sigma')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    ## this was changed to follow what kernlab does inside of ksvm and rvm:
                    sigmaEstimate <- try(kernlab::sigest(x, na.action = na.omit, scaled = TRUE),
                                         silent = TRUE)
                    if(!(class(sigmaEstimate) == "try-error")) {
                      if(search == "grid") {
                        out <- expand.grid(lambda = NA, 
                                           sigma = 1/seq(sigmaEstimate[1], sigmaEstimate[3], length = len))
                      } else {
                        rng <- extendrange(log(sigmaEstimate), f = .75)
                        out <- data.frame(lambda = 10^runif(len, min = -5, 0),
                                          sigma = 1/exp(runif(len, min = rng[1], max = rng[2])))
                      }
                      
                    } else {
                      if(search == "grid") {
                        out <- expand.grid(lambda = NA, 
                                           sigma = 1/(10 ^((1:len) - 3))) 
                      } else {
                        out <- data.frame(lambda = 10^runif(len, min = -5, 0),
                                          sigma = 1/(10^runif(len, min = -4, max = 0)))
                      }
                    }
                    out
                  }
                  ,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    KRLS::krls(x, y, lambda = if(is.na(param$lambda)) NULL else param$lambda,
                               sigma = param$sigma, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    KRLS:::predict.krls(modelFit, newdata)$fit[,1]
                  },
                  tags = c("Kernel Method", "L2 Regularization", "Radial Basis Function"),
                  prob = NULL,
                  sort = function(x) x[order(x$lambda),])
