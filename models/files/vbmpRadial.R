modelInfo <- list(label = "Variational Bayesian Multinomial Probit Regression",
                  library = "vbmp",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('estimateTheta'),
                                          class = c('character'),
                                          label = c('Theta Estimated')),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(estimateTheta = "yes"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    theDots <- list(...)
                    if(any(names(theDots) == "control"))
                    {
                      theDots$control$bThetaEstimate <- ifelse(param$estimateTheta == "Yes", TRUE, FALSE)
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- list(bThetaEstimate = ifelse(param$estimateTheta == "Yes", TRUE, FALSE))                        
                    if(any(names(theDots) == "theta"))
                    {
                      theta <- theDots$theta
                      theDots$theta <- NULL
                    } else theta <- runif(ncol(x))

                    vbmp::vbmp(x, as.numeric(y),
                               theta = theta,
                               control = ctl,
                               X.TEST = x[1,],
                               t.class.TEST  = as.numeric(y)[1])
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    probs <- vbmp::predictCPP(modelFit, newdata)
                    modelFit$obsLevels[apply(probs, 1, which.max)]
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    probs <- vbmp::predictCPP(modelFit, newdata)
                    colnames(probs) <- modelFit$obsLevels
                    probs
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Gaussian Process", "Bayesian Model", "Radial Basis Function"),
                  sort = function(x) x)
