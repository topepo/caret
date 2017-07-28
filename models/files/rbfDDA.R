modelInfo <- list(label = "Radial Basis Function Network",
                  library = "RSNNS",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('negativeThreshold'),
                                          class = c('numeric'),
                                          label = c('Activation Limit for Conflicting Classes')),
                  grid = function(x, y, len = NULL, search = "grid") 
                  {
                    if(search == "grid") {
                      out <- data.frame(negativeThreshold =  10 ^(-(1:len)))
                    } else {
                      out <- data.frame(negativeThreshold = runif(len, min = 0, 3))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "learnFunc"))
                    {
                      theDots$learnFunc <- NULL
                      warning("Cannot over-ride 'learnFunc' argument for this model. RBF-DDA is used.")
                    }
                    if(any(names(theDots) == "learnFuncParams"))
                    {
                      theDots$learnFuncParams[2] <- param$negativeThreshold
                    } else theDots$learnFuncParams <-c(0.4,  param$negativeThreshold, 5)
                    
                    
                    y <- RSNNS:::decodeClassLabels(y)
                    args <- list(x = x,
                                 y = y)
                    args <- c(args, theDots)
                    do.call(RSNNS::rbfDDA, args)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)
                    if(modelFit$problemType == "Classification")
                    {
                      out <- modelFit$obsLevels[apply(out, 1, which.max)]
                    } else out <- out[,1]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)
                    colnames(out) <- modelFit$obsLevels
                    out
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Neural Network","L2 Regularization", "Radial Basis Function"),
                  sort = function(x) x[order(-x$negativeThreshold),])
