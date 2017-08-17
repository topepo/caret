modelInfo <- list(label = "Radial Basis Function Network",
                  library = "RSNNS",
                  loop = NULL,
                  type = c('Classification','Regression'),
                  parameters = data.frame(parameter = c('size'),
                                          class = c('numeric'),
                                          label = c('#Hidden Units')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(size =  ((1:len) * 2) - 1)
                    } else {
                      out <- data.frame(size = unique(sample(1:20, size = len, replace = TRUE)))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    theDots <- list(...)
                    theDots <- theDots[!(names(theDots) %in% c("size", "linOut"))]
                    if(any(names(theDots) == "learnFunc"))
                    {
                      theDots$learnFunc <- NULL 
                      warning("Cannot over-ride 'learnFunc' argument for this model. RadialBasisLearning is used.")
                    }
                    if(!any(names(theDots) == "initFuncParams"))
                    {
                      theDots$initFuncParams <- c(0, 1, 0, 0.02, 0.04)
                      if(is.factor(y)) theDots$initFuncParams[1:2] <- c(-4, 4)
                    }
                    
                    if(!any(names(theDots) == "learnFuncParams"))
                    {
                      theDots$learnFuncParams <- c(1e-8, 0, 1e-8, 0.1, 0.8)
                    }
                
                    if(is.factor(y)) {
                      y <- RSNNS:::decodeClassLabels(y)
                      lin <- FALSE
                    } else lin <- TRUE
                    args <- list(x = x,
                                 y = y,                           
                                 size = param$size,
                                 linOut = lin)
                    args <- c(args, theDots)
                    do.call(RSNNS::rbf, args)
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
                  sort = function(x) x[order(x$size),])
