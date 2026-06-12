modelInfo <- list(label = "L2 Regularized Support Vector Machine (dual) with Linear Kernel",
                  library = "LiblineaR",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('cost', "Loss"),
                                          class = c("numeric", "character"),
                                          label = c("Cost", "Loss Function")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(cost = 2 ^((1:len) - 3),
                                         Loss = c("L1", "L2"))
                    } else {
                      out <- data.frame(cost = 2^runif(len, min = -10, max = 10),
                                        Loss = sample(c("L1", "L2"), size = len, replace = TRUE))
                    }
                    out
                  }, 
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    # LiblineaR lists the following models 
                    # 
                    #  2 - L2-regularized L2-loss support vector classification (primal) 
                    #  3 - L2-regularized L1-loss support vector classification (dual) 
                    # 12 - L2-regularized L2-loss support vector regression (dual)
                    # 13 - L2-regularized L1-loss support vector regression (dual) 
                    
                    if(param$Loss == "L2") {
                      model_type <- if(is.factor(y)) 2 else 12
                    } else model_type <- if(is.factor(y)) 3 else 13

                    out <- LiblineaR::LiblineaR(data = as.matrix(x), target = y,
                                                cost = param$cost,
                                                type = model_type,
                                                ...)

                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata)$predictions
                  },
                  prob = NULL,
                  predictors = function(x, ...) { 
                    out <- colnames(x$W)
                    out[out != "Bias"]
                    },
                  tags = c("Kernel Method", "Support Vector Machines","Linear Regression", "Linear Classifier",
                           "Robust Methods"),
                  levels = function(x) x$levels,
                  sort = function(x) {
                    x[order(x$cost),]
                  })
